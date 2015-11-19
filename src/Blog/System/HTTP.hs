{-# LANGUAGE OverloadedStrings #-}
module Blog.System.HTTP
(
  startHTTPS,
  startHTTP
)
where

import Blog.State
import Blog.System.IO
import Blog.System.FileCache

import Data.Maybe
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.STM

import Web.Scotty.Trans as Scotty

import Network.Wai (responseLBS,requestHeaderHost,rawPathInfo,rawQueryString,Application)
import Network.Wai.Handler.Warp (defaultSettings,setPort,setBeforeMainLoop,setInstallShutdownHandler,runSettings,Settings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,runTLS)
import Network.HTTP.Types.Status (status301)
import Network.Wai.Middleware.Gzip

import System.Exit
import System.Posix
  
{-
TODO (internals)
* investigate:
  add_header Strict-Transport-Security "max-age=31536000; includeSubdomains";
-}

-- app -> your Scotty app
-- port -> port to run the HTTPS server on
-- state -> the application state
startHTTP :: (ScottyError e) => ScottyT e WebM () -> Int -> AppState -> IO ()
startHTTP app port state = mkScottyAppT app state >>= runSettings (mkWarpSettings port state)

-- app -> your Scotty app
-- preredirect -> called before the HTTP redirection process is spawned
-- onkill -> called before the HTTP redirection process is killed
-- cert -> SSL certificate file path
-- key -> SSL private key file path
-- port -> port to run the HTTPS server on
-- state -> the application state
startHTTPS :: (ScottyError e) => ScottyT e WebM () -> IO () -> IO () -> FilePath -> FilePath -> Int -> AppState -> IO ()
startHTTPS app preredirect onkill cert key port state = do
  -- privileged <- isPrivileged
  -- when privileged $ startRedirectProcess preredirect onkill
  mkScottyAppT app state >>= run
  where
    run = runTLS tlsSettings $ mkWarpSettings port state
    tlsSettings = defaultTlsSettings { keyFile = key, certFile = cert }
    
{- Internal -}
  
mkWarpSettings :: Int -> AppState -> Settings
mkWarpSettings port state = setBeforeMainLoop before
                            $ setInstallShutdownHandler shutdown
                            $ setPort port
                            defaultSettings
  where
    before = resignPrivileges "daemon"
    shutdown act = do
      act
      teardownFileCache $ stateCache state
    
-- Adds middleware to a scotty app
applyMiddleware :: (ScottyError e) => ScottyT e WebM () -> ScottyT e WebM ()
applyMiddleware app = do
  middleware $ gzip def
  app
    
mkScottyAppT :: (ScottyError e) => ScottyT e WebM () -> AppState -> IO Application
mkScottyAppT app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO $ applyMiddleware app
    
resignPrivileges :: String -> IO ()
resignPrivileges user = do
  privileged <- isPrivileged
  when privileged $ do
    getUserEntryForName user >>= setUserID . userID

isPrivileged :: IO Bool
isPrivileged = ((==) 0) <$> getEffectiveUserID

startRedirectProcess :: IO () -> IO () -> IO ()
startRedirectProcess prefork prekill = void $ do
  prefork
  pid <- forkProcess $ do
    redirectStdout $ Just "/dev/null"
    redirectStderr $ Just "/dev/null"
    redirectStdin $ Just "/dev/null"
    installHandler sigTERM (Catch childHandler) Nothing
    runSettings warpSettings $ \req respond -> do
      respond $ responseLBS status301 (mkHeaders req) ""
  installHandler sigTERM (Catch $ parentHandler pid) Nothing
  installHandler sigINT (Catch $ parentHandler pid) Nothing
  where
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort 80 defaultSettings
    mkHeaders r = [("Location", url r)]
    host = fromJust . requestHeaderHost
    url r = mconcat ["https://", host r, rawPathInfo r, rawQueryString r]
    childHandler = exitImmediately ExitSuccess
    parentHandler pid = do
      prekill
      signalProcess sigTERM pid
      exitImmediately ExitSuccess