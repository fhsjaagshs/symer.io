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
import Blog.Web.Gzip

import Data.Maybe
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.STM

import Web.Scotty.Trans as Scotty

import Network.Wai (responseLBS,requestHeaderHost,rawPathInfo,rawQueryString,Application)
import Network.Wai.Handler.Warp (defaultSettings,setPort,setBeforeMainLoop,setInstallShutdownHandler,runSettings,Settings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,runTLS)
import Network.HTTP.Types.Status (status301)
import Network.Wai.Middleware.AddHeaders

import System.Exit
import System.Posix

-- app -> your Scotty app
-- port -> port to run the HTTPS server on
-- state -> the application state
startHTTP :: (ScottyError e) => ScottyT e WebM () -> IO AppState -> Int -> IO ()
startHTTP app mkstate port = mkstate >>= runApp (applyMiddleware False app)
  where
    runApp a st = mkScottyAppT a st >>= runSettings (mkWarpSettings port st)

-- app -> your Scotty app
-- mkstate -> IO action to create your app's initial state (post-fork to avoid DB connection issues)
-- cert -> SSL certificate file path
-- key -> SSL private key file path
-- port -> port to run the HTTPS server on
-- state -> the application state
startHTTPS :: (ScottyError e) => ScottyT e WebM () -> IO AppState -> FilePath -> FilePath -> Int -> IO ()
startHTTPS app mkstate cert key port = do
  privileged <- isPrivileged
  when privileged startRedirectProcess
  mkstate >>= runApp (applyMiddleware True app)
  where
    mksettings = defaultTlsSettings { keyFile = key, certFile = cert }
    runApp a st = mkScottyAppT a st >>= runTLS mksettings (mkWarpSettings port st)

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
applyMiddleware :: (ScottyError e) => Bool -> ScottyT e WebM () -> ScottyT e WebM ()
applyMiddleware ssl app = do
  middleware $ gzip 860 -- min length to GZIP
  when ssl $ middleware $ addHeaders [("Strict-Transport-Security","max-age=31536000")]
  app
    
mkScottyAppT :: (ScottyError e) => ScottyT e WebM () -> AppState -> IO Application
mkScottyAppT app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO app
    
resignPrivileges :: String -> IO ()
resignPrivileges user = do
  privileged <- isPrivileged
  when privileged $ do
    getUserEntryForName user >>= setUserID . userID

isPrivileged :: IO Bool
isPrivileged = ((==) 0) <$> getEffectiveUserID

startRedirectProcess :: IO ()
startRedirectProcess = void $ do
  putStrLn "starting HTTP -> HTTPS process"
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
      putStrLn "killing HTTP -> HTTPS process"
      signalProcess sigTERM pid
      exitImmediately ExitSuccess