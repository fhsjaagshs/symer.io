{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Blog.Auth
(
  getUser,
  setUser,
  authenticate,
  deleteAuth,
  accessToken
)
where
  
import Blog.State
import Blog.Cookie
import Blog.User

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Web.Scotty.Trans as Scotty

import Data.Default
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random

accessToken :: (ScottyError e) => ActionT e WebM (Maybe String)
accessToken = f <$> Scotty.header "Cookie"
  where
    f = maybe Nothing (g . parseCookie . BL.toStrict . TL.encodeUtf8)
    g = maybe Nothing (H.lookup "token" . cookiePairs)
    
getUser :: (ScottyError e) => ActionT e WebM (Maybe User)
getUser = do
  redis <- webM $ gets stateRedis
  f <$> accessToken
  -- mtoken <- accessToken
  -- case mtoken of
  --   Nothing -> return Nothing
  --   Just token -> liftIO $ do
  --     R.runRedis redis $ do
  --       R.expire (B.pack token) (3600*24)
  --       t <- R.get $ B.pack token
  --       case t of
  --         Right (Just j) -> return $ A.decodeStrict j
  --         _ -> return Nothing
  where
    f = maybe Nothing g
    g token = liftIO $ do
      R.runRedis redis $ do
        R.expire (B.pack token) (3600*24)
        t <- R.get $ B.pack token
        case t of
          Right (Just j) -> return $ A.decodeStrict j
          _ -> return Nothing

setUser :: (ScottyError e) => User -> ActionT e WebM ()
setUser user = do
  token <- (take 15 . randomRs ('a','z')) <$> (liftIO $ newStdGen)
  redis <- webM $ gets stateRedis
  saveUser redis token user
  setTokenCookie token
  where
    cookie token = def { cookiePairs = (H.singleton "token" token) }
    saveUser redis token = liftIO . R.runRedis redis . R.set (B.pack token) . BL.toStrict . A.encode
    setTokenCookie = mapM_ (addHeader "Set-Cookie" . TL.pack) . serializeCookie . cookie
    
deleteAuth :: (ScottyError e) => ActionT e WebM ()
deleteAuth = do
  redis <- webM $ gets stateRedis
  mtoken <- accessToken
  case mtoken of
    Just token -> liftIO $ void $ R.runRedis redis $ R.del [(B.pack token)]
    _ -> return ()

authenticate :: (ScottyError e) => ActionT e WebM (Maybe User)
authenticate = do
  maybeUser <- getUser
  case maybeUser of
    (Just user) -> return $ Just user
    _ -> do
      return Nothing
      redirect "/login?error_message=Login%20required%2E"