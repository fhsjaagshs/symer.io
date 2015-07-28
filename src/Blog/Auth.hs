{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
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
import Blog.Types

import qualified Data.Text.Lazy as TL

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Web.Scotty.Trans as Scotty

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random

-- TODO: Fixme
accessToken :: (ScottyError e, Monad m) => ActionT e m (Maybe String)
accessToken = do
  mv <- Scotty.header "Cookie"
  case mv of
    Just v -> case parseCookies . TL.unpack $ v of
      Just parsed -> return . lookup "token" . map cookieToTuple $ parsed
      _ -> return Nothing
    _ -> return Nothing
  where
    cookieToTuple (Cookie k v _ _ _ _ _) = (k, v)

getUser :: (ScottyError e) => ActionT e WebM (Maybe User)
getUser = do
  redis <- webM $ gets stateRedis
  mtoken <- accessToken
  case mtoken of
    Nothing -> return Nothing
    Just token -> do
      liftIO $ R.runRedis redis $ do
        R.expire (B.pack token) (3600*24)
        -- Redis (Either Reply (Maybe ByteString))
        t <- R.get (B.pack token)
        case t of
          Left (R.SingleLine json) -> return $ A.decodeStrict json
          _ -> return Nothing

setUser :: (ScottyError e) => User -> ActionT e WebM ()
setUser user = do
  token <- (take 15 . randomRs ('a','z')) <$> (liftIO $ newStdGen)
  redis <- webM $ gets stateRedis
  saveUser redis token user
  setTokenCookie token
  where
    cookie token = Cookie "token" token Nothing Nothing Nothing False False
    saveUser redis token = liftIO . R.runRedis redis . R.set (B.pack token) . BL.toStrict . A.encode
    setTokenCookie = addHeader "Set-Cookie" . TL.pack . serializeCookie . cookie
    
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