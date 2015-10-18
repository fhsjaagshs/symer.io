{-# LANGUAGE OverloadedStrings #-}

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

import Data.Default
import System.Random

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.HashMap.Strict as H

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.Trans as Scotty

import qualified Data.Aeson as A
import qualified Database.Redis as R

accessToken :: (ScottyError e) => ActionT e WebM (Maybe String)
accessToken = f <$> Scotty.header "Cookie"
  where
    f = maybe Nothing (g . parseCookie . BL.toStrict . TL.encodeUtf8)
    g = maybe Nothing (H.lookup "token" . cookiePairs)
    
getUser :: (ScottyError e) => ActionT e WebM (Maybe User)
getUser = accessToken >>= f
  where
    f = maybe (return Nothing) g
    g token = do
      redis <- webM $ gets stateRedis
      liftIO $ R.runRedis redis $ do
        R.expire (B.pack token) (3600*24)
        (R.get $ B.pack token) >>= return . e
    e (Right (Just j)) = A.decodeStrict j
    e _ = Nothing

setUser :: (ScottyError e) => User -> ActionT e WebM ()
setUser user = do
  token <- (take 15 . randomRs ('a','z')) <$> (liftIO $ newStdGen)
  redis <- webM $ gets stateRedis
  saveUser redis token user
  setToken token
  where
    cookie t = def { cookiePairs = (H.singleton "token" t), cookieHttpOnly = True }
    saveUser redis token = liftIO . R.runRedis redis . R.set (B.pack token) . BL.toStrict . A.encode
    setToken = mapM_ (addHeader "Set-Cookie" . TL.pack) . serializeCookie . cookie
    
deleteAuth :: (ScottyError e) => ActionT e WebM ()
deleteAuth = accessToken >>= f
  where
    f (Just token) = void $ do
      r <- webM $ gets stateRedis
      liftIO . R.runRedis r . R.del $ [(B.pack token)]
    f _ = return ()

authenticate :: (ScottyError e) => ActionT e WebM (Maybe User)
authenticate = getUser >>= f
  where
    f (Just user) = return $ Just user
    f _ = redirect "/login?err=Login%20required%2E"