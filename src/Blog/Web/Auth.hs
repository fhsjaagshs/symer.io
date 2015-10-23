{-# LANGUAGE OverloadedStrings #-}

module Blog.Web.Auth
(
  getUser,
  setUser,
  authenticate,
  deleteAuth,
  accessToken
)
where
  
import Blog.State
import Blog.User
import Blog.Web.Cookie

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
getUser = accessToken >>= maybe (return Nothing) (f . B.pack)
  where
    f token = do
      redis <- webM $ gets stateRedis
      liftIO $ R.runRedis redis $ do
        R.expire token 86400 -- 24 hours
        g <$> R.get token
    g (Right (Just j)) = A.decodeStrict j
    g _ = Nothing

setUser :: (ScottyError e) => User -> ActionT e WebM ()
setUser user = void $ do
  redis <- webM $ gets stateRedis
  token <- liftIO $ mkToken
  liftIO $ saveUser redis token user
  setToken token -- FIXME: this screws up pre warp-3.1.4
  where
    mkToken = (take 15 . randomRs ('a','z')) <$> newStdGen
    saveUser redis token = R.runRedis redis . R.set (B.pack token) . BL.toStrict . A.encode
    setToken token = Scotty.addHeader "Set-Cookie" $ mconcat ["token=", TL.pack token, "; HttpOnly; Secure"]
    
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