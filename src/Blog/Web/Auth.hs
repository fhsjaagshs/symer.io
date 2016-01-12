{-# LANGUAGE OverloadedStrings #-}

module Blog.Web.Auth
(
  getAuthenticatedUser,
  setAuthenticatedUser,
  authenticate,
  deleteAuth,
  accessToken
)
where
  
import Web.App
  
import Blog.AppState
import Blog.User
import Blog.Web.Cookie

import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H (lookup)

import Data.Maybe
import System.Random

accessToken :: (WebAppState s, MonadIO m) => RouteT s m (Maybe String)
accessToken = f <$> header "Cookie"
  where
    f = maybe Nothing (g . parseCookie)
    g = maybe Nothing (H.lookup "token" . cookiePairs)

authenticate :: (MonadIO m) => RouteT AppState m User
authenticate = getAuthenticatedUser >>= maybe onNothing return
  where onNothing = redirect "/login?err=Login%20required%2E" >> (return $ User 0 "" "" "")

getAuthenticatedUser :: (MonadIO m) => RouteT AppState m (Maybe User)
getAuthenticatedUser = accessToken >>= f
  where f (Just t) = listToMaybe <$> postgresQuery sql [t]
        f Nothing = return Nothing
        sql = "SELECT u.* FROM users u, auth a WHERE a.token=? AND a.user_id=u.id LIMIT 1"

setAuthenticatedUser :: (MonadIO m) => User -> RouteT AppState m ()
setAuthenticatedUser (User uid _ _ _) = do
  token <- liftIO $ B.pack . take 15 . curry randomRs 'a' 'z' <$> newStdGen
  postgresExec "INSERT INTO auth (token,user_id) VALUES (?,?)" (token, uid)
  addHeader "Set-Cookie" $ mconcat ["token=", token, "; HttpOnly; Secure"] -- this screws up pre warp-3.1.4

deleteAuth :: (MonadIO m) => RouteT AppState m ()
deleteAuth = accessToken >>= f
  where f Nothing = return ()
        f (Just token) = do
          postgresExec "DELETE FROM auth WHERE token=?" [token :: String]
          addHeader "Set-Cookie" "token=deleted; max-age=-1"