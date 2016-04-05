{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import Data.Monoid

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H (lookup)

import Data.Maybe

accessToken :: (WebAppState s, MonadIO m) => RouteT s m (Maybe Integer)
accessToken = fmap read . listToMaybe . f <$> getCookies
  where f = catMaybes . map (H.lookup "token" . cookiePairs)

authenticate :: (MonadIO m) => RouteT AppState m User
authenticate = getAuthenticatedUser >>= maybe onNothing return
  where onNothing = redirect "/login?err=Login%20required%2E" >> (return $ User 0 "" "")

getAuthenticatedUser :: (MonadIO m) => RouteT AppState m (Maybe User)
getAuthenticatedUser = accessToken >>= maybe (return Nothing) f
  where f t = (listToMaybe <$> postgresQuery sql [t])
        sql = "SELECT u.* FROM user_t u INNER JOIN session_t s ON s.UserID=u.UserID WHERE s.SessionID=? LIMIT 1"

setAuthenticatedUser :: (MonadIO m) => User -> RouteT AppState m ()
setAuthenticatedUser (User uid _ _) = do
  (token :: Maybe Integer) <- onlyQuery $ postgresQuery "INSERT INTO session_t (UserID) VALUES (?) RETURNING SessionID" [uid]
  case token of
    Just v -> do
      liftIO $ putStr "setting token: "
      liftIO $ print v
      addHeader "Set-Cookie" $ "token=" <> (B.pack $ show v) <> ";expires=Fri, 31 Dec 9999 23:59:59 GMT;"-- HttpOnly;Secure"
    Nothing -> return ()

deleteAuth :: (MonadIO m) => RouteT AppState m ()
deleteAuth = accessToken >>= maybe (return ()) f
  where f token = do
          postgresExec "DELETE FROM session_t WHERE SessionID=?" [token]
          addHeader "Set-Cookie" "token=deleted;max-age=-1"