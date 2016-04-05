{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Blog.User
(
  User(..),
  getUser,
  getAuthenticatedUser,
  setAuthenticatedUser,
  authenticate,
  deleteAuth
)
where

import Blog.AppState
import Blog.Cookie

import Web.App
import Database.PostgreSQL.Simple.FromRow

import Control.Monad.IO.Class

import Data.Maybe
import Data.Text (Text)
import Data.Monoid
import qualified Data.ByteString.Char8 as B

data User = User {
  userUID :: !Integer,
  userUsername :: Text,
  userPasswordHash :: Text
} deriving (Show)

instance Eq User where
  (User a _ _) == (User b _ _) = a == b

instance FromRow User where
  fromRow = numFieldsRemaining >>= f
    where f 2 = User <$> field <*> field <*> (return "") -- when hash is excluded for security
          f _ = User <$> field <*> field <*> field

getUser :: (MonadIO m) => Text -> RouteT AppState m (Maybe User)
getUser username = listToMaybe <$> postgresQuery q [username]
  where q = "SELECT * FROM user_t WHERE UserName=? LIMIT 1"
  
accessToken :: (WebAppState s, MonadIO m) => RouteT s m (Maybe Integer)
accessToken = fmap read . lookup "token" <$> getCookies

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
    Just v -> addHeader "Set-Cookie" $ "token=" <> (B.pack $ show v) <> ";expires=Fri, 31 Dec 9999 23:59:59 GMT;"-- HttpOnly;Secure"
    Nothing -> return ()

deleteAuth :: (MonadIO m) => RouteT AppState m ()
deleteAuth = accessToken >>= maybe (return ()) f
  where f token = do
          postgresExec "DELETE FROM session_t WHERE SessionID=?" [token]
          addHeader "Set-Cookie" "token=deleted;max-age=-1"