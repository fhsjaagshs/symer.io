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

import Web.App
import Database.PostgreSQL.Simple.FromRow

import Control.Monad.IO.Class

import Network.Wai
import qualified Data.Vault.Lazy as V
import System.IO.Unsafe

import Data.Maybe
import Data.Bool
import Data.Text (Text)
import Data.Monoid
import Text.Read
import qualified Data.ByteString.Char8 as B

data User = User {
  userUID :: !Integer,
  userUsername :: Text,
  userPasswordHash :: Text
} deriving (Show)

instance Eq User where
  (User a _ _) == (User b _ _) = a == b

instance FromRow User where
  fromRow = User <$> field <*> field <*> phashField
    where phashField = numFieldsRemaining >>= bool field (return "") . (==) 0

getUser :: (MonadIO m) => Text -> RouteT AppState m (Maybe User)
getUser username = listToMaybe <$> postgresQuery q [username]
  where q = "SELECT * FROM user_t WHERE UserName=? LIMIT 1"

accessToken :: (WebAppState s, MonadIO m) => RouteT s m (Maybe Integer)
accessToken = (>>= readKey "token") <$> (header "Cookie")

readKey :: (Read a) => B.ByteString -> B.ByteString -> Maybe a    
readKey k = readMaybe . B.unpack . dropKey . findPair
  where findPair = B.takeWhile (/= ';') . snd . B.breakSubstring (k <> "=")
        dropKey = B.drop (1 + B.length k)

authenticate :: (MonadIO m) => RouteT AppState m User
authenticate = getAuthenticatedUser >>= maybe onNothing return
  where onNothing = redirect "/login?err=Login%20required%2E" >> error ""

getAuthenticatedUser :: (MonadIO m) => RouteT AppState m (Maybe User)
getAuthenticatedUser = do
  mu <- V.lookup authUserKey . vault <$> request
  maybe (accessToken >>= maybe (pure Nothing) j) (pure . Just) mu
  where j t = listToMaybe <$> postgresQuery sql [t]
        sql = "SELECT u.* FROM user_t u INNER JOIN session_t s ON s.UserID=u.UserID WHERE s.SessionID=? LIMIT 1"

setAuthenticatedUser :: (MonadIO m) => User -> RouteT AppState m ()
setAuthenticatedUser u@(User uid _ _) = do
  (token :: Maybe Integer) <- onlyQuery $ postgresQuery "INSERT INTO session_t (UserID) VALUES (?) RETURNING SessionID" [uid]
  case token of
    Just v -> do
      opts <- bool "" ";Secure" <$> isTLS
      addHeader "Set-Cookie" $ "token=" <> (B.pack $ show v) <> ";expires=Fri, 31 Dec 9999 23:59:59 GMT;HttpOnly" <> opts
      mapRequest (\r -> r { vault = V.insert authUserKey u (vault r) } )
    Nothing -> return ()

deleteAuth :: (MonadIO m) => RouteT AppState m ()
deleteAuth = accessToken >>= maybe (return ()) f
  where f token = do
          postgresExec "DELETE FROM session_t WHERE SessionID=?" [token]
          addHeader "Set-Cookie" "token=deleted;max-age=-1"
          mapRequest (\r -> r { vault = V.delete authUserKey (vault r) } )
          
authUserKey :: V.Key User
authUserKey = unsafePerformIO V.newKey
{-# NOINLINE authUserKey #-}
