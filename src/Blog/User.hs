{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Blog.User
(
  User(..),
  getUser
)
where

import Blog.Postgres

import Control.Monad

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 (ByteString)

import Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Char8 as A
import Blaze.ByteString.Builder (fromByteString)

import Database.PostgreSQL.Simple.FromField as PG.FromField
import Database.PostgreSQL.Simple.ToField as PG.ToField
import Database.PostgreSQL.Simple.FromRow as PG.FromRow
import Database.PostgreSQL.Simple.ToRow as PG.ToRow

data User = User {
  userUID :: !Integer,
  userUsername :: Text,
  userDisplayName :: Text,
  userPasswordHash :: Text
} deriving (Show)

instance ToJSON User where
  toJSON User{..} = Aeson.object ["uid" .= userUID,
                                  "username" .= userUsername,
                                  "display_name" .= userDisplayName,
                                  "password_hash" .= userPasswordHash]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .: "uid"
    <*> o .: "username"
    <*> o .: "display_name"
    <*> o .: "password_hash"
  parseJSON _ = mzero

instance Eq User where
  (User a_ _ _ _) == (User b_ _ _ _) = a_ == b_

instance FromField User where
  fromField f (Just fdata) = do
    tinfo <- typeInfo f
    case tinfo of
      (Composite _ _ _ "users" _ _) -> do
        case parseUserRow fdata of
          Just user -> return user
          Nothing -> returnError ConversionFailed f "Failed to read users field."
      _ -> returnError ConversionFailed f "Failed to read users field."
  fromField f Nothing = do
    tinfo <- typeInfo f
    case tinfo of
      (Composite _ _ _ "users" _ _) -> returnError Incompatible f "Empty user field."
      _ -> returnError ConversionFailed f "Failed to read users field."

--------------------------------------------------------------------------------
instance ToField User where
  toField (User uid uname dname phash) =
    Many [plain "ROW(",
          toField uid, comma,
          toField uname, comma,
          toField dname, comma,
          toField phash,
          plain ")"]
    where
      plain = Plain . fromByteString
      comma = plain ","
  
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
  
instance ToRow User where
  toRow (User uid username displayName passwordHash) =
    [toField uid,
    toField username,
    toField displayName,
    toField passwordHash]
    
parseUserRow :: ByteString -> Maybe User
parseUserRow = maybeResult . A.parse userRowParser
  where
    -- TODO: Allow for end paren aka ')' to be part of a field
    userRowParser = User
      <$> (char '(' >> decimal)
      <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
      <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
      <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
      where
        takeable ',' = False
        takeable '"' = False
        takeable ')' = False
        takeable _   = True
        skipUntakeable = skipWhile (not . takeable)
        takeTakeable = A.takeWhile takeable
    
getUser :: Text -> PostgresActionM (Maybe User)
getUser username = listToMaybe <$> postgresQuery "SELECT * FROM users WHERE username=? LIMIT 1" [username]