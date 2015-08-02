{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Blog.User
(
  User(..)
)
where
  
import Control.Monad
import Data.Maybe

import           Data.Attoparsec.ByteString.Char8 as A

import           Blaze.ByteString.Builder (fromByteString)
  
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
  
import qualified Data.ByteString.Char8 as B
  
import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow

data User = User {
  userUID :: !Integer,
  userUsername :: T.Text,
  userDisplayName :: T.Text,
  userPasswordHash :: Maybe T.Text
} deriving (Show)

instance ToJSON User where
  toJSON User{..} = Aeson.object ["uid" .= userUID,
                                  "username" .= userUsername,
                                  "display_name" .= userDisplayName,
                                  "password_hash" .= userPasswordHash]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .:  "uid"
    <*> o .:  "username"
    <*> o .:  "display_name"
    <*> o .:? "password_hash"
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
    Many [Plain $ fromByteString "ROW(",
          toField uid, comma, toField uname, comma, toField dname, comma,
          fromMaybe nullv (toField <$> phash),
          Plain $ fromByteString ")"]
    where
      comma = Plain $ fromByteString ","
      nullv = (Plain $ fromByteString "NULL")
  
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
  
instance ToRow User where
  toRow (User uid username displayName passwordHash) =
    [toField uid,
    toField username,
    toField displayName,
    toField passwordHash]
    
parseUserRow :: B.ByteString -> Maybe User
parseUserRow = maybeResult . parse userRowParser

userRowParser :: Parser User
userRowParser = User
  <$> (skipUntakeable >> decimal)
  <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
  <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
  <*> (skipUntakeable >> option Nothing (Just . T.decodeUtf8 <$> takeTakeable))
  where
    takeable ',' = False
    takeable '"' = False
    takeable ')' = False
    takeable '(' = False
    takeable _   = True
    skipUntakeable = skipWhile (not . takeable)
    takeTakeable = A.takeWhile takeable