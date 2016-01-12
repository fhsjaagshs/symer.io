{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Blog.User
(
  User(..),
  getUser
)
where

import Blog.AppState

import Control.Monad.IO.Class
import Control.Applicative

import Web.App

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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
} deriving (Eq,Show)

instance ToJSON User where
  toJSON User{..} = Aeson.object ["id" .= userUID,
                                  "username" .= userUsername,
                                  "display_name" .= userDisplayName,
                                  "password_hash" .= userPasswordHash]

instance FromField User where
  fromField fld Nothing = returnError ConversionFailed fld "empty field"
  fromField fld (Just fdata) = typeInfo fld >>= f
      where convFailed = returnError ConversionFailed fld
            f (Composite _ _ _ "users" _ _) = g $ parseUserRow fdata
            f _ = convFailed "not a user field"
            g = either (convFailed . (++) "failed to parse user: ") return

instance ToField User where
  toField (User i u d ph) = Many [plain "ROW(",
                                  toField i, plain ",",
                                  toField u, plain ",",
                                  toField d, plain ",",
                                  toField ph,
                                  plain ")"]
    where plain = Plain . fromByteString
  
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
  
instance ToRow User where
  toRow (User i u d ph) = [toField i, toField u, toField d, toField ph]
    
parseUserRow :: ByteString -> Either String User
parseUserRow = eitherResult . A.parse user
  where
    user = char '(' *> (User <$> decimal
                             <*> (skipSeparator *> rowField)
                             <*> (skipSeparator *> rowField)
                             <*> (skipSeparator *> rowField)) <* char ')'
                       
    skipSeparator = char ',' *> skipSpace
    
    rowField = quotedField <|> unquotedField
    quotedField = char '"' *> (T.pack <$> many c) <* char '"'
    c = dblquote <|> snglquote <|> notquote
    dblquote = char '"' *> char '"'
    snglquote = char '\'' *> char '\''
    notquote = satisfy p
      where p '"' = False
            p '\'' = False
            p _ = True

    unquotedField = T.decodeUtf8 <$> A.takeWhile1 p
      where p ',' = False
            p ')' = False
            p _ = True

getUser :: (MonadIO m) => Text -> RouteT AppState m (Maybe User)
getUser username = listToMaybe <$> postgresQuery q [username]
  where q = "SELECT * FROM users WHERE username=? LIMIT 1"