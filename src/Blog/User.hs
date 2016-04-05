{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Blog.User
(
  User(..),
  getUser
)
where

import Blog.AppState

import Control.Monad.IO.Class

import Web.App

import Data.Maybe
import Data.Text (Text)

import Data.Aeson as Aeson
import Database.PostgreSQL.Simple.ToField as PG.ToField
import Database.PostgreSQL.Simple.FromRow as PG.FromRow
import Database.PostgreSQL.Simple.ToRow as PG.ToRow

data User = User {
  userUID :: !Integer,
  userUsername :: Text,
  userPasswordHash :: Text
} deriving (Eq,Show)

instance ToJSON User where
  toJSON User{..} = Aeson.object ["id" .= userUID,
                                  "username" .= userUsername,
                                  "password_hash" .= userPasswordHash]

instance FromRow User where
  fromRow = numFieldsRemaining >>= f
    where f 2 = User <$> field <*> field <*> (return "") -- for security
          f _ = User <$> field <*> field <*> field
  
instance ToRow User where
  toRow (User i u ph) = [toField i, toField u, toField ph]

getUser :: (MonadIO m) => Text -> RouteT AppState m (Maybe User)
getUser username = listToMaybe <$> postgresQuery q [username]
  where q = "SELECT * FROM user_t WHERE UserName=? LIMIT 1"