{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Blog.Util.DumpDatabase
(
  loadDB,
  dumpDB
)
where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.List (nub)
import qualified Data.ByteString.Lazy.Char8 as BL (writeFile,readFile)
import qualified Data.ByteString.Char8 as B (pack)
  
import Control.Monad
  
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToField

data DumpUser = DumpUser Integer Text Text Text

instance ToJSON DumpUser where
  toJSON (DumpUser i u d ph) = object ["id" .= i,
                                       "username" .= u,
                                       "display_name" .= d,
                                       "password_hash" .= ph]
                                             
instance FromJSON DumpUser where
  parseJSON (Object v) = DumpUser
                         <$> v .: "id"
                         <*> v .: "username"
                         <*> v .: "display_name"
                         <*> v .: "password_hash"
  parseJSON _          = mzero

instance FromRow DumpUser where
  fromRow = DumpUser <$> field <*> field <*> field <*> field
  
instance ToRow DumpUser where
  toRow (DumpUser i u d ph) = toRow (i,u,d,ph)
  
data DumpComment = DumpComment Integer (Maybe Integer) Integer Text Text UTCTime Text

instance ToJSON DumpComment where
  toJSON (DumpComment i p pst e d t b) = object ["id" .= i,
                                                 "parent_id" .= p,
                                                 "post_id" .= pst,
                                                 "email" .= e,
                                                 "display_name" .= d,
                                                 "timestamp" .= t,
                                                 "body" .= b]
instance FromJSON DumpComment where
  parseJSON (Object v) = DumpComment
                         <$> v .: "id"
                         <*> v .: "parent_id"
                         <*> v .: "post_id"
                         <*> v .: "email"
                         <*> v .: "display_name"
                         <*> v .: "timestamp"
                         <*> v .: "body"
  parseJSON _          = mzero

instance FromRow DumpComment where
  fromRow = DumpComment <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DumpComment where
  toRow (DumpComment i pid pstid e d t b) = toRow (i,pid,pstid,e,d,t,b)
  
data DumpPost = DumpPost Integer Text Text UTCTime [Text] Integer Bool

instance ToJSON DumpPost where
  toJSON (DumpPost i t b ts tg aid d) = object ["id" .= i,
                                                "title" .= t,
                                                "body" .= b,
                                                "timestamp" .= ts,
                                                "tags" .= tg,
                                                "author_id" .= aid,
                                                "draft" .= d]
  
instance FromJSON DumpPost where
  parseJSON (Object v) = DumpPost
                         <$> v .: "id"
                         <*> v .: "title"
                         <*> v .: "body"
                         <*> v .: "timestamp"
                         <*> v .: "tags"
                         <*> v .: "author_id"
                         <*> v .: "draft"
  parseJSON _          = mzero

instance FromRow DumpPost where
  fromRow = DumpPost <$> field <*> field <*> field <*> field <*> (fmap fromPGArray field) <*> field <*> field

instance ToRow DumpPost where
  toRow (DumpPost i t b ts tg d aid) = toRow (i,t,b,ts,mkTagsField tg,d,aid)
    where mkTagsField tags = Many [toField $ PGArray $ nub tags, Plain "::text[]"]

data Dump = Dump [DumpUser] [DumpPost] [DumpComment]

instance ToJSON Dump where
  toJSON (Dump u p c) = object ["users" .= u,
                                "posts" .= p,
                                "comments" .= c]

instance FromJSON Dump where
  parseJSON (Object v) = Dump
                         <$> v .: "users"
                         <*> v .: "posts"
                         <*> v .: "comments"
  parseJSON _          = mzero

loadDB :: FilePath -> String -> IO ()
loadDB fp connstring = do
  dump <- eitherDecode <$> BL.readFile fp
  case dump of
    Left err -> putStrLn err
    Right (Dump u p c) -> do
      conn <- connectPostgreSQL $ B.pack connstring
      executeMany conn "INSERT INTO users VALUES (?,?,?,?)" u
      executeMany conn "INSERT INTO posts VALUES (?,?,?,?,?,?,?)" p
      executeMany conn "INSERT INTO comments VALUES (?,?,?,?,?,?,?)" c
      
      close conn
  
dumpDB :: FilePath -> String -> IO ()
dumpDB fp connstring = do
  conn <- connectPostgreSQL $ B.pack connstring
  dump <- Dump
          <$> query_ conn "SELECT * FROM users"
          <*> query_ conn "SELECT * FROM posts"
          <*> query_ conn "SELECT * FROM comments"
  close conn
  BL.writeFile fp (encode dump)
  