{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Post
(
  Post(..),
  postDescription
)
where

import Blog.User
import Blog.Util.Markdown

import           Cheapskate
import           Data.Aeson as Aeson

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock

import           Blog.Database.PGExtensions()
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow

data Post = Post {
  postID :: !Integer,
  postTitle :: Text,
  postBody :: Text,
  postTimestamp :: UTCTime,
  postTags :: [Text],
  postIsDraft :: !Bool,
  postAuthor :: User
} deriving (Show)

instance Eq Post where
  (==) a_ b_ = (postID a_) == (postID b_)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance ToRow Post where
  toRow (Post pid title body ts tags isDraft (User authorId _ _ _)) =
    [toField pid,
    toField title,
    toField body,
    toField ts,
    toField tags,
    toField authorId,
    toField isDraft]
  
instance ToJSON Post where
  toJSON (Post pid title body ts tags isDraft author) =
    Aeson.object [
      "id" .= pid,
      "title" .= title,
      "body" .= body,
      "timestamp" .= ts,
      "tags" .= tags,
      "draft" .= isDraft,
      "author" .= toJSON author
    ]

postDescription :: Post -> Text
postDescription = T.take 150 . stripMarkdown . markdown def . postBody
