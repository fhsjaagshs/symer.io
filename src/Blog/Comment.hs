{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Comment
(
  Comment(..),
  getCommentsForPost,
  insertComment
)
where

import Web.App

import Blog.AppState
import Blog.Util.Markdown

import Control.Monad.IO.Class

import Data.Maybe
import Data.Time.Clock
import Data.Text (Text)
import Data.Bool

import Data.Aeson
import Database.PostgreSQL.Simple.FromRow

-- |Data structure representing a comment on a post. Never use the
-- 'Comment' constructor directly; If you want to make a comment, use
-- the 'insertComment' function.
data Comment = Comment {
  commentID :: !Integer, -- ^ the comment's id
  commentParentID :: Maybe Integer, -- ^ the id of the comment's parent comment
  commentPostID :: !Integer, -- ^ the id of the post the comment is on
  commentBody :: Text, -- ^ the comment itself
  commentTimestamp :: UTCTime, -- ^ when the comment was made
  commentChildren :: [Comment] -- ^ replies to the comment; empty until @nestComments@ is used
} deriving (Show)

instance Eq Comment where
  (Comment a _ _ _ _ _) == (Comment b _ _ _ _ _) = a == b

instance ToJSON Comment where
 toJSON (Comment cid _ postId bdy ts children) = object
   ["id" .= cid,
    "post_id" .= postId,
    "timestamp" .= ts,
    "body" .= (renderMarkdown $ parseMarkdown bdy),
    "children" .= map toJSON children]

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field <*> return []

-- |Nest comments based on @commentParentID@.
nestComments :: [Comment] -> [Comment]
nestComments = f []
  where
    f rs [] = rs
    f rs (x@(Comment _ Nothing _ _ _ _):xs) = f (rs ++ [x]) xs
    f rs (x:xs) = f (map (insert x) rs) (map (insert x) xs)
      where
        insert c a = modifyChildren a $ bool (map $ insert c) (++ [c]) $ isParent a c
        isParent p c = maybe False ((==) (commentID p)) $ commentParentID c
        modifyChildren n g = n { commentChildren = g $ commentChildren n }
    
-- | Get a post's comments
getCommentsForPost :: (MonadIO m)
                   => Integer -- ^ post id
                   -> RouteT AppState m [Comment]
getCommentsForPost pid = nestComments <$> postgresQuery "SELECT * FROM comment_t WHERE PostID=?" [pid]

-- |Insert a comment into the database
insertComment :: (MonadIO m)
              => Maybe Integer -- ^ parent comment id
              -> Integer -- ^ post id
              -> Text -- ^ body
              -> RouteT AppState m (Maybe Comment) -- ^ the inserted comment
insertComment parent i b = listToMaybe <$> postgresQuery "INSERT INTO comment_t (CommentParentID,PostID,CommentBody) VALUES (?,?,?) RETURNING *" (parent,i,b)