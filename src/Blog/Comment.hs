{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Comment
(
  -- * Types
  Comment(..),
  -- * Comment Creation
  getCommentsForPost,
  insertComment
)
where
  
import Web.App

import Blog.AppState

import Control.Monad.IO.Class
  
import Data.Maybe
import Data.Time.Clock
import Data.Text (Text)
import Data.Aeson as Aeson

import Cheapskate
import Cheapskate.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
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
  commentChildren :: [Comment] -- ^ replies to the comment
} deriving (Show) -- TODO: ord instance

instance Eq Comment where
  (Comment a _ _ _ _ _) == (Comment b _ _ _ _ _) = a == b

instance ToJSON Comment where
 toJSON (Comment cid _ postId bdy ts children) =
    Aeson.object ["id" .= cid,
                  "post_id" .= postId,
                  "timestamp" .= ts,
                  "body" .= (renderHtml $ renderDoc $ markdown def bdy),
                  "children" .= map toJSON children]

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field <*> return []

-- TODO: add strictness ($!)
-- TODO: fix children ordering (it's reverse)
-- TODO: make sure comment ordering doesn't affect nesting (this func
--       works only if @cmnts@ is in posted order due to the nature
--       of how comments & replies are posted) (Ord instance)
nestComments :: [Comment] -> [Comment]
nestComments cmnts = map (f antiroots) roots
  where
    roots = filter (isNothing . commentParentID) cmnts
    antiroots = filter (isJust . commentParentID) cmnts
    f [] a = a
    f (x:xs) a = f xs $! addHierarchical a x
    modifyChildren n g = n { commentChildren = (g $ commentChildren n) }
    isParent p c = maybe False ((==) (commentID p)) (commentParentID c)
    addHierarchical a c = modifyChildren a $! if isParent a c
                            then ((:) c)
                            else (map $ \a' -> addHierarchical a' c)

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