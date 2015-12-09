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
  
import Blog.Postgres
  
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
  commentEmail :: Text, -- ^ the email of the commenter
  commentDisplayName :: Text, -- ^ the display name of the commenter
  commentTimestamp :: UTCTime, -- ^ when the comment was made
  commentBody :: Text, -- ^ the comment itself
  commentChildren :: [Comment] -- ^ replies to the comment
} deriving (Eq,Show)

instance ToJSON Comment where
 toJSON (Comment cid _ postId email dname ts body children) =
    Aeson.object ["id" .= cid,
                  "post_id" .= postId,
                  "email" .= email,
                  "display_name" .= dname,
                  "timestamp" .= ts,
                  "body" .= (renderHtml $ renderDoc $ markdown def body),
                  "children" .= map toJSON children]

instance FromRow Comment where
  fromRow = Comment
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> return []

-- TODO: add strictness ($!)
-- TODO: fix children ordering (it's reverse)
-- TODO: make sure comment ordering doesn't affect nesting (this func
--       works only if @cmnts@ is in posted order due to the nature
--       of how comments & replies are posted)
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
getCommentsForPost :: Integer -- ^ post id
                   -> PostgresActionM [Comment]
getCommentsForPost pid = nestComments <$> postgresQuery "SELECT * FROM comments WHERE postId=?" [pid]

-- TODO: have this take a 'Comment', switching 'commentID' to a @Maybe Integer@
-- |Insert a comment into the database
insertComment :: Maybe Integer -- ^ parent comment id
              -> Integer -- ^ post id
              -> Text -- ^ email
              -> Text -- ^ display name
              -> Text -- ^ body
              -> PostgresActionM (Maybe Comment) -- ^ the inserted comment
insertComment (Just parentId) i e dn b = listToMaybe <$> postgresQuery "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING *" (parentId, i, e, dn, b)
insertComment Nothing         i e dn b = listToMaybe <$> postgresQuery "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING *" (i, e, dn, b)