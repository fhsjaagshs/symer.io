{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}

module Blog.Post
(
  -- * Data Structures
  Post(..),
  -- * Post Representations
  postDescription,
  -- * Queries
  isOnLastPage,
  upsertPost,
  getPostsByTag,
  getPosts,
  getDrafts,
  getPost,
  deletePost,
  -- * Config
  postsPerPage
)
where

import Blog.User
import Blog.Util.Markdown
import Blog.AppState

import Control.Monad.IO.Class

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (take)
import Data.Time.Clock (UTCTime)

import Web.App
import Cheapskate
import Database.PostgreSQL.Simple.FromRow (FromRow(..),field)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (PGArray(..),Only(..))

-- |Represents a post - a row from the view @v_posts@.
data Post = Post {
  postID :: Integer, -- ^ the post's identifier
  postTitle :: Text, -- ^ post's title
  postBody :: Text, -- ^ the post itself
  postTimestamp :: UTCTime, -- ^ when the post was created
  postTags :: [Text], -- ^ a post's tags
  postDraft :: !Bool, -- ^ if the post is a draft
  postAuthor :: User -- ^ the user who wrote the post
} deriving (Eq, Show)

instance FromRow Post where -- as selected from v_posts or v_drafts
  fromRow = Post <$> field <*> field <*> field <*> field <*> (fmap fromPGArray field) <*> field <*> fromRow

-- TODO: tags table & use_tag(text) function

-- |Get an SEO-ready description from a post.
postDescription :: Post -> Text
postDescription = T.take 150 . stripMarkdown . markdown def . postBody

-- |Number of posts to return per page.
postsPerPage :: Int
postsPerPage = 10

-- |Determine if a post is on the last page
isOnLastPage :: (MonadIO m) => Integer -> RouteT AppState m Bool
isOnLastPage = fmap (maybe True (<= postsPerPage)) . onlyQuery . postgresQuery sql . Only
  where sql = "SELECT count(p.PostID) FROM post_t p WHERE p.PostID > ?"

-- TODO: use postgresql-simple's fold function instead of loading all posts into memory

-- |Either @INSERT@ or @UPDATE@ a post in the database.
upsertPost :: (MonadIO m)
           => Maybe Integer -- ^ post identifier
           -> Text -- ^ post title
           -> Text -- ^ post body
           -> [Text] -- ^ post tags
           -> Bool -- ^ whether the post is a draft
           -> User -- ^ post author
           -> RouteT AppState m (Maybe Integer) -- ^ the identifier of the post from the database
upsertPost (Just pid) title body_ tags draft (User authorid _ _) =
  onlyQuery $ postgresQuery "SELECT update_post(?,?,?,?,?,?)" (pid,title,body_,draft,mkTagsField tags,authorid)
upsertPost Nothing title body_ tags draft (User authorid _ _) =
  onlyQuery $ postgresQuery "SELECT insert_post(?,?,?,?,?)" (title,body_,draft,mkTagsField tags,authorid)

-- |Get a page of posts by tag.
getPostsByTag :: (MonadIO m)
              => Text -- ^ a tag
              -> Integer -- ^ the page number
              -> RouteT AppState m [Post]
getPostsByTag tag pageNum = postgresQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where sql = "SELECT * FROM v_posts WHERE ?=any(tags) OFFSET ? LIMIT ?"

-- |Get a page of posts.
getPosts :: (MonadIO m)
         => Integer -- ^ page number
         -> RouteT AppState m [Post]
getPosts pageNum = postgresQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_posts OFFSET ? LIMIT ?"

-- |Get a page of a user's drafts.
getDrafts :: (MonadIO m)
          => User -- ^ user to get drafts for
          -> Integer -- ^ page number
          -> RouteT AppState m [Post]
getDrafts user pageNum = postgresQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_drafts WHERE UserID=? OFFSET ? LIMIT ?"

-- |Get a post by id.
getPost :: (MonadIO m)
        => Integer -- ^ post id
        -> RouteT AppState m (Maybe Post)
getPost pid = listToMaybe <$> postgresQuery sql [pid]
  where sql = "SELECT * FROM v_posts_all WHERE PostID=? LIMIT 1"

-- |Delete a post.
deletePost :: (MonadIO m)
           => User -- ^ post owner
           -> Integer -- ^ post id
           -> RouteT AppState m (Maybe Integer) -- ^ id of deleted post
deletePost (User uid _ _) pid = onlyQuery $ postgresQuery sql (pid, uid)
  where sql = "DELETE FROM post_t WHERE PostID=? AND PostAuthorID=? RETURNING PostID"

{- Internal -}

-- |Satisfy SQL type checking if the tags list is empty
mkTagsField :: [Text] -- ^ the tags to serialize
            -> Action -- ^ the resulting type cast field value for the tags list
mkTagsField tags = Many [toField $ PGArray tags,Plain "::text[]"]