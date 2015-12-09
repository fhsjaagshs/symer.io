{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}

module Blog.Post
(
  -- * Data Structures
  Post(..),
  -- * Post Representations
  postDescription,
  -- * Queries
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
import Blog.Postgres

import Cheapskate

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (take)
import Data.Time.Clock (UTCTime)
import Data.List (nub)

import Database.PostgreSQL.Simple.FromRow (FromRow(..),field)
import Database.PostgreSQL.Simple.ToField -- (toField)
import Database.PostgreSQL.Simple.Types (PGArray(..))

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
  fromRow = Post <$> field <*> field <*> field <*> field <*> (fmap fromPGArray field) <*> field <*> field

-- |Get an SEO-ready description from a post.
postDescription :: Post -> Text
postDescription = T.take 150 . stripMarkdown . markdown def . postBody

-- |Number of posts to return per page.
postsPerPage :: Int
postsPerPage = 10

-- TODO: use postgresql-simple's fold function instead of loading all posts into memory

-- |Either @INSERT@ or @UPDATE@ a post in the database.
upsertPost :: Maybe Integer -- ^ post identifier
           -> Text -- ^ post title
           -> Text -- ^ post body
           -> [Text] -- ^ post tags
           -> Bool -- ^ whether the post is a draft
           -> User -- ^ post author
           -> PostgresActionM (Maybe Integer) -- ^ the identifier of the post from the database
upsertPost (Just p) t b tg d (User aid _ _ _) = onlyQuery $ postgresQuery sql (t,b,mkTagsField tg,d,aid,p)
  where sql = "UPDATE posts SET title=?,body=?,tags=?,draft=? WHERE author_id=? AND id=? RETURNING id"
upsertPost Nothing t b tg d (User aid _ _ _) = onlyQuery $ postgresQuery sql (t,b,mkTagsField tg,d,aid)
  where sql = "INSERT INTO posts (title,body,tags,draft,author_id) VALUES (?,?,?,?,?) RETURNING id"

-- |Get a page of posts by tag.
getPostsByTag :: Text -- ^ a tag
              -> Integer -- ^ the page number
              -> PostgresActionM [Post]
getPostsByTag tag pageNum = postgresQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where sql = "SELECT * FROM v_posts WHERE ?=any(tags) ORDER BY timestamp DESC OFFSET ? LIMIT ?"

-- |Get a page of posts.
getPosts :: Integer -- ^ page number
         -> PostgresActionM [Post]
getPosts pageNum = postgresQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_posts ORDER BY timestamp DESC OFFSET ? LIMIT ?"

-- |Get a page of a user's drafts.
getDrafts :: User -- ^ user to get drafts for
          -> Integer -- ^ page number
          -> PostgresActionM [Post]
getDrafts user pageNum = postgresQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_drafts p WHERE (p.user).id=? ORDER BY timestamp DESC OFFSET ? LIMIT ?"

-- |Get a post by id.
getPost :: Integer -- ^ post id
        -> PostgresActionM (Maybe Post)
getPost pid = listToMaybe <$> postgresQuery sql [pid]
  where sql = "SELECT * FROM v_posts_all WHERE id=? LIMIT 1"

-- |Delete a post.
deletePost :: User -- ^ post owner
           -> Integer -- ^ post id
           -> PostgresActionM (Maybe Integer) -- ^ id of deleted post
deletePost (User uid _ _ _) pid = onlyQuery $ postgresQuery sql (pid, uid)
  where sql = "DELETE FROM posts WHERE id=? AND author_id=? RETURNING id"

{- Internal -}

-- |Satisfy SQL type checking if the tags list is empty
mkTagsField :: [Text] -- ^ the tags to serialize
            -> Action -- ^ the resulting type cast field value for the tags list
mkTagsField tags = Many [toField $ PGArray $ nub tags, Plain "::text[]"]