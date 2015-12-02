{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}

module Blog.Post
(
  -- * Data Structures
  Post(..),
  -- * Post Representations
  postDescription,
  -- * Queries
  upsertPost,
  insertPost,
  updatePost,
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
import Data.Aeson as Aeson

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.List (intercalate)

import Database.PostgreSQL.Simple.ToField as PG.ToField
import Database.PostgreSQL.Simple.FromRow as PG.FromRow
import Database.PostgreSQL.Simple.ToRow as PG.ToRow
import Database.PostgreSQL.Simple.Types as PG.Types

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
  fromRow = Post <$> field <*> field <*> field <*> field <*> (fmap fromPGArray field) <*> field <*> field
  
instance ToRow Post where
  toRow (Post pid title body ts tags isDraft (User authorId _ _ _)) =
    [toField pid,
    toField title,
    toField body,
    toField ts,
    toField $ PGArray tags,
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

-- |Number of posts to return per page
postsPerPage :: Int
postsPerPage = 10

-- TODO: use postgresql-simple's fold function instead of loading all posts into memory

upsertPost :: User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
upsertPost user Nothing    title body tags draft = insertPost user title body tags draft
upsertPost user (Just pid) title body tags draft = updatePost user pid title body tags draft

insertPost :: User -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
insertPost user title body tags draft = maybeQuery $ postgresQuery q fieldValues
  where
    q = "INSERT INTO blogposts (" ++ intercalate "," fieldNames ++ ") VALUES (" ++ intercalate "," qMarks ++ ") RETURNING identifier"
    fieldNames = map fst values
    fieldValues = map snd values
    qMarks = replicate (length values) "?"
    values :: [(String, Action)]
    values = catMaybes $ [Just ("is_draft", toField draft),
                          Just ("author_id", toField $ userUID user),
                          mkField "title" <$> title,
                          mkField "bodyText" <$> body,
                          (mkField "tags" . PGArray) <$> tags]
      where
        mkField sql = (sql,) . toField

updatePost :: User -> Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
updatePost user pid title body tags draft = maybeQuery $ postgresQuery q (fvalues ++ [toField $ userUID user, toField pid])
  where
    q = "UPDATE blogposts SET " ++ (intercalate "," fsetters) ++ " WHERE author_id=? AND identifier=? RETURNING identifier"
    fsetters = "timestamp=clock_timestamp()":(map fst values)
    fvalues = map snd values
    values :: [(String, Action)]
    values = catMaybes $ [Just ("is_draft=?", toField draft),
                          mkField "title=?" title,
                          mkField "bodyText=?" body,
                          mkField "tags=uniq(?)" $ PGArray <$> tags]
    mkField sql (Just v) = Just (sql, toField v)
    mkField _   Nothing = Nothing

getPostsByTag :: Text -> Integer -> PostgresActionM [Post]
getPostsByTag tag pageNum = postgresQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where sql = "SELECT * FROM v_posts v WHERE ?=any(v.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPosts :: Integer -> PostgresActionM [Post]
getPosts pageNum = postgresQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_posts ORDER BY identifier DESC OFFSET ? LIMIT ?"

getDrafts :: User -> Integer -> PostgresActionM [Post]
getDrafts user pageNum = postgresQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_drafts WHERE (v_drafts.user).id=? ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPost :: Integer -> PostgresActionM (Maybe Post)
getPost pid = listToMaybe <$> postgresQuery sql [pid]
  where sql = "SELECT * FROM v_posts_all WHERE identifier=? LIMIT 1"

deletePost :: Integer -> User -> PostgresActionM (Maybe Integer)
deletePost pid (User uid _ _ _) = maybeQuery $ postgresQuery sql (pid, uid)
  where sql = "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier"
