{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Blog.Database.Util
(
  runMigrations,
  upsertPost,
  insertComment,
  getPostsByTag,
  getPosts,
  getDrafts,
  getPost,
  deletePost,
  getCommentsForPost,
  getUserWithUsername
)
where

import Blog.Database.Config
import Blog.State
import Blog.User
import Blog.Comment
import Blog.Post

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import Web.Scotty.Trans

import Data.Text.Lazy (Text)

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Migration as PG.Migration
import Database.PostgreSQL.Simple.Types as PG.Types

import           Blaze.ByteString.Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

{-
Database TODO:

1. Rename columns
2. Look into views for blogposts

This will require migrating production:
1. Finalize migrations
2. Backup prod
3. Run migrations in prod
4. Condense migrations into one .sql file
5. Manually update prod accordingly

-}

runMigrations :: PG.Connection -> IO ()
runMigrations pg = do
  execute_ pg "SET client_min_messages=WARNING;"
  withTransaction pg $ runMigration $ MigrationContext MigrationInitialization True pg
  execute_ pg "SET client_min_messages=NOTICE;"
  withTransaction pg $ do
    forM_ migrations $ \(f, p) -> do
      runMigration $ MigrationContext (MigrationFile f p) True pg

-- TODO: only update if user is correct
upsertPost :: (ScottyError e) => User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [String] -> Maybe [String] -> Bool -> ActionT e WebM (Maybe Integer)
upsertPost _    (Just identifier_) Nothing       Nothing      Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET is_draft=? WHERE identifier=? RETURNING identifier" (isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) Nothing      Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) (Just body_) Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) (Just body_) (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) (Just body_) (Just tags_) (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, deletedTags_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       (Just body_) Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (body_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       Nothing      (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (tags_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       (Just body_) (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, tags_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) Nothing      (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, tags_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       Nothing      (Just tags_) (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=uniq_cat(array_diff(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, tags_, isdraft, identifier_)
upsertPost _    (Just identifier_) Nothing       (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, deletedTags_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, deletedTags_, isdraft, identifier_)
upsertPost _    (Just identifier_) (Just title_) (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, deletedTags_, isdraft, identifier_)
upsertPost user Nothing            (Just title_) (Just body_) Nothing      _                   isdraft = processResult $ webMQuery "INSERT INTO blogposts (title, bodyText, author_id, is_draft) VALUES (?, ?, ?, ?) RETURNING identifier" (title_, body_, userUID user, isdraft)
upsertPost user Nothing            (Just title_) (Just body_) (Just tags_) _                   isdraft = processResult $ webMQuery "INSERT INTO blogposts (title, bodyText, tags, author_id, is_draft) VALUES (?, ?, ?, ?, ?) RETURNING identifier" (title_, body_, tags_, userUID user, isdraft)
upsertPost _    _                 _            _            _           _                      _       = return Nothing

getPostsByTag :: (ScottyError e) => Text -> Maybe Integer -> ActionT e WebM [Post]
getPostsByTag tag mPageNum = webMQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id AND ?=any(b.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPosts :: (ScottyError e) => Maybe Integer -> ActionT e WebM [Post]
getPosts mPageNum = webMQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?"

getDrafts :: (ScottyError e) => User -> Maybe Integer -> ActionT e WebM [Post]
getDrafts user mPageNum = webMQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u FROM blogposts b, users u WHERE is_draft='t'::bool AND b.author_id=? AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPost :: (ScottyError e) => Integer -> ActionT e WebM (Maybe Post)
getPost identifier_ = listToMaybe <$> webMQuery sql [identifier_]
  where sql = "SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u FROM blogposts b, users u WHERE u.id=b.author_id AND identifier=? LIMIT 1"

deletePost :: (ScottyError e) => Integer -> User -> ActionT e WebM (Maybe Integer)
deletePost identifier_ (User uid_ _ _ _) = processResult $ webMQuery sql (identifier_, uid_)
  where sql = "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier"

getCommentsForPost :: (ScottyError e) => Integer -> ActionT e WebM [Comment]
getCommentsForPost identifier_ = webMQuery "SELECT * FROM comments WHERE postId=?" [identifier_]

insertComment :: (ScottyError e) => Maybe Integer -> Integer -> Text -> Text -> Text -> ActionT e WebM (Maybe Integer)
insertComment (Just parentId_) postId_ email_ displayName_ body_ = processResult $ webMQuery "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING id" (parentId_, postId_, email_, displayName_, body_)
insertComment Nothing          postId_ email_ displayName_ body_ = processResult $ webMQuery "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING id" (postId_, email_, displayName_, body_)

getUserWithUsername :: (ScottyError e) => Text -> ActionT e WebM (Maybe User)
getUserWithUsername username = listToMaybe <$> webMQuery "SELECT * FROM users WHERE username=? LIMIT 1" [username]

-- | Internal
webMQuery :: (ToRow q, FromRow r, ScottyError e) => String -> q -> ActionT e WebM [r]
webMQuery q ps = do
  pg <- webM $ gets statePostgres
  liftIO $ query pg (Query . toByteString . Utf8.fromString $ q) ps
  
processResult :: ActionT e WebM [Only Integer] -> ActionT e WebM (Maybe Integer)
processResult res = (listToMaybe . map fromOnly) <$> res
