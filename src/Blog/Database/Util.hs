{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Blog.Database.Util
(
  upsertPost,
  insertComment,
  getPostsByTag,
  getPosts,
  getDrafts,
  getPost,
  deletePost,
  getCommentsForPost,
  getUserWithUsername,
  
  webMQuery,
  webMQuery_
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

import Web.Scotty.Trans (ActionT, ScottyError)

import Data.Text (Text)
import Data.List (intercalate)

import Blog.Database.PGExtensions()

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG.Types
import Database.PostgreSQL.Simple.ToField as PG.ToField

import           Blaze.ByteString.Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

{-
Database TODO:

1. Rename columns
2. Dump & rebuild production DB

-}

upsertPost :: (ScottyError e) => User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> ActionT e WebM (Maybe Integer)
upsertPost user Nothing    title body tags draft = insertPost user title body tags draft
upsertPost user (Just pid) title body tags draft = updatePost user pid title body tags draft

insertPost :: (ScottyError e) => User -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> ActionT e WebM (Maybe Integer)
insertPost user title body tags draft = processResult $ webMQuery q fieldValues
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
                          mkField "tags" <$> tags]
      where
        mkField sql = (sql,) . toField

-- TODO: update timestamp??
updatePost :: (ScottyError e) => User -> Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> ActionT e WebM (Maybe Integer)
updatePost user pid title body tags draft = processResult $ webMQuery q (fvalues ++ [toField $ userUID user, toField pid])
  where
    q = "UPDATE blogposts SET " ++ (intercalate "," fsetters) ++ " WHERE author_id=? AND identifier=? RETURNING identifier"
    fsetters = map fst values
    fvalues = map snd values
    values :: [(String, Action)]
    values = catMaybes $ [Just ("is_draft=?", toField draft),
                          mkField "title=?" title,
                          mkField "bodyText=?" body,
                          mkField "tags=uniq(?)" tags]
    mkField sql (Just v ) = Just (sql, toField v)
    mkField _   Nothing = Nothing

getPostsByTag :: (ScottyError e) => Text -> Maybe Integer -> ActionT e WebM [Post]
getPostsByTag tag mPageNum = webMQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT * FROM v_posts v WHERE ?=any(v.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPosts :: (ScottyError e) => Maybe Integer -> ActionT e WebM [Post]
getPosts mPageNum = webMQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT * FROM v_posts ORDER BY identifier DESC OFFSET ? LIMIT ?"

getDrafts :: (ScottyError e) => User -> Maybe Integer -> ActionT e WebM [Post]
getDrafts user mPageNum = webMQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where
    pageNum = fromMaybe 0 mPageNum
    sql = "SELECT * FROM v_drafts WHERE (v_drafts.user).id=? ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPost :: (ScottyError e) => Integer -> ActionT e WebM (Maybe Post)
getPost pid = listToMaybe <$> webMQuery sql [pid]
  where sql = "SELECT * FROM v_posts_all WHERE identifier=? LIMIT 1"

deletePost :: (ScottyError e) => Integer -> User -> ActionT e WebM (Maybe Integer)
deletePost pid (User uid _ _ _) = processResult $ webMQuery sql (pid, uid)
  where sql = "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier"

-- TODO: use postgresql-simple's fold function instead of loading all posts into memory
getCommentsForPost :: (ScottyError e) => Integer -> ActionT e WebM [Comment]
getCommentsForPost pid = nestComments <$> webMQuery "SELECT * FROM comments WHERE postId=?" [pid]

insertComment :: (ScottyError e) => Maybe Integer -> Integer -> Text -> Text -> Text -> ActionT e WebM (Maybe Integer)
insertComment (Just parentId) pid email displayName body = processResult $ webMQuery "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING id" (parentId, pid, email, displayName, body)
insertComment Nothing         pid email displayName body = processResult $ webMQuery "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING id" (pid, email, displayName, body)

getUserWithUsername :: (ScottyError e) => Text -> ActionT e WebM (Maybe User)
getUserWithUsername username = listToMaybe <$> webMQuery "SELECT * FROM users WHERE username=? LIMIT 1" [username]

-- | Internal
webMQuery :: (ToRow q, FromRow r, ScottyError e) => String -> q -> ActionT e WebM [r]
webMQuery q ps = do
  pg <- webM $ gets statePostgres
  liftIO $ query pg (Query . toByteString . Utf8.fromString $ q) ps
  
webMQuery_ :: (ToRow q, ScottyError e) => String -> q -> ActionT e WebM ()
webMQuery_ q ps = void $ do
  pg <- webM $ gets statePostgres
  liftIO $ execute pg (stringToQuery q) ps

stringToQuery :: String -> Query
stringToQuery = Query . toByteString . Utf8.fromString

processResult :: ActionT e WebM [Only Integer] -> ActionT e WebM (Maybe Integer)
processResult res = (listToMaybe . map fromOnly) <$> res
