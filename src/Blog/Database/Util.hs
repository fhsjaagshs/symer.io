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

import Blog.Postgres
import Blog.User
import Blog.Comment
import Blog.Post

import Data.Maybe

import Data.Text (Text)
import Data.List (intercalate)

-- import Blog.Database.PGExtensions()

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG.Types
import Database.PostgreSQL.Simple.ToField as PG.ToField

{-
Database TODO:

1. Rename columns
2. Dump & rebuild production DB

-}

-- TODO: use postgresql-simple's fold function instead of loading all posts into memory

upsertPost :: User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
upsertPost user Nothing    title body tags draft = insertPost user title body tags draft
upsertPost user (Just pid) title body tags draft = updatePost user pid title body tags draft

insertPost :: User -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
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
                          (mkField "tags" . PGArray) <$> tags]
      where
        mkField sql = (sql,) . toField

-- TODO: update timestamp??
updatePost :: User -> Integer -> Maybe Text -> Maybe Text -> Maybe [Text] -> Bool -> PostgresActionM (Maybe Integer)
updatePost user pid title body tags draft = processResult $ webMQuery q (fvalues ++ [toField $ userUID user, toField pid])
  where
    q = "UPDATE blogposts SET " ++ (intercalate "," fsetters) ++ " WHERE author_id=? AND identifier=? RETURNING identifier"
    fsetters = map fst values
    fvalues = map snd values
    values :: [(String, Action)]
    values = catMaybes $ [Just ("is_draft=?", toField draft),
                          mkField "title=?" title,
                          mkField "bodyText=?" body,
                          mkField "tags=uniq(?)" $ PGArray <$> tags]
    mkField sql (Just v) = Just (sql, toField v)
    mkField _   Nothing = Nothing

getPostsByTag :: Text -> Integer -> PostgresActionM [Post]
getPostsByTag tag pageNum = webMQuery sql (tag,pageNum*(fromIntegral postsPerPage),postsPerPage+1)
  where sql = "SELECT * FROM v_posts v WHERE ?=any(v.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPosts :: Integer -> PostgresActionM [Post]
getPosts pageNum = webMQuery sql (pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_posts ORDER BY identifier DESC OFFSET ? LIMIT ?"

getDrafts :: User -> Integer -> PostgresActionM [Post]
getDrafts user pageNum = webMQuery sql (userUID user, pageNum*(fromIntegral postsPerPage), postsPerPage+1)
  where sql = "SELECT * FROM v_drafts WHERE (v_drafts.user).id=? ORDER BY identifier DESC OFFSET ? LIMIT ?"

getPost :: Integer -> PostgresActionM (Maybe Post)
getPost pid = listToMaybe <$> webMQuery sql [pid]
  where sql = "SELECT * FROM v_posts_all WHERE identifier=? LIMIT 1"

deletePost :: Integer -> User -> PostgresActionM (Maybe Integer)
deletePost pid (User uid _ _ _) = processResult $ webMQuery sql (pid, uid)
  where sql = "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier"

getCommentsForPost :: Integer -> PostgresActionM [Comment]
getCommentsForPost pid = nestComments <$> webMQuery "SELECT * FROM comments WHERE postId=?" [pid]

insertComment :: Maybe Integer -> Integer -> Text -> Text -> Text -> PostgresActionM (Maybe Integer)
insertComment (Just parentId) pid email displayName body = processResult $ webMQuery "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING id" (parentId, pid, email, displayName, body)
insertComment Nothing         pid email displayName body = processResult $ webMQuery "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING id" (pid, email, displayName, body)

getUserWithUsername :: Text -> PostgresActionM (Maybe User)
getUserWithUsername username = listToMaybe <$> webMQuery "SELECT * FROM users WHERE username=? LIMIT 1" [username]

-- | Internal

processResult :: PostgresActionM [Only a] -> PostgresActionM (Maybe a)
processResult res = (listToMaybe . map fromOnly) <$> res
