module Blog.Database.Util
(
  runMigrations,
  upsertBlogPost,
  insertComment,
  getBlogPostsByTag,
  getBlogPosts,
  getDrafts,
  getBlogPost,
  deleteBlogPost
)
where

import Blog.Types
import Blog.Database.Config

import Data.Text (Text)

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.Migration as PG.Migration

runMigrations :: PG.Connection -> IO ()
runMigrations pg = do
  execute_ pg "SET client_min_messages=WARNING;"
  withTransaction pg $ runMigration $ MigrationContext MigrationInitialization True pg
  execute_ pg "SET client_min_messages=NOTICE;"
  withTransaction pg $ do
    forM_ migrations $ \(f, p) -> do
      runMigration $ MigrationContext (MigrationFile f p) True pg

-- TODO: only update if user is correct
upsertBlogPost :: PG.Connection -> User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [String] -> Maybe [String] -> Bool -> IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      Nothing      Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET is_draft=? WHERE identifier=? RETURNING identifier" (isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      Nothing      Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET title=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) Nothing      Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET title=?, bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) (Just tags_) Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) (Just tags_) (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, deletedTags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) Nothing      Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (body_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      (Just tags_) Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (tags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) (Just tags_) Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, tags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      (Just tags_) Nothing             isdraft = processResult $ query pg "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, tags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      (Just tags_) (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET tags=uniq_cat(array_diff(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, tags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, deletedTags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET title=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, deletedTags_, isdraft, identifier_)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, deletedTags_, isdraft, identifier_)
upsertBlogPost pg user Nothing            (Just title_) (Just body_) Nothing      _                   isdraft = processResult $ query pg "INSERT INTO blogposts (title, bodyText, author_id, is_draft) VALUES (?, ?, ?, ?) RETURNING identifier" (title_, body_, Types.uid user, isdraft)
upsertBlogPost pg user Nothing            (Just title_) (Just body_) (Just tags_) _                   isdraft = processResult $ query pg "INSERT INTO blogposts (title, bodyText, tags, author_id, is_draft) VALUES (?, ?, ?, ?, ?) RETURNING identifier" (title_, body_, tags_, Types.uid user, isdraft)
upsertBlogPost _  _    _                 _            _            _           _                      _       = return Nothing

insertComment :: PG.Connection -> Maybe Integer -> Integer -> Text -> Text -> Text -> IO (Maybe Integer)
insertComment pg (Just parentId_) postId_ email_ displayName_ body_ = processResult $ query pg "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING id" (parentId_, postId_, email_, displayName_, body_)
insertComment pg Nothing          postId_ email_ displayName_ body_ = processResult $ query pg "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING id" (postId_, email_, displayName_, body_)

getBlogPostsByTag :: PG.Connection -> Text -> Maybe Integer -> IO [BlogPost]
getBlogPostsByTag pg tag Nothing        = getBlogPostsByTag pg tag (Just 1)
getBlogPostsByTag pg tag (Just pageNum) = query pg "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id AND ?=any(b.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?" (tag,(pageNum-1)*(fromIntegral postsPerPage),postsPerPage+1)

getBlogPosts :: PG.Connection -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg (Just pageNum) = query pg "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?" ((pageNum-1)*(fromIntegral postsPerPage), postsPerPage+1)
getBlogPosts pg Nothing        = getBlogPosts pg (Just 1)

getDrafts :: PG.Connection -> User -> Maybe Integer -> IO [BlogPost]
getDrafts pg user (Just pageNum) = query pg "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='t'::bool AND b.author_id=? AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?" (Types.uid user, (pageNum-1)*(fromIntegral postsPerPage), postsPerPage+1)
getDrafts pg user Nothing        = getDrafts pg user (Just 1)

getBlogPost :: PG.Connection -> Integer -> IO (Maybe BlogPost)
getBlogPost pg identifier_ = listToMaybe <$> query pg "SELECT b.*,u FROM blogposts b, users u WHERE u.id=b.author_id AND identifier=? LIMIT 1" [identifier_ :: Integer]

deleteBlogPost :: PG.Connection -> Integer -> User -> IO (Maybe Integer)
deleteBlogPost pg identifier_ (User uid_ _ _ _) = listToMaybe <$> map fromOnly <$> ((query pg "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier" (identifier_, uid_)) :: IO [Only Integer])


-- | Internal

processResult :: IO [Only Integer] -> IO (Maybe Integer)
processResult res = listToMaybe <$> map fromOnly <$> res
