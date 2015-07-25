module Blog.Database.Util
(
  runMigrations,
  upsertBlogPost,
  insertComment,
  getBlogPostsByTag,
  getBlogPosts,
  getDrafts,
  getBlogPost,
  deleteBlogPost,
  getCommentsForPost,
  getUserWithUsername
)
where

import Blog.Types
import Blog.Database.Config
import Blog.State

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
upsertBlogPost :: User -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe [String] -> Maybe [String] -> Bool -> ScottyT e WebM (Maybe Integer)
upsertBlogPost _    (Just identifier_) Nothing       Nothing      Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET is_draft=? WHERE identifier=? RETURNING identifier" (isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) Nothing      Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) (Just body_) Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) (Just body_) (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) (Just body_) (Just tags_) (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, deletedTags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       (Just body_) Nothing      Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (body_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       Nothing      (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (tags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       (Just body_) (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, tags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) Nothing      (Just tags_) Nothing             isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, tags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       Nothing      (Just tags_) (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET tags=uniq_cat(array_diff(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, tags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) Nothing       (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, deletedTags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) Nothing      Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, deletedTags_, isdraft, identifier_)
upsertBlogPost _    (Just identifier_) (Just title_) (Just body_) Nothing      (Just deletedTags_) isdraft = processResult $ webMQuery "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, deletedTags_, isdraft, identifier_)
upsertBlogPost user Nothing            (Just title_) (Just body_) Nothing      _                   isdraft = processResult $ webMQuery "INSERT INTO blogposts (title, bodyText, author_id, is_draft) VALUES (?, ?, ?, ?) RETURNING identifier" (title_, body_, Types.uid user, isdraft)
upsertBlogPost user Nothing            (Just title_) (Just body_) (Just tags_) _                   isdraft = processResult $ webMQuery "INSERT INTO blogposts (title, bodyText, tags, author_id, is_draft) VALUES (?, ?, ?, ?, ?) RETURNING identifier" (title_, body_, tags_, Types.uid user, isdraft)
upsertBlogPost _    _                 _            _            _           _                      _       = return Nothing

insertComment :: Maybe Integer -> Integer -> Text -> Text -> Text -> ScottyT e WebM (Maybe Integer)
insertComment (Just parentId_) postId_ email_ displayName_ body_ = processResult $ webMQuery "INSERT INTO comments (parentId,postId,email,displayName,body) VALUES (?,?,?,?,?) RETURNING id" (parentId_, postId_, email_, displayName_, body_)
insertComment Nothing          postId_ email_ displayName_ body_ = processResult $ webMQuery "INSERT INTO comments (postId,email,displayName,body) VALUES (?,?,?,?) RETURNING id" (postId_, email_, displayName_, body_)

getBlogPostsByTag :: Text -> Maybe Integer -> ScottyT e WebM [BlogPost]
getBlogPostsByTag tag Nothing        = getBlogPostsByTag tag (Just 1)
getBlogPostsByTag tag (Just pageNum) = webMQuery "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id AND ?=any(b.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?" (tag,(pageNum-1)*(fromIntegral postsPerPage),postsPerPage+1)

getBlogPosts :: Maybe Integer -> ScottyT e WebM [BlogPost]
getBlogPosts Nothing        = getBlogPosts (Just 1)
getBlogPosts (Just pageNum) = webMQuery "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='f'::bool AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?" ((pageNum-1)*(fromIntegral postsPerPage), postsPerPage+1)

getDrafts :: User -> Maybe Integer -> ScottyT e WebM [BlogPost]
getDrafts user (Just pageNum) = webMQuery "SELECT b.*,u FROM blogposts b, users u WHERE is_draft='t'::bool AND b.author_id=? AND u.id=b.author_id ORDER BY identifier DESC OFFSET ? LIMIT ?" (Types.uid user, (pageNum-1)*(fromIntegral postsPerPage), postsPerPage+1)
getDrafts user Nothing        = getDrafts user (Just 1)

getBlogPost :: Integer -> ScottyT e WebM (Maybe BlogPost)
getBlogPost identifier_ = listToMaybe <$> webMQuery "SELECT b.*,u FROM blogposts b, users u WHERE u.id=b.author_id AND identifier=? LIMIT 1" [identifier_]

deleteBlogPost :: Integer -> User -> ScottyT e WebM (Maybe Integer)
deleteBlogPost identifier_ (User uid_ _ _ _) = processResult $ webMQuery "DELETE FROM blogposts WHERE identifier=? AND author_id=? RETURNING identifier" (identifier_, uid_)

getCommentsForPost :: Integer -> ScottyT e WebM [Comment]
getCommentsForPost identifier_ = webMQuery "SELECT * FROM comments WHERE postId=?" [identifier_]

getUserWithUsername :: Text -> ScottyT e WebM (Maybe User)
getUserWithUsername username = processResult $ webMQuery "SELECT * FROM users WHERE username=? LIMIT 1" [pUsername]

-- | Internal

webMQuery :: (PG.QueryParams q, PG.QueryResults r) => PG.Query -> q -> ScottyT e WebM [r]
webMQuery q params = do
  pg <- webM $ gets statePostgres
  query pg q params
  
processResult :: ScottyT e WebM [Only Integer] -> ScottyT e WebM (Maybe Integer)
processResult res = listToMaybe <$> map fromOnly <$> res
