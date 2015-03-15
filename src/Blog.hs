{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main (main) where

import           Auth
import           Config
import           Types
import           Helpers

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Maybe

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.List as List

import           Web.Scotty as Scotty
import           Network.HTTP.Types.Status
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.Migration as PG.Migration

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Prelude as P hiding (head, id, div)

import           Magic -- for mimetypes

-- TODO:
-- 1. Finish pages (procrastinateable)
--    a. not found
--    b. unauthorized
-- 2. Page numbers at bottom (would require extra request)
-- 4. drafts
-- 5. blog post author
-- 6. Site footer (copyright etc)

-- Nitpick todo:
-- 1. fix timezone
-- 2. Rewrite using a state monad?
-- 4. cache minification results

-- Miscellania:
-- 1. 'Top 5' tags map in side bar?
    
main :: IO ()
main = do
  port <- read <$> Helpers.safeGetEnv "PORT" "3000"
  scotty port $ do
    liftIO $ putStrLn "--| establishing database connections"
    pg <- liftIO $ PG.connectPostgreSQL $ B.pack $ Config.postgresConnStr
    redis <- liftIO $ Redis.connect Redis.defaultConnectInfo
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic

    liftIO $ putStrLn ("--| starting blog: " ++ unsafeGetEnv "ENVIRONMENT" "development")
    liftIO $ putStrLn "--| running database migrations"
    liftIO $ execute_ pg "SET client_min_messages=WARNING;"
    liftIO $ withTransaction pg $ runMigration $ MigrationContext MigrationInitialization True pg
    liftIO $ execute_ pg "SET client_min_messages=NOTICE;"
    liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "blog.sql" "./migrations/blog.sql") True pg
    liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "array_funcs.sql" "./migrations/array_funcs.sql") True pg
    liftIO $ putStrLn "--| clearing cached data from Redis"
    liftIO $ Redis.runRedis redis $ Redis.flushall
    liftIO $ putStrLn "--| done |--"
  
    -- shortcuts for auth
    let protected = checkAuth redis
    let authenticatedUser = getUser redis
    
    -- 
    -- Posts
    -- These are HTML pages
    --
    
    -- blog root
    get "/" $ do
      maybeUser <- authenticatedUser
      maybePNum <- maybeParam "page"
      let mPageNum = ((read . T.unpack . TL.toStrict <$> maybePNum)) :: Maybe Integer
      posts <- liftIO $ getBlogPosts pg mPageNum
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead [] [] blogTitle
        renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
          render (take postsPerPage posts) maybeUser
          renderPageControls mPageNum (length posts > postsPerPage)
          
    -- create a post
    get "/posts/new" $ do
      protected
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] $ appendedBlogTitle "New Post"
        renderBody Nothing Nothing Nothing $ do
          renderPostEditor Nothing
    
    -- view a specific post
    get "/posts/:id" $ do
      identifier_ <- param "id"
      maybeUser <- authenticatedUser
      res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier_ :: Integer]
      case res of
        Nothing -> next
        Just post_ -> do
          Scotty.html $ R.renderHtml $ docTypeHtml $ do
            renderHead [] (postTags $ Types.tags post_) $ appendedBlogTitle $ Types.title post_
            renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
              render post_ maybeUser
    
    get "/posts/by/tag/:tag" $ do
      tag <- param "tag"
      maybeUser <- authenticatedUser
      maybePNum <- maybeParam "page"
      let mPageNum = ((read . T.unpack . TL.toStrict <$> maybePNum)) :: Maybe Integer
      posts <- liftIO $ getBlogPostsByTag pg tag mPageNum
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead [] [] (appendedBlogTitle $ tag)
        renderBody (Just $ T.append "Posts tagged '" $ T.append tag "'") Nothing maybeUser $ do
          -- render (posts :: [BlogPost]) maybeUser
          render (take postsPerPage posts) maybeUser
          renderPageControls mPageNum (length posts > postsPerPage)
  
    -- edit a post
    get "/posts/:id/edit" $ do
      protected
      identifier_ <- param "id"
      res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier_ :: Integer]
      case res of
        Nothing -> next
        Just post_ -> do
          Scotty.html $ R.renderHtml $ docTypeHtml $ do
            renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] (appendedBlogTitle $ Types.title post_)
            renderBody Nothing Nothing Nothing $ do
              renderPostEditor $ Just post_
              
    get "/login" $ do
      maybeUser <- (getUser redis)
      when (isJust maybeUser) (redirect "/")
      maybeErrorMessage <- (rescue (Just <$> param "error_message") (\_ -> return Nothing))
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead ["/assets/css/login.css"] [("robots","noindex, nofollow")] $ appendedBlogTitle "Login"
        renderBody (Just "Login") maybeErrorMessage Nothing $ do
          H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
            input ! type_ "hidden" ! A.name "source" ! value "form"
            renderInput "text" ! A.id "username" ! placeholder "Username" ! A.name "username" ! onkeydown "if(event.keyCode==13)document.getElementById('password').focus()"
            renderInput "password" ! A.id "password" ! placeholder "Password" ! A.name "password" ! onkeydown "if(event.keyCode==13)document.getElementById('submit').click()"
          a ! A.id "submit" ! onclick "document.getElementById('loginform').submit();" ! class_ "blogbutton" ! rel "nofollow" $ "Login"
  
    get "/logout" $ do
      atoken <- accessToken
      liftIO $ Auth.deleteObject redis (T.encodeUtf8 <$> atoken)
      redirect "/"
  
    post "/login" $ do
      pUsername <- param "username" :: ActionM T.Text
      pPassword <- param "password" :: ActionM T.Text
  
      res <- liftIO $ listToMaybe <$> (query pg "SELECT * FROM users WHERE username=? LIMIT 1" [pUsername] :: IO [User])
      
      case res of
        Nothing     -> redirect "/login?error_message=Username%20not%20found%2E"
        (Just user) -> do
          if Helpers.checkPassword (Types.passwordHash user) pPassword
            then do
              token <- liftIO $ Auth.saveObject redis user
              setAccessToken (T.decodeUtf8 token)
              redirect "/"
            else redirect "/login?error_message=Invalid%20password%2E"
        
    matchAny "/unauthorized" $ do
      status $ Status 401 "Unauthorized"
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead [] [("robots","noindex, nofollow")] (appendedBlogTitle "Unauthorized")
        renderBody (Just "Unauthorized") Nothing Nothing $ do
          h1 $ "Authorization is required beyond this point."
  
    -- deletes a BlogPost from the database
    -- returns JSON
    delete "/posts/:id" $ do
      protected
      identifier_ <- param "id"
      res <- liftIO $ listToMaybe <$> query pg "DELETE FROM blogposts WHERE identifier=? RETURNING *" [identifier_ :: Integer]
      case res of
        Nothing -> emptyResponse
        Just bp -> Scotty.json (bp :: BlogPost)
  
    -- creates/updates a BlogPost in the database
    -- returns JSON
    post "/posts" $ do
      protected
      ps <- params
      maybeBlogPost <- liftIO $ (upsertBlogPost pg
                                               (((read . TL.unpack) <$> lookup "id" ps) :: Maybe Integer)
                                               (TL.toStrict <$> lookup "title" ps)
                                               (TL.toStrict <$> lookup "body" ps)
                                               (((splitList ',') . TL.unpack) <$> (lookup "tags" ps))
                                               (((splitList ',') . TL.unpack) <$> (lookup "deleted_tags" ps))
                                              )
      case maybeBlogPost of
        Nothing -> emptyResponse
        Just bp -> do
          addHeader "Location" $ TL.pack $ "/posts/" ++ (show $ Types.identifier bp)
          Scotty.json (bp :: BlogPost)
  
    -- returns minified JS
    get "/assets/js/:filename" $ do
      filename <- param "filename"
      setHeader "Content-Type" "text/javascript"
      setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour
      cachedText redis filename $ do
        js <- liftIO $ BL.readFile $ "assets/js/" ++ (TL.unpack filename)
        return $ (if (List.isInfixOf ".min." $ TL.unpack filename) then js else (Helpers.minifyJS js))

    -- returns minified CSS
    get "/assets/css/:filename" $ do
      filename <- param "filename"
      setHeader "Content-Type" "text/css"
      setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour
      cachedText redis filename $ do
        css <- liftIO $ BL.readFile $ "assets/css/" ++ (TL.unpack filename)
        return (if (List.isInfixOf ".min." $ TL.unpack filename) then css else (Helpers.minifyCSS $ BL.toStrict css))
      
    get (regex "/(assets/.*)") $ do
      relPath <- param "1"
      mimeType <- liftIO $ TL.pack <$> magicFile magic relPath
      setHeader "Content-Type" mimeType
      setHeader "Cache-Control" "public, max-age=604800, s-max-age=604800, no-transform" -- one week
      Scotty.file relPath
  
    notFound $ Scotty.html $ R.renderHtml $ docTypeHtml $ h1 $ toHtml $ ("Not Found." :: T.Text)

-------------------------------------------------------------------------------
--- | Constants

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: T.Text
blogSubtitle = "a blog about code."

postsPerPage :: Int
postsPerPage = 10

seoTags :: [(T.Text, T.Text)]
seoTags = [
            ("revisit-after", "2 days"),
            ("description", "Rants and raves about computer science, functional programming, politics, and everything in between."),
            ("keywords", "computer science, politics, haskell, ruby, web development, art, blogs, money, computers, startups, tutorial, rails, ruby on rails, scotty haskell, snap framework")
            ]
            
-------------------------------------------------------------------------------
--- | DRY Rendering

renderHead :: [T.Text] -> [(T.Text, T.Text)] -> T.Text -> Html
renderHead cssFiles metaTags title_ = H.head $ do
  H.title $ toHtml title_
  renderCssLinks $ cssFiles ++ ["/assets/css/blog.css"]
  script ! src "/assets/js/jquery-2.1.3.min.js" $ ""
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  renderTags $ List.nubBy (\(keyOne, _) (keyTwo, _) -> keyOne == keyTwo) $ metaTags ++ seoTags

renderBody :: Maybe T.Text -> Maybe T.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (textValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! A.id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! A.id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) (a ! href "/logout" ! class_ "blogbutton" ! rel "nofollow" $ "Logout")
  when (isJust maybeUser) (a ! href "/posts/new" ! class_ "blogbutton" ! rel "nofollow" $ "New Post")
  div ! A.id "content" $ bodyHtml
  
renderMdEditor :: Maybe BlogPost -> Html
renderMdEditor Nothing = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" $ ""
renderMdEditor (Just (BlogPost identifier_ _ body_ _ _)) = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" ! customAttribute "post-id" (stringValue $ show identifier_) $ toHtml body_
      
renderTitleField :: Maybe BlogPost -> Html
renderTitleField (Just (BlogPost _ title_ _ _ _)) = input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue title_)
renderTitleField Nothing = input ! type_ "text" ! id "title-field" ! placeholder "Post title"

renderTagEditor :: Maybe BlogPost -> Html
renderTagEditor Nothing = textarea ! A.id "tags" ! class_ "wordlist" $ do ""
renderTagEditor (Just (BlogPost _ _ _ _ tags_)) = textarea ! A.id "tags" ! class_ "wordlist" $ do toHtml $ List.intercalate ", " tags_
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  renderTitleField maybeBlogPost
  renderMdEditor maybeBlogPost
  renderTagEditor maybeBlogPost
  
  a ! A.id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! A.id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! A.id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"

  script ! src "/assets/js/marked.min.js" $ ""
  script ! src "/assets/js/wordlist.js" $ ""
  script ! src "/assets/js/editor.js" $ ""
  
renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls Nothing hasNext = renderPageControls (Just 1) hasNext
renderPageControls (Just pageNum) hasNext = do
  when (pageNum > 2) (a ! A.id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum-1)) $ "Newer")
  when (pageNum == 2) (a ! A.id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/") $ "Newer")
  when hasNext (a ! A.id "nextbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum+1)) $ "Older")

-------------------------------------------------------------------------------
--- | Specialized Helpers

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle s = T.append s (T.append " | " blogTitle)

postTags :: [String] -> [(T.Text, T.Text)]
postTags ts = [("keywords", T.append (T.pack $ (List.intercalate ", " ts) ++ ", ") (fromJust $ lookup "keywords" seoTags))]

-------------------------------------------------------------------------------
--- | Authentication
  
getUser :: Redis.Connection -> ActionM (Maybe User)
getUser redis = do
  atoken <- Helpers.accessToken
  liftIO $ Auth.refreshObject redis (T.encodeUtf8 <$> atoken)
  maybeUser <- liftIO $ Auth.getObject redis (T.encodeUtf8 <$> atoken)
  return maybeUser
  
checkAuth :: Redis.Connection -> ActionM (Maybe User)
checkAuth redis = do
  maybeUser <- (getUser redis)
  case maybeUser of
    (Just user) -> return $ Just user
    _ -> do
      return Nothing
      redirect "/unauthorized"

-------------------------------------------------------------------------------
--- | Caching

cachedText :: Redis.Connection -> TL.Text -> ActionM BL.ByteString -> ActionM ()
cachedText redis key valueFunc = do
  env <- liftIO $ safeGetEnv "ENVIRONMENT" "development"
  if env == "production"
    then do
      redisValue <- liftIO $ Redis.runRedis redis $ Redis.get $ BL.toStrict $ TL.encodeUtf8 key
      case redisValue of
        Right (Just cached) -> do
          addHeader "ETag" (TL.decodeUtf8 $ md5Sum $ BL.fromStrict $ cached)
          Scotty.raw $ BL.fromStrict $ cached
        _ -> do
          v <- valueFunc
          addHeader "ETag" (TL.decodeUtf8 $ md5Sum v)
          liftIO $ Redis.runRedis redis $ do
            Redis.set (BL.toStrict $ TL.encodeUtf8 key) (BL.toStrict v)
            Redis.expire (BL.toStrict $ TL.encodeUtf8 key) 3600
          Scotty.raw v
    else do
      v <- valueFunc
      addHeader "ETag" (TL.decodeUtf8 $ md5Sum v)
      Scotty.raw v
      -- valueFunc >>= \v -> Scotty.raw v
  return ()

-------------------------------------------------------------------------------
--- | Database

upsertBlogPost :: PG.Connection -> Maybe Integer -> Maybe T.Text -> Maybe T.Text -> Maybe [String] -> Maybe [String] -> IO (Maybe BlogPost)
upsertBlogPost pg (Just identifier_) Nothing       Nothing      Nothing      Nothing             = listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier_]
upsertBlogPost pg (Just identifier_) (Just title_) Nothing      Nothing      Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET title=? WHERE identifier=? RETURNING *" (title_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) (Just body_) Nothing      Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=? WHERE identifier=? RETURNING *" (title_, body_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) (Just body_) (Just tags_) Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?)  WHERE identifier=? RETURNING *" (title_, body_, tags_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) (Just body_) (Just tags_) (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags, ?),?) WHERE identifier = ? RETURNING *" (title_, body_, tags_, deletedTags_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       (Just body_) Nothing      Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET bodyText=? WHERE identifier=? RETURNING *" (body_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       Nothing      (Just tags_) Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET tags=uniq_cat(tags,?) WHERE identifier=? RETURNING *" (tags_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       (Just body_) (Just tags_) Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET bodyText=?, tags=uniq_cat(tags,?) WHERE identifier=? RETURNING *" (body_, tags_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) Nothing      (Just tags_) Nothing             = listToMaybe <$> query pg "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?) WHERE identifier=? RETURNING *" (title_, tags_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       Nothing      Nothing      (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (deletedTags_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       Nothing      (Just tags_) (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET tags=uniq_cat(array_diff(tags,?),?) WHERE identifier=? RETURNING *" (deletedTags_, tags_, identifier_)
upsertBlogPost pg (Just identifier_) Nothing       (Just body_) Nothing      (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET bodyText=?, tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (body_, deletedTags_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) Nothing      Nothing      (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (title_, deletedTags_, identifier_)
upsertBlogPost pg (Just identifier_) (Just title_) (Just body_) Nothing      (Just deletedTags_) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (title_, body_, deletedTags_, identifier_)
upsertBlogPost pg Nothing            (Just title_) (Just body_) Nothing      _                   = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" (title_, body_)
upsertBlogPost pg Nothing            (Just title_) (Just body_) (Just tags_) _                   = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText, tags) VALUES (?, ?, ?) RETURNING *" (title_, body_, tags_)
upsertBlogPost _  _                 _            _            _           _                      = return Nothing

getBlogPostsByTag :: PG.Connection -> T.Text -> Maybe Integer -> IO [BlogPost]
getBlogPostsByTag pg tag Nothing        = getBlogPostsByTag pg tag (Just 1)
getBlogPostsByTag pg tag (Just pageNum) = query pg "SELECT * FROM blogposts WHERE ?=any(tags) ORDER BY identifier DESC OFFSET ? LIMIT ?" (tag,(pageNum-1)*(fromIntegral postsPerPage),postsPerPage+1)

getBlogPosts :: PG.Connection -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg (Just pageNum) = query pg "SELECT * FROM blogposts ORDER BY identifier DESC OFFSET ? LIMIT ?" ((pageNum-1)*(fromIntegral postsPerPage), postsPerPage+1)
getBlogPosts pg Nothing        = getBlogPosts pg (Just 1)
