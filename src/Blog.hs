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
import           Prelude as P hiding (head, div)

import           Magic -- for mimetypes

-- TODO:
-- comments (id, thread_id, parent_id, body, email, display_name)
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- drafts button
-- Save MD5 sum in redis

-- Miscellaneous Ideas:
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
    liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "authorship.sql" "./migrations/authorship.sql") True pg
    liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "drafts.sql" "./migrations/drafts.sql") True pg
    liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "comments.sql" "./migrations/comments.sql") True pg
    liftIO $ putStrLn "--| clearing cached data from Redis"
    liftIO $ Redis.runRedis redis $ Redis.flushall
    liftIO $ putStrLn "--| blog started"
  
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
          
    get "/drafts" $ do
      maybeUser <- protected
      maybePNum <- maybeParam "page"
      let mPageNum = ((read . T.unpack . TL.toStrict <$> maybePNum)) :: Maybe Integer
      posts <- liftIO $ getDrafts pg (fromJust maybeUser) mPageNum
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead [] [("robots","noindex, nofollow")] (appendedBlogTitle "Drafts")
        renderBody (Just "Drafts") Nothing maybeUser $ do
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
      res <- liftIO $ getBlogPost pg identifier_
      case res of
        Nothing -> next
        Just post_ -> do
          case maybeUser of
            Just user -> unless ((Types.author post_) == user) (redirect "/notfound")
            Nothing -> when (Types.isDraft post_) (redirect "/notfound")

          Scotty.html $ R.renderHtml $ docTypeHtml $ do
            renderHead ["/assets/css/post.css"] (postTags $ Types.tags post_) $ appendedBlogTitle $ Types.title post_
            renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
              render post_ maybeUser
              hr ! class_ "separator"
              div ! A.id "comments" $ ""
              script ! src "/assets/js/md5.js" $ ""
              script ! src "/assets/js/common.js" $ ""
              script ! src "/assets/js/post.js" $ ""
              
    get "/posts/by/tag/:tag" $ do
      tag <- param "tag"
      maybeUser <- authenticatedUser
      maybePNum <- maybeParam "page"
      let mPageNum = ((read . T.unpack . TL.toStrict <$> maybePNum)) :: Maybe Integer
      posts <- liftIO $ getBlogPostsByTag pg tag mPageNum
      Scotty.html $ R.renderHtml $ docTypeHtml $ do
        renderHead [] [] (appendedBlogTitle $ tag)
        renderBody (Just $ T.append "Posts tagged '" $ T.append tag "'") Nothing maybeUser $ do
          render (take postsPerPage posts) maybeUser
          renderPageControls mPageNum (length posts > postsPerPage)
  
    -- edit a post
    get "/posts/:id/edit" $ do
      protected
      identifier_ <- param "id"
      res <- liftIO $ getBlogPost pg identifier_
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
      
    --
    -- JSON pages
    --
    
    get "/posts/:id/comments.json" $ do
      identifier_ <- param "id"
      (liftIO $ query pg "SELECT * FROM comments WHERE postId=?" [identifier_ :: Integer]) >>= Scotty.json . nestComments . (map (\c -> Node c [] Nothing))
    
    --
    -- Non-Pages
    --
  
    post "/login" $ do
      pUsername <- param "username" :: ActionM T.Text
      pPassword <- param "password" :: ActionM T.Text
      pRedirectPath <- maybeParam "redirect" :: ActionM (Maybe TL.Text)
  
      res <- liftIO $ listToMaybe <$> (query pg "SELECT * FROM users WHERE username=? LIMIT 1" [pUsername] :: IO [User])
      
      case res of
        Nothing     -> redirect "/login?error_message=Username%20not%20found%2E"
        (Just user) -> do
          if Helpers.checkPassword (fromJust $ Types.passwordHash user) pPassword
            then do
              token <- liftIO $ Auth.saveObject redis user
              setAccessToken (T.decodeUtf8 token)
              redirect $ Data.Maybe.maybe "/" P.id pRedirectPath
            else redirect "/login?error_message=Invalid%20password%2E"

    -- deletes a BlogPost from the database
    -- returns JSON
    delete "/posts/:id" $ do
      authUser <- protected
      identifier_ <- param "id"
      res <- liftIO $ deleteBlogPost pg identifier_ (fromJust authUser)
      case (res :: Maybe Integer) of
        Nothing -> do
          status $ Status 404 "blog post not found."
        Just _ -> do
          Scotty.text "ok"  

    -- creates/updates a BlogPost in the database
    -- returns JSON
    post "/posts" $ do
      authUser <- protected
      ps <- params

      maybeBPIdentifier <- liftIO $ (upsertBlogPost pg
                                                    (fromJust authUser)
                                                    (((read . TL.unpack) <$> lookup "id" ps) :: Maybe Integer)
                                                    (TL.toStrict <$> lookup "title" ps)
                                                    (TL.toStrict <$> lookup "body" ps)
                                                    (((splitList ',') . TL.unpack) <$> (lookup "tags" ps))
                                                    (((splitList ',') . TL.unpack) <$> (lookup "deleted_tags" ps))
                                                    (elem (TL.unpack $ maybe "True" P.id $ lookup "draft" ps) ["t", "true", "True", "y", "yes"])
                                                   )
      case maybeBPIdentifier of
        Nothing -> do
          status $ Status 400 "Missing required parameters"
          emptyResponse
        Just identifier_ -> do
          addHeader "Location" $ TL.pack $ "/posts/" ++ (show identifier_)
          emptyResponse
  
    post "/posts/:id/comments" $ do
      identifier_ <- param "post_id"
      email <- param "email"
      cdn <- param "display_name"
      commentBody <- param "body"
      parentId <- maybeParam "parent_id"
      liftIO $ ((query pg "INSERT INTO comments (postId, email, commentDisplayName, body) VALUES (?,?,?,?)" (identifier_ :: Integer, (email :: String), cdn :: String, commentBody :: String, (maybe "NULL" P.id parentId) :: TL.Text)) :: IO [Comment])
      addHeader "Location" $ TL.pack $ "/posts/" ++ (show identifier_)
      emptyResponse
  
    -- returns minified JS
    get "/assets/js/:filename" $ do
      filename <- param "filename"
      setHeader "Content-Type" "text/javascript"
      setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour
      cachedBody redis filename $ do
        js <- liftIO $ BL.readFile $ "assets/js/" ++ (TL.unpack filename)
        return (if (List.isInfixOf ".min." $ TL.unpack filename) then js else (Helpers.minifyJS js))

    -- returns minified CSS
    get "/assets/css/:filename" $ do
      filename <- param "filename"
      setHeader "Content-Type" "text/css"
      setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour
      cachedBody redis filename $ do
        css <- liftIO $ BL.readFile $ "assets/css/" ++ (TL.unpack filename)
        return (if (List.isInfixOf ".min." $ TL.unpack filename) then css else (Helpers.minifyCSS $ BL.toStrict css))
      
    get (regex "/(assets/.*)") $ do
      relPath <- param "1"
      mimeType <- liftIO $ TL.pack <$> magicFile magic relPath
      setHeader "Content-Type" mimeType
      setHeader "Cache-Control" "public, max-age=604800, s-max-age=604800, no-transform" -- one week
      cachedBody redis (TL.pack relPath) $ liftIO $ BL.readFile relPath
  
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
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  forM_ ("/assets/css/blog.css":cssFiles) (\x -> link ! href (textValue x) ! rel "stylesheet" ! type_ "text/css")
  forM_ (List.nubBy (\(keyOne, _) (keyTwo, _) -> keyOne == keyTwo) $ metaTags ++ seoTags) (\x -> meta ! A.name (textValue $ fst x) ! content (textValue $ snd x))

renderBody :: Maybe T.Text -> Maybe T.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (textValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! A.id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! A.id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) (a ! href "/logout" ! class_ "blogbutton" ! rel "nofollow" $ "Logout")
  when (isJust maybeUser) (a ! href "/posts/new" ! class_ "blogbutton" ! rel "nofollow" $ "New Post")
  div ! A.id "content" $ bodyHtml
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  input ! type_ "text" ! A.id "title-field" ! placeholder "Post title" ! value (textValue $ maybe "" Types.title maybeBlogPost)

  div ! A.id "preview" $ ""
  textarea ! A.id "editor" ! customAttribute "post-id" (stringValue $ maybe "-1" (show . Types.identifier) maybeBlogPost) $ H.text $ maybe "" Types.body maybeBlogPost
  textarea ! A.id "tags" ! class_ "wordlist" $ toHtml $ List.intercalate ", " $ maybe [] Types.tags maybeBlogPost
  
  div ! A.id "checkbox-container" $ do
    case maybeBlogPost of
      Just (BlogPost _ _ _ _ _ _ False _) -> input ! type_ "checkbox" ! A.id "public-checkbox" ! A.checked ""
      _                                   -> input ! type_ "checkbox" ! A.id "public-checkbox"
    H.label ! customAttribute "for" "public-checkbox" $ "Public"

  a ! A.id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! A.id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! A.id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"

  script ! src "/assets/js/jquery-2.1.3.min.js" $ ""
  script ! src "/assets/js/marked.min.js" $ ""
  script ! src "/assets/js/wordlist.js" $ ""
  script ! src "/assets/js/common.js" $ ""
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
  liftIO $ Auth.getObject redis (T.encodeUtf8 <$> atoken)
  
checkAuth :: Redis.Connection -> ActionM (Maybe User)
checkAuth redis = do
  maybeUser <- (getUser redis)
  case maybeUser of
    (Just user) -> return $ Just user
    _ -> do
      return Nothing
      redirect "/login?error_message=Login%20required%2E"

-------------------------------------------------------------------------------
--- | Caching

cachedBody :: Redis.Connection -> TL.Text -> ActionM BL.ByteString -> ActionM ()
cachedBody redis key valueFunc = do
  env <- liftIO $ safeGetEnv "ENVIRONMENT" "development"
  if env == "production"
    then do
      redisValue <- liftIO $ Redis.runRedis redis $ Redis.get $ BL.toStrict $ TL.encodeUtf8 key
      
      case redisValue of
        Right (Just cached) -> rawBodyCached $ BL.fromStrict $ cached
        _ -> do
          v <- valueFunc
          liftIO $ Redis.runRedis redis $ do
            Redis.set (BL.toStrict $ TL.encodeUtf8 key) (BL.toStrict v)
            Redis.expire (BL.toStrict $ TL.encodeUtf8 key) 3600
          rawBodyCached v
          
    else valueFunc >>= Scotty.raw
    
rawBodyCached :: BL.ByteString -> ActionM ()
rawBodyCached str = do
  let hashSum = (md5Sum str)
  maybeinm <- Scotty.header "If-None-Match"
  case maybeinm of
    Just inm -> do
      if hashSum == (TL.encodeUtf8 inm)
        then status $ Status 304 ""
        else do
          setHeader "ETag" $ TL.decodeUtf8 hashSum
          Scotty.raw str
    Nothing -> do
      setHeader "ETag" $ TL.decodeUtf8 hashSum
      Scotty.raw str

-------------------------------------------------------------------------------
--- | Database

upsertBlogPost :: PG.Connection -> User -> Maybe Integer -> Maybe T.Text -> Maybe T.Text -> Maybe [String] -> Maybe [String] -> Bool -> IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      Nothing      Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET is_draft=? WHERE identifier=? RETURNING identifier" (isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      Nothing      Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) Nothing      Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) (Just tags_) Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) (Just tags_) (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, tags_, deletedTags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) Nothing      Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET bodyText=?, is_draft=? WHERE identifier=? RETURNING identifier" (body_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      (Just tags_) Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (tags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) (Just tags_) Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET bodyText=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, tags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      (Just tags_) Nothing             isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, tags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      Nothing      (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       Nothing      (Just tags_) (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET tags=uniq_cat(array_diff(tags,?),?), is_draft=? WHERE identifier=? RETURNING identifier" (deletedTags_, tags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) Nothing       (Just body_) Nothing      (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (body_, deletedTags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) Nothing      Nothing      (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, deletedTags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg _    (Just identifier_) (Just title_) (Just body_) Nothing      (Just deletedTags_) isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?), is_draft=? WHERE identifier=? RETURNING identifier" (title_, body_, deletedTags_, isdraft, identifier_)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg user Nothing            (Just title_) (Just body_) Nothing      _                   isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "INSERT INTO blogposts (title, bodyText, author_id, is_draft) VALUES (?, ?, ?, ?) RETURNING identifier" (title_, body_, Types.uid user, isdraft)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost pg user Nothing            (Just title_) (Just body_) (Just tags_) _                   isdraft = (listToMaybe <$> map fromOnly <$> ((query pg "INSERT INTO blogposts (title, bodyText, tags, author_id, is_draft) VALUES (?, ?, ?, ?, ?) RETURNING identifier" (title_, body_, tags_, Types.uid user, isdraft)) :: IO [Only Integer])) :: IO (Maybe Integer)
upsertBlogPost _  _    _                 _            _            _           _                      _       = return Nothing

getBlogPostsByTag :: PG.Connection -> T.Text -> Maybe Integer -> IO [BlogPost]
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
