module Blog.App
(
  initState,
  startApp,
  app
)
where

import Blog.State
import Blog.Database.Config
import Blog.Database.Util
import Blog.Caching
import Blog.HTMLUtil
import Blog.Assets
import Blog.Types
import Blog.Auth

import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Maybe

import           Web.Scotty.Trans as Scotty
import           Web.Scotty.TLS as Scotty
import           Network.HTTP.Types.Status

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.List as L
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Prelude as P hiding (head, div)

-- TODO:
-- Comment-optional posts
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- Save MD5 sum in redis
-- Make sure comments are rendered safely
-- Links in comments *** nofollow them
-- search

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?

initState :: IO State
initState = do
  liftIO $ putStrLn "--| establishing database connections"
  pg <- PG.connectPostgreSQL $ B.pack $ Config.postgresConnStr
  redis <- Redis.connect Redis.defaultConnectInfo
  putStrLn $ "--| starting blog: " ++ env
  putStrLn "--| running database migrations"
  runMigrations pg
  putStrLn "--| clearing cached data from Redis"
  Redis.runRedis redis $ Redis.flushall
  putStrLn "--| blog started"
  return $ State redis pg

startApp :: Integer -> String -> FilePath -> FilePath -> State -> IO ()
startApp port env crtfile keyfile state = do
  sync <- newTVarIO state
  let runActionToIO = (>>=) sync . runReaderT . runWebM
  scottyTTLS port keyfile crtfile runActionToIO app

app :: ScottyT TL.Text WebM ()
app = do
  -- blog root
  get "/" $ do
    maybeUser <- getUser
    maybePNum <- maybeParam "page"
    let mPageNum = read . TL.unpack <$> maybePNum
    posts <- getBlogPosts mPageNum
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] blogTitle
      renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  get "/drafts" $ do
    maybeUser <- authenticate
    maybePNum <- maybeParam "page"
    let mPageNum = read . TL.unpack <$> maybePNum
    posts <- getDrafts (fromJust maybeUser) mPageNum
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [("robots","noindex, nofollow")] (appendedBlogTitle "Drafts")
      renderBody (Just "Drafts") Nothing maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  -- create a post
  get "/posts/new" $ do
    authenticate
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] $ appendedBlogTitle "New Post"
      renderBody Nothing Nothing Nothing $ do
        renderPostEditor Nothing
  
  -- view a specific post
  get "/posts/:id" $ do
    identifier_ <- param "id"
    maybeUser <- getUser
    res <- getBlogPost identifier_
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
            div ! A.id "comments" $ do
              div ! A.id "spinner-container" $ ""
            script ! src "/assets/js/spin.js" $ ""
            script ! src "/assets/js/md5.js" $ ""
            script ! src "/assets/js/common.js" $ ""
            script ! src "/assets/js/post.js" $ ""
            
  get "/posts/by/tag/:tag" $ do
    tag <- param "tag"
    maybeUser <- getUser
    maybePNum <- maybeParam "page"
    let mPageNum = read . TL.unpack <$> maybePNum
    posts <- getBlogPostsByTag tag mPageNum
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] (appendedBlogTitle $ tag)
      renderBody (Just $ T.append "Posts tagged '" $ T.append tag "'") Nothing maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)

  -- edit a post
  get "/posts/:id/edit" $ do
    authenticate
    identifier_ <- param "id"
    res <- getBlogPost identifier_
    case res of
      Nothing -> next
      Just post_ -> do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] (appendedBlogTitle $ Types.title post_)
          renderBody Nothing Nothing Nothing $ do
            renderPostEditor $ Just post_
            
  get "/login" $ do
    maybeUser <- getUser
    when (isJust maybeUser) (redirect "/")
    maybeErrorMessage <- (rescue (Just <$> param "error_message") (\_ -> return Nothing))
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [("robots","noindex, nofollow")] $ appendedBlogTitle "Login"
      renderBody (Just "Login") maybeErrorMessage Nothing $ do
        H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
          input ! type_ "hidden" ! A.name "source" ! value "form"
          renderInput "text" ! A.id "username" ! placeholder "Username" ! A.name "username" ! onkeydown "if(event.keyCode==13)document.getElementById('password').focus()"
          renderInput "password" ! A.id "password" ! placeholder "Password" ! A.name "password" ! onkeydown "if(event.keyCode==13)document.getElementById('submit').click()"
        a ! A.id "submit" ! onclick "document.getElementById('loginform').submit();" ! class_ "blogbutton" ! rel "nofollow" $ "Login"

  get "/logout" $ do
    deleteAuth
    redirect "/"
  
  get "/posts/:id/comments.json" $ do
    identifier_ <- param "id"
    getCommentsForPost identifier_ >>= Scotty.json . nestComments . (map (\c -> Node c [] Nothing))
  
  post "/login" $ do
    pUsername <- param "username"
    pPassword <- param "password"
    pRedirectPath <- maybeParam "redirect"
    res <- getUserWithUsername pUsername
    
    case res of
      Nothing     -> redirect "/login?error_message=Username%20not%20found%2E"
      (Just user) -> do
        -- TODO: rewrite!!!!!
        if Helpers.checkPassword (fromJust $ Types.passwordHash user) pPassword
          then do
            setAuth user
            redirect $ fromMaybe "/" pRedirectPath
          else redirect "/login?error_message=Invalid%20password%2E"

  -- deletes a BlogPost from the database
  -- returns JSON
  delete "/posts/:id" $ do
    authUser <- authenticate
    identifier_ <- param "id"
    res <- deleteBlogPost identifier_ (fromJust authUser)
    case (res :: Maybe Integer) of
      Nothing -> status $ Status 404 "blog post not found."
      Just _ -> Scotty.text "ok"

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    authUser <- authenticate
    ps <- params
    maybeBPIdentifier <- (upsertBlogPost (fromJust authUser)
                                         ((read . TL.unpack) <$> lookup "id" ps)
                                         (TL.toStrict <$> lookup "title" ps)
                                         (TL.toStrict <$> lookup "body" ps)
                                         ((splitList ',' . TL.unpack) <$> (lookup "tags" ps))
                                         ((splitList ',' . TL.unpack) <$> (lookup "deleted_tags" ps))
                                         (elem (TL.unpack . fromMaybe "True" . lookup "draft" $ ps) ["t", "true", "True", "y", "yes"]))
    case maybeBPIdentifier of
      Nothing -> status $ Status 400 "Missing required parameters"
      Just identifier_ -> addHeader "Location" $ TL.pack $ "/posts/" ++ (show identifier_)

  post "/posts/:id/comments" $ do
    postId_ <- param "post_id"
    email_ <- param "email"
    displayName_ <- param "display_name"
    body_ <- param "body"
    parentId_ <- maybeParam "parent_id"
    commentId <- insertComment ((read . TL.unpack) <$> parentId_) postId_ email_ displayName_ body_ 
    addHeader "Location" $ TL.pack $ "/posts/" ++ (show postId_)
    case commentId of
      Just cid_ -> Scotty.text $ TL.pack $ show cid_
      Nothing -> do
        Scotty.status $ Status 500 "Failed to insert comment."
        Scotty.text "Failed to insert comment."

  -- returns minified JS
  get "/assets/js/:filename" $ do
    filename <- param "filename"
    setHeader "Content-Type" "text/javascript"
    when (env == "production") setCacheControl
    cachedBody filename $ fromMaybe "" $ js $ B.unpack filename

  -- returns minified CSS
  get "/assets/css/:filename" $ do
    filename <- param "filename"
    setHeader "Content-Type" "text/css"
    env <- fromMaybe "development" <$> lookupEnv "ENV"
    when (env == "production") setCacheControl
    cachedBody filename $ fromMaybe "" $ css $ B.unpack filename
    
  get (regex "/(assets/.*)") $ do
    relPath <- param "1"
    setHeader "Content-Type" $ TL.pack $ getMimeAtPath $ TL.unpack relPath
    when (env == "production") $ setHeader "Cache-Control" "public, max-age=604800, s-max-age=604800, no-transform" -- one week
    cachedBody (T.encodeUtf8 $ TL.toStrict relPath) $ BL.readFile $ TL.unpack relPath

  notFound $ Scotty.text "Not Found."