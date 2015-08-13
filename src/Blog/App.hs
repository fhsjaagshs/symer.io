{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Blog.App
(
  initState,
  startApp,
  startRedirect,
  startRedirectProcess,
  app
)
where

import Blog.State
import Blog.Database.Config
import Blog.Database.Util
import Blog.Caching
import Blog.HTMLUtil
import Blog.Assets
import Blog.Composable
import Blog.Comment
import Blog.User
import Blog.Post as Post
import Blog.Auth
import Blog.MIME

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Control.Applicative
import           Data.Maybe

import           Web.Scotty.Trans as Scotty
import           Network.HTTP.Types.Status
import           Network.Wai (responseLBS, requestHeaderHost, rawPathInfo, rawQueryString)
import           Network.Wai.Handler.Warp (defaultSettings, setPort, setBeforeMainLoop, runSettings)
import           Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,runTLS)

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

import qualified Crypto.BCrypt as BCrypt

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Prelude as P hiding (head, div)

import           System.Exit
import           System.Posix
import           System.Environment

-- TODO (future):
-- redo comments' appearance
-- Comment-optional posts
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- Save MD5 sum in redis
-- Make sure comments are rendered safely
-- Links in comments *** nofollow them
-- search
-- truncate post body when more than one post is visible.

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?

startRedirectProcess :: IO ()
startRedirectProcess = do
  privileged <- isPrivileged
  when privileged $ do
    putStrLn "spawning redirection process"
    pname <- getProgName
    pid <- withProgName (pname ++ "-redirectssl") $ do
      forkProcess $ do
        void $ installHandler sigTERM (Catch childHandler) Nothing
        startRedirect
    void $ installHandler sigTERM (Catch $ parentHandler pid) Nothing
    void $ installHandler sigINT (Catch $ parentHandler pid) Nothing
    where
      childHandler = do
        putStrLn "killed redirection process"
        exitImmediately ExitSuccess
      parentHandler pid = do
        putStrLn "killing redirection process"
        signalProcess sigTERM pid
        exitImmediately ExitSuccess

resignPrivileges :: String -> IO ()
resignPrivileges user = do
  privileged <- isPrivileged
  when privileged $ do
    getUserEntryForName user >>= setUserID . userID

isPrivileged :: IO Bool
isPrivileged = getEffectiveUserID >>= return . ((==) 0)

initState :: String -> IO AppState
initState dbpass = do
  putStrLn "establishing database connections"
  pg <- PG.connectPostgreSQL $ B.pack $ postgresConnStr dbpass
  redis <- Redis.connect Redis.defaultConnectInfo
  putStrLn "running database migrations"
  runMigrations pg
  return $ AppState redis pg

-- The problem daemonizing comes from Warp
startApp :: Int -> FilePath -> FilePath -> AppState -> IO ()
startApp port cert key state = do
  putStrLn "starting https"
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO app >>= liftIO . runTLS tlsSettings warpSettings
  where
    tlsSettings = defaultTlsSettings { keyFile = key, certFile = cert }
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort port defaultSettings
  
startRedirect :: IO ()
startRedirect = do
  privileged <- isPrivileged
  when privileged $ do
    putStrLn "redirecting unencrypted traffic"
    runSettings warpSettings $ \req respond -> do
      respond $ responseLBS status301 (mkHeaders req) ""
  where
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort 80 defaultSettings
    mkHeaders r = [("Location", mconcat ["https://", fromJust $ requestHeaderHost r, rawPathInfo r, rawQueryString r])]

--------------------------------------------------------------------------------
app :: ScottyT TL.Text WebM ()
app = do
  get "/" $ do
    maybeUser <- getUser
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getPosts mPageNum
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHead blogTitle $ do
        renderMeta "description" description
        renderMeta "keywords" (TL.fromStrict $ T.intercalate ", " keywords)
      renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  get "/drafts" $ do
    maybeUser <- authenticate
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getDrafts (fromJust maybeUser) mPageNum
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHiddenHead' $ appendedBlogTitle "Drafts"
      renderBody (Just "Drafts") Nothing maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  -- create a post
  get "/posts/new" $ do
    authenticate
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHiddenHead (appendedBlogTitle "New Post") $ do
        renderStylesheet "/assets/css/editor.css"
        renderStylesheet "/assets/css/wordlist.css"
      renderBody Nothing Nothing Nothing $ do
        renderPostEditor Nothing

  -- view a specific post
  get "/posts/:id" $ do
    mpost <- param "id" >>= getPost
    case mpost of
      Nothing -> redirect "/notfound"
      Just pst@(Post _ ttl bdy _ tags draft author) -> do
        maybeUser <- getUser
        
        when (draft && (maybe True ((/=) author) maybeUser)) (redirect "/notfound")

        Scotty.html . R.renderHtml . docTypeHtml $ do
          renderHead (appendedBlogTitle $ TL.fromStrict ttl) $ do
            renderStylesheet "/assets/css/post.css"
            renderMeta "keywords" . TL.fromStrict . T.intercalate ", " . flip (++) keywords $ tags
            renderMeta "description" . TL.take 150 . TL.fromStrict $ bdy
          renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
            render pst maybeUser
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
    mPageNum <- (fmap (read . TL.unpack) . lookup "page") <$> params
    posts <- getPostsByTag tag mPageNum
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHead (appendedBlogTitle $ TL.fromStrict tag) $ do
        renderMeta "description" description
        renderMeta "keywords" . TL.fromStrict . T.intercalate ", " $ keywords
      renderBody (Just $ mconcat ["Posts tagged '", TL.fromStrict tag, "'"]) Nothing maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)

  -- edit a post
  get "/posts/:id/edit" $ do
    authenticate
    identifier_ <- param "id"
    res <- getPost identifier_
    case res of
      Nothing -> next
      Just post_ -> Scotty.html . R.renderHtml . docTypeHtml $ do
        renderHiddenHead (appendedBlogTitle $ TL.fromStrict $ postTitle post_) $ do
          renderStylesheet "/assets/css/editor.css"
          renderStylesheet "/assets/css/wordlist.css"
        renderBody Nothing Nothing Nothing $ do
          renderPostEditor $ Just post_

  get "/login" $ do
    maybeUser <- getUser
    when (isJust maybeUser) (redirect "/")
    maybeErrorMessage <- lookup "error_message" <$> params
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHiddenHead' $ appendedBlogTitle "Login"
      renderBody (Just "Login") maybeErrorMessage Nothing $ do
        H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
          input ! type_ "hidden" ! A.name "source" ! value "form"
          renderInput "text" ! A.id "username" ! placeholder "Username" ! A.name "username"
          renderInput "password" ! A.id "password" ! placeholder "Password" ! A.name "password"
        renderButton'' "Login" "submit"
        script $ toHtml $ unlines [
          "var passwd = document.getElementById('password');",
          "var uname = document.getElementById('username');",
          "var form = document.getElementById('loginform');",
          "var submit = document.getElementById('submit')",
          "uname.onkeydown = function(e) {if(e.keyCode==13)passwd.focus();}",
          "passwd.onkeydown = function(e) {if(e.keyCode==13)submit.click();}",
          "submit.onclick = function() {form.submit();}"
          ]

  get "/logout" $ deleteAuth >> redirect "/"
  
  post "/login" $ do
    pPassword <- T.encodeUtf8 <$> param "password"
    pRedirectPath <- lookup "redirect" <$> params
    mUser <- param "username" >>= getUserWithUsername
    case mUser of
      Nothing -> redirect "/login?error_message=Username%20does%20not%20exist%2E"
      (Just user@(User _ _ _ (Just phash))) -> do
        if BCrypt.validatePassword (T.encodeUtf8 phash) pPassword
          then do
            setUser user
            redirect $ fromMaybe "/" pRedirectPath
          else redirect "/login?error_message=Invalid%20password%2E"
      _ -> redirect  "/login?error_message=Invalid%20User%2E"

  -- deletes a BlogPost from the database
  -- returns JSON
  Scotty.delete "/posts/:id" $ do
    authUser <- authenticate
    res <- param "id" >>= (flip deletePost $ fromJust authUser)
    case (res :: Maybe Integer) of
      Nothing -> status $ Status 404 "blog post not found."
      Just _  -> Scotty.text "ok"
      
  get "/posts/:id/comments.json" $ do
    param "id" >>= getCommentsForPost >>= Scotty.json . nestComments

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    auth <- authenticate
    case auth of
      Nothing -> status $ Status 401 "Missing authentication"
      (Just authUser) -> do
        ps <- params
        mPostID <- upsertPost authUser
                              (read . TL.unpack <$> lookup "id" ps)
                              (TL.toStrict <$> lookup "title" ps)
                              (TL.toStrict <$> lookup "body" ps)
                              (map TL.toStrict . TL.splitOn "," <$> lookup "tags" ps)
                              (map TL.toStrict . TL.splitOn "," <$> lookup "deleted_tags" ps)
                              (elem (fromMaybe "True" . lookup "draft" $ ps) ["t", "true", "True", "y", "yes"])
        case mPostID of
          Nothing -> status $ Status 400 "Missing required parameters"
          Just pid -> addHeader "Location" $ TL.pack $ "/posts/" ++ (show pid)

  post "/posts/:id/comments" $ do
    postId_ <- param "post_id"
    email_ <- param "email"
    displayName_ <- param "display_name"
    body_ <- param "body"
    parentId_ <- (fmap (read . TL.unpack) . lookup "parent_id") <$> params
    commentId <- insertComment parentId_ postId_ email_ displayName_ body_ 
    addHeader "Location" $ TL.pack $ "/posts/" ++ (show postId_)
    case commentId of
      Just cid_ -> Scotty.text $ TL.pack $ show cid_
      Nothing -> do
        Scotty.status $ Status 500 "Failed to insert comment."
        Scotty.text "Failed to insert comment."

  -- returns minified JS
  get "/assets/js/:filename" $ do
    filename <- param "filename"
    env <- fromMaybe "development" <$> (liftIO $ lookupEnv "ENV")
    setHeader "Content-Type" "text/javascript"
    when (env == "production") setCacheControl
    cachedBody filename $ (fromMaybe "") <$> (js $ B.unpack filename)

  -- returns minified CSS
  get "/assets/css/:filename" $ do
    filename <- param "filename"
    setHeader "Content-Type" "text/css"
    env <- fromMaybe "development" <$> (liftIO $ lookupEnv "ENV")
    when (env == "production") setCacheControl
    cachedBody filename $ (fromMaybe "") <$> (css $ B.unpack filename)
    
  get (regex "/(assets/.*)") $ do
    relPath <- param "1"
    env <- fromMaybe "development" <$> (liftIO $ lookupEnv "ENV")
    setHeader "Content-Type" $ TL.pack $ getMimeAtPath $ TL.unpack relPath
    when (env == "production") $ setHeader "Cache-Control" "public, max-age=604800, s-max-age=604800, no-transform" -- one week
    cachedBody (T.encodeUtf8 $ TL.toStrict relPath) $ BL.readFile $ TL.unpack relPath

  notFound $ Scotty.text "Not Found."
           
keywords :: [T.Text]
keywords = ["computer science", "functional programming", "fp", "politics", "haskell", "ruby", "web development", "art", "blogs", "computers", "startups", "tutorial", "rails"]

description :: TL.Text
description = "Rants and raves about functional programming, politics, and everything in between."