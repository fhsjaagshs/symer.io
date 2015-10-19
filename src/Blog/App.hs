{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Blog.App
(
  initState,
  startApp,
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
import Blog.Comment
import Blog.User
import Blog.Post as Post
import Blog.Auth
import Blog.MIME
import Blog.Env

import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Maybe
       
import Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status
import Network.Wai (responseLBS,requestHeaderHost,rawPathInfo,rawQueryString)
import Network.Wai.Handler.Warp (defaultSettings,setPort,setBeforeMainLoop,runSettings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,runTLS)
import Network.Wai.Middleware.Gzip

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

import qualified Crypto.BCrypt as BCrypt

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.String

import Data.Aeson (encode)
import Text.Blaze.Html5 as H hiding (style, param, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
       
import System.Exit
import System.Posix
import System.Directory

-- TODO (features):
-- Comment-optional posts
-- Editor key commands (cmd-i, cmd-b, etc)
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- Save MD5 sum in redis
-- Links in comments *** nofollow them
-- search

{-
TODO (internals)
* investigate:
  add_header Strict-Transport-Security "max-age=31536000; includeSubdomains";
* start blog without ssl
-}

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?

startRedirectProcess :: IO ()
startRedirectProcess = void $ do
  putStrLn "starting http -> https redirection process"
  pid <- forkProcess $ do
    installHandler sigTERM (Catch childHandler) Nothing
    runSettings warpSettings $ \req respond -> do
      respond $ responseLBS status301 (mkHeaders req) ""
  installHandler sigTERM (Catch $ parentHandler pid) Nothing
  installHandler sigINT (Catch $ parentHandler pid) Nothing
  where
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort 80 defaultSettings
    mkHeaders r = [("Location", url r)]
    host = fromJust . requestHeaderHost
    url r = mconcat ["https://", host r, rawPathInfo r, rawQueryString r]
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
isPrivileged = ((==) 0) <$> getEffectiveUserID

initState :: String -> FilePath -> FilePath -> FilePath -> IO AppState
initState dbpass crt key rootcrt = do
  putStrLn "establishing database connections"
  pg <- PG.connectPostgreSQL $ B.pack $ postgresConnStr dbpass crt key rootcrt
  redis <- Redis.connect Redis.defaultConnectInfo
  putStrLn "running database migrations"
  runMigrations pg
  return $ AppState redis pg

startApp :: Int -> FilePath -> FilePath -> AppState -> IO ()
startApp port cert key state = do
  privileged <- isPrivileged
  when privileged startRedirectProcess
  putStrLn "starting https"
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO app >>= liftIO . run
  where
    run = runTLS tlsSettings warpSettings
    tlsSettings = defaultTlsSettings { keyFile = key, certFile = cert }
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort port defaultSettings

--------------------------------------------------------------------------------
app :: ScottyT TL.Text WebM ()
app = do
  middleware $ gzip def
  
  get "/" $ do
    maybeUser <- getUser
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getPosts mPageNum
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHead blogTitle $ do
        renderMeta "description" description
        renderMeta "keywords" (TL.fromStrict $ T.intercalate ", " keywords)
      renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
        renderPosts (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  get "/drafts" $ do
    maybeUser <- authenticate
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getDrafts (fromJust maybeUser) mPageNum
    Scotty.html . R.renderHtml . docTypeHtml $ do
      renderHiddenHead' $ appendedBlogTitle "Drafts"
      renderBody (Just "Drafts") Nothing maybeUser $ do
        renderPosts (take postsPerPage posts) maybeUser
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
      Just pst@(Post _ ttl _ _ tags draft author) -> do
        maybeUser <- getUser
        
        when (draft && (maybe True ((/=) author) maybeUser)) (redirect "/notfound")

        Scotty.html . R.renderHtml . docTypeHtml $ do
          renderHead (appendedBlogTitle $ TL.fromStrict ttl) $ do
            renderMeta "keywords" . TL.fromStrict . T.intercalate ", " . flip (++) keywords $ tags
            renderMeta "description" $ TL.fromStrict $ postDescription pst
          renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
            renderPost False pst maybeUser
            renderScript "/assets/js/common.js"
            renderScript "/assets/js/comments.js"
            
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
        renderPosts (take postsPerPage posts) maybeUser
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
    maybeErrorMessage <- lookup "err" <$> params
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
          "var submit = document.getElementById('submit');",
          "uname.onkeydown = function(e) {if(e.keyCode==13)passwd.focus();}",
          "passwd.onkeydown = function(e) {if(e.keyCode==13)submit.click();}",
          "submit.onclick = function() {form.submit();}"
          ]

  get "/logout" $ deleteAuth >> redirect "/"
  
  post "/login" $ do
    mUser <- param "username" >>= getUserWithUsername
    case mUser of
      Nothing -> redirect "/login?err=Username%20does%20not%20exist%2E"
      (Just user@(User _ _ _ (Just phash))) -> do
        pPassword <- T.encodeUtf8 <$> param "password"
        if BCrypt.validatePassword (T.encodeUtf8 phash) pPassword
          then do
            setUser user
            params >>= redirect . fromMaybe "/" . lookup "redirect"
          else redirect "/login?err=Invalid%20password%2E"
      _ -> redirect "/login?err=Invalid%20user%2E"

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
                              (maybe True truthy (lookup "draft" ps))
        case mPostID of
          Nothing -> status $ Status 400 "Missing required parameters"
          Just pid -> addHeader "Location" $ TL.pack $ "/posts/" ++ (show pid)

  -- deletes a BlogPost from the database
  Scotty.delete "/posts/:id" $ do
    authUser <- authenticate
    res <- param "id" >>= (flip deletePost $ fromJust authUser)
    case (res :: Maybe Integer) of
      Nothing -> status $ Status 404 "blog post not found."
      Just _  -> Scotty.text "ok"

  post "/posts/:id/comments" $ do
    postId <- param "id"
    email <- param "email"
    displayName <- param "display_name"
    bdy <- param "body"
    parentId <- fmap (read . TL.unpack) . lookup "parent_id" <$> params
    mCommentId <- insertComment parentId postId email displayName bdy
    case mCommentId of
      Just commentId -> do
        addHeader "Location" $ TL.pack $ "/posts/" ++ (show postId)
        Scotty.text . TL.pack . show $ commentId
      Nothing -> do
        Scotty.status $ Status 500 "Failed to insert comment."
        Scotty.text "Failed to insert comment."
        
  get "/posts/:id/comments.json" $ do
    production setCacheControl
    path <- rawPathInfo <$> request
    setHeader "Content-Type" "application/json"
    let act = param "id" >>= getCommentsForPost >>= return . encode . nestComments
    cachedBody path act
    
  get (regex "/(assets/.*)") $ do
    production setCacheControl
    relPath <- param "1"
    let mimetype = getMimeAtPath relPath
    let f = cachedBody (B.pack relPath)
    let eact err = (Scotty.status . Status 500 . B.pack $ err) >> return ""
    setHeader "Content-Type" $ TL.pack mimetype

    exists <- liftIO $ doesFileExist relPath
    if not exists
      then Scotty.status . Status 404 . B.pack $ "File " ++ relPath ++ " does not exist."
      else case mimetype of
        "application/javascript" -> f $ (liftIO $ js relPath) >>= either eact return
        "text/css" -> f $ (liftIO $ css relPath) >>= either eact return
        _ -> f . liftIO . BL.readFile $ relPath

  get (regex "/favicon.*") $ redirect "/assets/images/philly_skyline.svg"

  notFound $ Scotty.text "Not Found."

truthy :: (Eq a, IsString a) => a -> Bool
truthy "t" = True
truthy "y" = True
truthy "true" = True
truthy "True" = True
truthy _ = False

keywords :: [T.Text]
keywords = ["computer science",
            "functional programming",
            "fp",
            "politics",
            "haskell",
            "ruby",
            "web development",
            "art",
            "blogs",
            "computers",
            "startups",
            "tutorial",
            "rails"]

description :: TL.Text
description = "Rants and raves about functional programming, politics, and everything in between."