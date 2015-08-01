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
import Blog.Types as Types
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

app :: ScottyT TL.Text WebM ()
app = do
  -- blog root
  get "/" $ do
    maybeUser <- getUser
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getBlogPosts mPageNum
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] blogTitle
      renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
        render (take postsPerPage posts) maybeUser
        renderPageControls mPageNum (length posts > postsPerPage)
        
  get "/drafts" $ do
    maybeUser <- authenticate
    maybePNum <- lookup "page" <$> params
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
          renderHead ["/assets/css/post.css"] (postTags $ Types.tags post_) $ appendedBlogTitle $ TL.fromStrict $ Types.title post_
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
    mPageNum <- (fmap (read . TL.unpack) . lookup "page") <$> params
    posts <- getBlogPostsByTag tag mPageNum
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] (appendedBlogTitle $ tag)
      renderBody (Just $ TL.append "Posts tagged '" $ TL.append tag "'") Nothing maybeUser $ do
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
          renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] (appendedBlogTitle $ TL.fromStrict $ Types.title post_)
          renderBody Nothing Nothing Nothing $ do
            renderPostEditor $ Just post_
            
  get "/login" $ do
    maybeUser <- getUser
    when (isJust maybeUser) (redirect "/")
    maybeErrorMessage <- lookup "error_message" <$> params
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
    pRedirectPath <- lookup "redirect" <$> params
    res <- getUserWithUsername pUsername
    
    case res of
      Nothing     -> redirect "/login?error_message=Username%20not%20found%2E"
      (Just user@(User _ _ _ (Just phash))) -> do
        if BCrypt.validatePassword (T.encodeUtf8 phash) (T.encodeUtf8 pPassword)
          then do
            setUser user
            redirect $ fromMaybe "/" pRedirectPath
          else redirect "/login?error_message=Invalid%20password%2E"
      _ -> redirect  "/login?error_message=Invalid%20User%2E"

  -- deletes a BlogPost from the database
  -- returns JSON
  Scotty.delete "/posts/:id" $ do
    authUser <- authenticate
    identifier_ <- param "id"
    res <- deleteBlogPost identifier_ (fromJust authUser)
    case (res :: Maybe Integer) of
      Nothing -> status $ Status 404 "blog post not found."
      Just _ -> Scotty.text "ok"

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    auth <- authenticate
    case auth of
      Nothing -> status $ Status 401 "Missing authentication"
      (Just authUser) -> do
        ps <- params
        maybeBPIdentifier <- upsertBlogPost authUser
                                             (read . TL.unpack <$> lookup "id" ps)
                                             (lookup "title" ps)
                                             (lookup "body" ps)
                                             (splitList ',' . TL.unpack <$> lookup "tags" ps)
                                             (splitList ',' . TL.unpack <$> lookup "deleted_tags" ps)
                                             (elem (fromMaybe "True" . lookup "draft" $ ps) ["t", "true", "True", "y", "yes"])
        case maybeBPIdentifier of
          Nothing -> status $ Status 400 "Missing required parameters"
          Just identifier_ -> addHeader "Location" $ TL.pack $ "/posts/" ++ (show identifier_)
        

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

renderInput :: String -> Html
renderInput kind = input
                   ! class_ "blogtextfield"
                   ! customAttribute "autocorrect" "off"
                   ! customAttribute "autocapitalize" "off"
                   ! customAttribute "spellcheck" "false"
                   ! type_ (stringValue kind)