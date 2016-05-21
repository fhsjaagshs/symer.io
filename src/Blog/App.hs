{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Blog.App
(
  app
)
where

import Blog.User
import Blog.Post
import Blog.Comment
import Blog.Assets
import Blog.AppState
import qualified Blog.HTML as HTML
import qualified Blog.HTML.CSS as CSS
import qualified Blog.HTML.SVG as SVG

import Web.App
import Network.HTTP.Types.Status
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.ForceSSL

import Data.Maybe
import Data.Niagra (NiagraT(..),css)
 
import Control.Monad
import Control.Monad.IO.Class

import System.IO.Unsafe
import System.Environment

import qualified Crypto.BCrypt as BCrypt

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B

-- TODO features:
-- Comment-optional posts
-- Editor key commands (cmd-i, cmd-b, etc)
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- Links in comments *** nofollow them
-- search

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?
-- 2. JSON API

--------------------------------------------------------------------------------

app :: WebApp AppState IO
app = mconcat [
  middleware                               $ addHeaders [("Cache-Control",ccontrol)],
  middleware                               $ gzip 1400,
  forceSSLMiddleware,
  get  "/"                                 getRoot,
  get  "/login"                            getLogin,
  get  "/logout"                           $ deleteAuth >> redirect "/",
  post "/login"                            postLogin,
  get  "/drafts"                           getPageDrafts,
  post "/posts"                            postPosts,
  get  "/posts/new"                        $ authenticate >> (renderHtml $ HTML.postEditor Nothing),
  get  "/posts/:id"                        getPagePostById,
  get  "/posts/by/tag/:tag"                getPagePostsByTag,
  get  "/posts/:id/edit"                   getPageEditor,
  post "/posts/:id/comments"               postComments,
  get  "/assets/css/blog.css"              $ cssFile CSS.blog,
  get  "/assets/css/comments.css"          $ cssFile CSS.comments,
  get  "/assets/css/editor.css"            $ cssFile CSS.editor,
  get  "/assets/css/wordlist.css"          $ cssFile CSS.wordlist,
  get  "/assets/images/gobutton.svg"       $ svgFile SVG.goButton,
  get  "/assets/images/philly_skyline.svg" $ svgFile SVG.phillySkyline,
  get  (regex "/assets/(.*)")              $ param "1" >>= loadAsset,
  matchAll                                 $ renderHtml $ HTML.notFound
  ]
  where
    ccontrol = "public,max-age=3600,s-max-age=3600,no-cache,must-revalidate,proxy-revalidate,no-transform"
    forceSSLMiddleware = if (environment == (Just "production")) then middleware forceSSL else mempty
    environment = unsafePerformIO $ lookupEnv "ENV"
{- Route functions -}

getRoot :: (MonadIO m) => RouteT AppState m ()
getRoot = renderHtmlM $ HTML.root <$> getAuthenticatedUser
                                  <*> (getPageNumber >>= getPosts)
                                  <*> getPageNumber
  
getPageDrafts :: (MonadIO m) => RouteT AppState m ()
getPageDrafts = do
  user <- authenticate
  pg <- getPageNumber
  drafts <- getDrafts user pg
  renderHtml $ HTML.drafts user drafts pg

postLogin :: (MonadIO m) => RouteT AppState m ()
postLogin = param "username" >>= getUser >>= f
  where
    f Nothing = redirect "/login?err=Username%20does%20not%20exist%2E"
    f (Just user@(User _ _ phash)) = do
      pPassword <- T.encodeUtf8 <$> param "password"
      if BCrypt.validatePassword (T.encodeUtf8 phash) pPassword
        then do
          setAuthenticatedUser user
          maybeParam "lookup" >>= redirect . fromMaybe "/"
        else redirect "/login?err=Invalid%20password%2E"
        
getLogin :: (MonadIO m) => RouteT AppState m ()
getLogin = do
  maybeUser <- getAuthenticatedUser
  when (isJust maybeUser) $ redirect "/"
  maybeParam "err" >>= renderHtml . HTML.login

postPosts :: (MonadIO m) => RouteT AppState m ()
postPosts = do
  u <- authenticate
  m <- maybeParam "method"
  i <- maybeParam "id"
  handleMethod u (m :: Maybe B.ByteString) i
  where
    handleMethod user (Just "DELETE") (Just p) = do
      pid <- deletePost user p
      case pid of
        Nothing -> do
          status status404
          writeBodyBytes "Failed to find post to delete."
        Just _ -> redirect "/"
    handleMethod user _ pid = do
      title <- (maybe "" $ T.decodeUtf8 . uncrlf)  <$> maybeParam "title"
      bdy   <- (maybe "" $ T.decodeUtf8 . uncrlf)  <$> maybeParam "body"
      tags  <- (maybe [] $ T.splitOn ",")          <$> maybeParam "tags"
      draft <- (maybe True not)                    <$> maybeParam "draft"
      p <- upsertPost pid title bdy tags draft user
      case p of
        Nothing -> do
          status status400
          writeBodyBytes "Missing required parameters"
        Just p' -> redirect $ B.pack $ "/posts/" ++ show p'
      where uncrlf = B.foldl f B.empty
              where f bs c
                      | c == '\n' && B.last bs == '\r' = B.snoc (B.init bs) '\n'
                      | otherwise = B.snoc bs c
            
getPagePostById :: (MonadIO m) => RouteT AppState m ()
getPagePostById = param "id" >>= getPost >>= maybe (redirect "/notfound") f
  where
    f pst@(Post pid _ _ _ _ draft author) = do
      maybeUser <- getAuthenticatedUser
      if draft && (maybe True (/= author) maybeUser)
        then redirect "/notfound"
        else getCommentsForPost pid >>= renderHtml . HTML.postDetail maybeUser pst
        
getPagePostsByTag :: (MonadIO m) => RouteT AppState m ()
getPagePostsByTag = do
  tag <- param "tag"
  pg <- getPageNumber
  user <- getAuthenticatedUser
  posts <- getPostsByTag tag pg
  renderHtml $ HTML.postsByTag user (TL.fromStrict tag) posts pg
  
postComments :: (MonadIO m) => RouteT AppState m ()
postComments = doInsert >>= maybe errorOut (\c -> (param "id" >>= redirectPost (commentID c)))
  where
    doInsert = do
      p <- maybeParam "parent_id"
      i <- param "id"
      b <- param "body"
      insertComment p i b
    redirectPost :: (MonadIO m) => Integer -> Integer -> RouteT AppState m ()
    redirectPost cid pid = redirect $ B.pack $ "/posts/" ++ show pid ++ "#comment" ++ show cid
    errorOut = status status500 >> writeBodyBytes "Failed to insert comment."
  
getPageEditor :: (MonadIO m) => RouteT AppState m ()
getPageEditor = authenticate >> param "id" >>= getPost >>= f
  where f = maybe next (renderHtml . HTML.postEditor . Just)
  
{- Helper Functions -}

cssFile :: (MonadIO m) => NiagraT (RouteT AppState m) () -> RouteT AppState m ()
cssFile c = (css c >>= writeBody) >> addHeader "Content-Type" "text/css"

svgFile :: (MonadIO m) => SVG.Svg -> RouteT AppState m ()
svgFile s = writeBody s >> addHeader "Content-Type" "image/svg+xml"

getPageNumber :: (WebAppState s, MonadIO m) => RouteT s m Integer
getPageNumber = fromMaybe 0 <$> maybeParam "page"

renderHtmlM :: (WebAppState s, Monad m) => RouteT s m HTML.Html -> RouteT s m ()
renderHtmlM act = act >>= renderHtml

renderHtml :: (WebAppState s, Monad m) => HTML.Html -> RouteT s m ()
renderHtml html = addHeader "Content-Type" "text/html" >> writeBody html