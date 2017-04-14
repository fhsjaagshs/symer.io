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

import Data.Maybe
import Data.Niagra (NiagraT(..),css)
 
import Control.Monad
import Control.Monad.IO.Class

import qualified Crypto.BCrypt as BCrypt

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B

-- TODO
-- 1. Kill comments
-- 2. Site footer (all posts copyright Nathaniel Symer)

-- TODO features:
-- Comment-optional posts
-- Editor key commands (cmd-i, cmd-b, etc)
-- Page numbers at bottom (would require extra db hit)
-- search

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?
-- 2. JSON API

--------------------------------------------------------------------------------

app :: [Route AppState IO]
app = [
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
          redirect "/"
        else redirect "/login?err=Invalid%20password%2E"
        
getLogin :: (MonadIO m) => RouteT AppState m ()
getLogin = do
  maybeUser <- getAuthenticatedUser
  when (isJust maybeUser) $ redirect "/"
  maybeParam "err" >>= renderHtml . HTML.login

postPosts :: (MonadIO m) => RouteT AppState m ()
postPosts = do
  m <- fromMaybe "POST" <$> maybeParam "method" 
  handleMethod (m :: B.ByteString)
  where
    handleMethod "DELETE" = do
      pid <- join $ deletePost <$> authenticate <*> param "id"
      maybe (status status404) (const $ redirect "/") pid
    handleMethod _ = do
      p <- join $ upsertPost
                    <$> (maybeParam "id")
                    <*> ((fmap $ T.decodeUtf8 . uncrlf) <$> maybeParam "title")
                    <*> ((fmap $ T.decodeUtf8 . uncrlf) <$> maybeParam "body")
                    <*> ((fmap $ T.splitOn ",")         <$> maybeParam "tags")
                    <*> ((fmap not)                     <$> maybeParam "draft")
                    <*> authenticate
      maybe (status status400) (redirect . B.pack . (++) "/posts/" . show) p
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
postComments = doInsert >>= maybe (status status500) (\c -> (param "id" >>= redirectPost (commentID c)))
  where
    doInsert = join $ insertComment
                      <$> maybeParam "parent_id"
                      <*> param "id"
                      <*> param "body"
    redirectPost :: (MonadIO m) => Integer -> Integer -> RouteT AppState m ()
    redirectPost cid pid = redirect $ B.pack $ "/posts/" ++ show pid ++ "#comment" ++ show cid
  
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