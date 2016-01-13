{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Blog.App
(
  app
)
where

import Blog.User
import Blog.Post
import Blog.Comment
import Blog.Web.Auth
import Blog.Assets
import Blog.AppState
import qualified Blog.HTML as HTML
import qualified Blog.HTML.CSS as CSS
import qualified Blog.HTML.SVG as SVG

import Web.App
import Network.HTTP.Types.Status
-- import Network.HTTP.Types.URI
import Blaze.ByteString.Builder.Char.Utf8

import Data.Maybe
 
import Control.Monad
import Control.Monad.IO.Class

import qualified Crypto.BCrypt as BCrypt

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as B

import Text.Blaze.Svg11 (Svg)
import qualified Text.Blaze.Html.Renderer.Text as R (renderHtml)

-- TODO features:
-- Comment-optional posts
-- Editor key commands (cmd-i, cmd-b, etc)
-- Page numbers at bottom (would require extra db hit)
-- Site footer (copyright etc)
-- Links in comments *** nofollow them
-- search

-- TODO internals:
-- figure out compression better

-- Miscellaneous Ideas:
-- 1. 'Top 5' tags map in side bar?

--------------------------------------------------------------------------------

app :: WebAppT AppState IO ()
app = do
  get "/"                                   $ getRoot
  get "/login"                              $ getLogin
  get "/logout"                             $ deleteAuth >> redirect "/"
  post "/login"                             $ postLogin
  get "/drafts"                             $ getPageDrafts
  post "/posts"                             $ postPosts
  get "/posts/new"                          $ authenticate >> (renderHtml $ HTML.postEditor Nothing)
  get (captured "/posts/:id")               $ getPagePostById
  get (captured "/posts/by/tag/:tag")       $ getPagePostsByTag
  get (captured "/posts/:id/edit")          $ getPageEditor
  post (captured "/posts/:id/comments")     $ postComments
  get (captured "/posts/:id/comments.json") $ param "id" >>= getCommentsForPost >>= writeJSON
  get "/assets/css/blog.css"                $ cssFile CSS.blog
  get "/assets/css/comments.css"            $ cssFile CSS.comments
  get "/assets/css/editor.css"              $ cssFile CSS.editor
  get "/assets/css/wordlist.css"            $ cssFile CSS.wordlist
  get "/assets/images/gobutton.svg"         $ svgFile SVG.goButton
  get "/assets/images/philly_skyline.svg"   $ svgFile SVG.phillySkyline
  get (regex "/assets/(.*)")                $ param "1" >>= loadAsset
  matchAll                                  $ renderHtml $ HTML.notFound
  
{- Route functions -}

getRoot :: (MonadIO m) => RouteT AppState m ()
getRoot = renderHtmlM $ HTML.root <$> getAuthenticatedUser
                                  <*> (getPageNumber >>= getPosts)
                                  <*> getPageNumber
  
getPageDrafts :: (MonadIO m) => RouteT AppState m ()
getPageDrafts = do
  user <- authenticate
  renderHtmlM $ HTML.drafts user <$> (getPageNumber >>= getDrafts user)
                                 <*> getPageNumber

postLogin :: (MonadIO m) => RouteT AppState m ()
postLogin = param "username" >>= getUser >>= f
  where
    f Nothing = redirect "/login?err=Username%20does%20not%20exist%2E"
    f (Just user@(User _ _ _ phash)) = do
      pPassword <- T.encodeUtf8 <$> param "password"
      if BCrypt.validatePassword (T.encodeUtf8 phash) pPassword
        then do
          setAuthenticatedUser user
          params >>= redirect . maybe "/" (fromMaybe "/") . lookup "redirect"
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
  handleMethod u m i
  where
    handleMethod :: (MonadIO m) => User -> Maybe String -> Maybe Integer -> RouteT AppState m ()
    handleMethod user (Just "DELETE") (Just p) = do
      pid <- deletePost user p
      case pid of
        Nothing -> do
          status status404
          writeBody "Failed to find post to delete."
        Just _ -> redirect "/"
    handleMethod user _ pid = do
      (title :: T.Text) <- (maybe "" $ T.decodeUtf8 . uncrlf) <$> maybeParam "title"
      bdy <- (maybe "" $ T.decodeUtf8 . uncrlf) <$> maybeParam "body"
      tags <- (maybe [] $ map T.decodeUtf8 . B.split ',') <$> maybeParam "tags"
      draft <- (maybe True (/= ("on" :: T.Text))) <$> maybeParam "draft"
      p <- upsertPost pid title bdy tags draft user
      case p of
        Nothing -> do
          status status400
          writeBody "Missing required parameters"
        Just p' -> redirect $ B.pack $ "/posts/" ++ show p'
      where uncrlf = B.foldl f B.empty
              where f bs '\n' = if B.last bs == '\r' then B.init bs else f bs '\n'
                    f bs c = B.snoc bs c
            --uncrlfT = TL.replace "\r\n" "\n"
            
getPagePostById :: (MonadIO m) => RouteT AppState m ()
getPagePostById = param "id" >>= getPost >>= f
  where
    f Nothing = redirect "/notfound"
    f (Just pst@(Post _ _ _ _ _ draft author)) = do
      maybeUser <- getAuthenticatedUser
      if draft && (maybe True (/= author) maybeUser)
        then redirect "/notfound"
        else renderHtml $ HTML.postDetail maybeUser pst
      
getPagePostsByTag :: (MonadIO m) => RouteT AppState m ()
getPagePostsByTag = renderHtmlM $ HTML.postsByTag <$> getAuthenticatedUser
                                                  <*> (TL.fromStrict <$> param "tag")
                                                  <*> (do
                                                    pn <- getPageNumber
                                                    tg <- param "tag"
                                                    getPostsByTag tg pn)
                                                  <*> getPageNumber
                                             
postComments :: (MonadIO m) => RouteT AppState m ()
postComments = doInsert >>= maybe errorOut writeJSON
  where
    doInsert = do
      p <- maybeParam "parent_id"
      i <- param "i"
      e <- param "email"
      dn <- param "display_name"
      b <- param "body"
      insertComment p i e dn b
    errorOut = status status500 >> writeBody "Failed to insert comment."
  
getPageEditor :: (MonadIO m) => RouteT AppState m ()
getPageEditor = do
  void $ authenticate
  param "id" >>= getPost >>= maybe next (renderHtml . HTML.postEditor . Just)
  
{- Helper Functions -}
  
-- setCacheControl :: ActionT e WebM ()
-- setCacheControl = Scotty.setHeader "Cache-Control" ccontrol
--   where
--     ccontrol = "public,max-age=3600,s-max-age=3600,no-cache,must-revalidate,proxy-revalidate,no-transform"

cssFile :: (MonadIO m) => T.Text -> RouteT AppState m ()
cssFile c = writeBody (fromText c) >> addHeader "Content-Type" "text/css"

svgFile :: (MonadIO m) => Svg -> RouteT AppState m ()
svgFile s = writeBody (fromLazyText $ R.renderHtml s) >> addHeader "Content-Type" "image/svg+xml"

getPageNumber :: (WebAppState s, MonadIO m) => RouteT s m Integer
getPageNumber = fromMaybe 0 <$> maybeParam "page"

renderHtmlM :: (WebAppState s, Monad m) => RouteT s m HTML.Html -> RouteT s m ()
renderHtmlM act = act >>= renderHtml

renderHtml :: (WebAppState s, Monad m) => HTML.Html -> RouteT s m ()
renderHtml html = do
  addHeader "Content-Type" "text/html"
  writeBody $ fromLazyText $ R.renderHtml html