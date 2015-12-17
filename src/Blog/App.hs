{-# LANGUAGE OverloadedStrings #-}

module Blog.App
(
  app
)
where

import Blog.Postgres
import Blog.User
import Blog.Post
import Blog.Comment
import Blog.Web.Auth
import qualified Blog.HTML as HTML
import qualified Blog.HTML.CSS as CSS

import Web.App.Assets

import Data.Maybe
       
import Control.Monad

import Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status (Status(..))

import qualified Crypto.BCrypt as BCrypt

import Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

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

-- type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

--------------------------------------------------------------------------------

app :: PostgresScottyM ()
app = do
  get "/" $ do
    maybeUser <- getAuthenticatedUser
    pageNum <- getPageNumber
    posts <- getPosts pageNum
    renderHtml $ HTML.root maybeUser posts pageNum
        
  get "/drafts" $ do
    maybeUser <- authenticate
    pageNum <- getPageNumber
    drafts <- getDrafts (fromJust maybeUser) pageNum
    renderHtml $ HTML.drafts maybeUser drafts pageNum
        
  -- view a specific post
  get "/posts/:id" $ do
    mpost <- param "id" >>= getPost
    case mpost of
      Nothing -> redirect "/notfound"
      Just pst@(Post _ _ _ _ _ draft author) -> do
        maybeUser <- getAuthenticatedUser
        when (draft && (maybe True ((/=) author) maybeUser)) (redirect "/notfound")
        renderHtml $ HTML.postDetail maybeUser pst
            
  get "/posts/by/tag/:tag" $ do
    tag <- param "tag"
    maybeUser <- getAuthenticatedUser
    pageNum <- getPageNumber
    posts <- getPostsByTag tag pageNum
    renderHtml $ HTML.postsByTag maybeUser (TL.fromStrict tag) posts pageNum
        
  -- create a post
  get "/posts/new" $ do
    void $ authenticate
    renderHtml $ HTML.postEditor Nothing

  -- edit a post
  get "/posts/:id/edit" $ do
    void $ authenticate
    pst <- param "id" >>= getPost
    case pst of
      Nothing -> next
      Just _ -> renderHtml $ HTML.postEditor pst

  get "/login" $ do
    maybeUser <- getAuthenticatedUser
    when (isJust maybeUser) (redirect "/")
    params >>= renderHtml . HTML.login . lookup "err"

  get "/logout" $ deleteAuth >> redirect "/"
  
  post "/login" $ do
    mUser <- param "username" >>= getUser
    case mUser of
      Nothing -> redirect "/login?err=Username%20does%20not%20exist%2E"
      Just user@(User _ _ _ phash) -> do
        pPassword <- T.encodeUtf8 <$> param "password"
        if BCrypt.validatePassword (T.encodeUtf8 phash) pPassword
          then do
            setAuthenticatedUser user
            params >>= redirect . fromMaybe "/" . lookup "redirect"
          else redirect "/login?err=Invalid%20password%2E"

  post "/posts" $ postPosts
    
  post "/posts/:id/comments" $ do
    postId <- param "id"
    email <- param "email"
    displayName <- param "display_name"
    bdy <- param "body"
    parentId <- fmap (read . TL.unpack) . lookup "parent_id" <$> params
    mcomment <- insertComment parentId postId email displayName bdy
    case mcomment of
      Nothing -> Scotty.status $ Status 500 "Failed to insert comment." -- TODO: revisit this
      Just comment -> Scotty.json $ comment

  get "/posts/:id/comments.json" $ param "id" >>= getCommentsForPost >>= Scotty.json
  get "/assets/css/blog.css" $ cssFile CSS.blog
  get "/assets/css/comments.css" $ cssFile CSS.comments
  get "/assets/css/editor.css" $ cssFile CSS.editor
  get "/assets/css/wordlist.css" $ cssFile CSS.wordlist
  get (regex "/assets/(.*)") $ param "1" >>= loadAsset

  defaultHandler $ renderHtml . HTML.internalError
  notFound $ renderHtml $ HTML.notFound
  
cssFile :: TL.Text -> PostgresActionM ()
cssFile c = Scotty.raw (TL.encodeUtf8 c) >> Scotty.setHeader "Content-Type" "text/css"
  
postPosts :: PostgresActionM ()
postPosts = authenticate >>= handleAuth
  where
    handleAuth Nothing     = addHeader "Location" "/login"
    handleAuth (Just user) = params >>= handleParams user . filter ((<) 0 . TL.length . snd)
    handleParams user ps = handleMethod user ps (lookup "method" ps) (read . TL.unpack <$> lookup "id" ps)
    handleMethod user _ (Just "DELETE") (Just pid) = deletePost user pid >>= handleDelete
    handleMethod user ps _ pid = upsertPost pid title bdy tags draft user >>= handleUpsert
      where title = maybe "" TL.toStrict                        $ lookup "title" ps
            bdy   = maybe "" TL.toStrict                        $ lookup "body" ps
            tags  = maybe [] (map TL.toStrict . TL.splitOn ",") $ lookup "tags" ps
            draft = maybe True truthy                           $ lookup "draft" ps
    handleUpsert Nothing = status $ Status 400 "Missing required parameters"
    handleUpsert (Just postId) = do
      status $ Status 302 ""
      addHeader "Location" $ TL.pack $ "/posts/" ++ (show postId)
    handleDelete Nothing = status $ Status 404 "Failed to find post to delete."
    handleDelete (Just _) = (status $ Status 302 "") >> (addHeader "Location" "/")
          
    
-- setCacheControl :: ActionT e WebM ()
-- setCacheControl = Scotty.setHeader "Cache-Control" ccontrol
--   where
--     ccontrol = "public,max-age=3600,s-max-age=3600,no-cache,must-revalidate,proxy-revalidate,no-transform"

truthy :: (Eq a, IsString a) => a -> Bool
truthy "t" = True
truthy "y" = True
truthy "true" = True
truthy "True" = True
truthy _ = False

getPageNumber :: (ScottyError e, Monad m) => ActionT e m Integer
getPageNumber = maybe 0 (read . TL.unpack) . lookup "page" <$> params

renderHtml :: (ScottyError e, Monad m) => HTML.Html -> ActionT e m ()
renderHtml = Scotty.html . R.renderHtml