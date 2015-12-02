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

import Web.App.Assets

import Data.Maybe
       
import Control.Monad

import Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status (Status(..))

import qualified Crypto.BCrypt as BCrypt

import Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

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

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    auth <- authenticate
    case auth of
      Nothing -> status $ Status 401 "Missing authentication"
      Just authUser -> do
        ps <- params
        let pid      = read . TL.unpack <$> lookup "id" ps
            ptitle   = TL.toStrict <$> lookup "title" ps
            pbody    = TL.toStrict <$> lookup "body" ps
            ptags    = map TL.toStrict . TL.splitOn "," <$> lookup "tags" ps
            pisdraft = maybe True truthy $ lookup "draft" ps
        mPostID <- upsertPost authUser pid ptitle pbody ptags pisdraft
        case mPostID of
          Nothing -> status $ Status 400 "Missing required parameters"
          Just postId -> addHeader "Location" $ TL.pack $ "/posts/" ++ (show postId)

  -- deletes a BlogPost from the database
  delete "/posts/:id" $ do
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
      Nothing -> Scotty.status $ Status 500 "Failed to insert comment."
      Just commentId -> do
        addHeader "Location" $ mconcat ["/posts/", TL.pack $ show postId]
        Scotty.text . TL.pack . show $ commentId

  get "/posts/:id/comments.json" $ param "id" >>= getCommentsForPost >>= Scotty.json
  get (regex "/assets/(.*)") $ param "1" >>= loadAsset

  defaultHandler $ renderHtml . HTML.internalError
  notFound $ renderHtml $ HTML.notFound
    
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