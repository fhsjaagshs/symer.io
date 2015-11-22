{-# LANGUAGE OverloadedStrings #-}

module Blog.App
(
  app
)
where

import Blog.State
import Blog.User
import Blog.Post as Post
import Blog.Database.Config
import Blog.Database.Util
import Blog.Util.HTML
import Blog.Util.MIME
import Blog.Util.Env
import Blog.Web.Auth
import qualified Blog.System.FileCache as FC

import Data.Maybe
       
import Control.Monad
import Control.Monad.IO.Class
       
import Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status (Status(..))

import qualified Crypto.BCrypt as BCrypt
import qualified Crypto.Hash.MD5 as MD5 (hashlazy)

import Data.String
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

import Text.Blaze.Html5 as H hiding (style, param, map)
import Text.Blaze.Html5.Attributes as A
import qualified Text.CSS.Parse as CSS (parseNestedBlocks)
import qualified Text.CSS.Render as CSS (renderNestedBlocks)
import qualified Text.Jasmine as JS (minifym)

import System.Directory

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
app :: ScottyT TL.Text WebM ()
app = do
  get "/" $ do
    maybeUser <- getUser
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getPosts mPageNum
    beginHtml $ do
      renderHead blogTitle $ do
        renderMeta "description" description
        renderKeywords keywords
      renderBody $ do
        renderTitle blogTitle
        renderSubtitle blogSubtitle
        when (isJust maybeUser) renderAdminControls
        mapM_ (renderPost True maybeUser) (take postsPerPage posts)
        renderPageControls mPageNum (length posts > postsPerPage)
        
  get "/drafts" $ do
    maybeUser <- authenticate
    mPageNum <- fmap (read . TL.unpack) . lookup "page" <$> params
    posts <- getDrafts (fromJust maybeUser) mPageNum
    beginHtml $ do
      renderHead (appendedBlogTitle "Drafts") $ do
        renderMeta "robots" "noindex, nofollow"
      renderBody $ do
        renderTitle "Drafts"
        when (isJust maybeUser) renderAdminControls
        mapM_ (renderPost True maybeUser) (take postsPerPage posts)
        renderPageControls mPageNum (length posts > postsPerPage)
        
  -- create a post
  get "/posts/new" $ do
    authenticate
    beginHtml $ do
      renderHead (appendedBlogTitle "New Post") $ do
        renderMeta "robots" "noindex, nofollow"
        renderStylesheet "/assets/css/editor.css"
        renderStylesheet "/assets/css/wordlist.css"
      renderBody $ do
        renderPostEditor Nothing

  -- view a specific post
  get "/posts/:id" $ do
    mpost <- param "id" >>= getPost
    case mpost of
      Nothing -> redirect "/notfound"
      Just pst@(Post _ ttl _ _ tags draft author) -> do
        maybeUser <- getUser
        when (draft && (maybe True ((/=) author) maybeUser)) (redirect "/notfound")

        beginHtml $ do
          renderHead (appendedBlogTitle $ TL.fromStrict ttl) $ do
            renderKeywords $ tags ++ keywords
            renderMeta "description" $ TL.fromStrict $ postDescription pst
          renderBody $ do
            renderTitle blogTitle
            renderSubtitle blogSubtitle
            when (isJust maybeUser) renderAdminControls
            renderPost False maybeUser pst
            renderScript "/assets/js/common.js"
            renderScript "/assets/js/comments.js"
            
  get "/posts/by/tag/:tag" $ do
    tag <- param "tag"
    maybeUser <- getUser
    mPageNum <- (fmap (read . TL.unpack) . lookup "page") <$> params
    posts <- getPostsByTag tag mPageNum
    beginHtml $ do
      renderHead (appendedBlogTitle $ TL.fromStrict tag) $ do
        renderMeta "description" description
        renderKeywords keywords
      renderBody $ do
        renderTitle $ mconcat ["Posts tagged '", TL.fromStrict tag, "'"]
        when (isJust maybeUser) renderAdminControls
        mapM_ (renderPost True maybeUser) (take postsPerPage posts)
        renderPageControls mPageNum (length posts > postsPerPage)

  -- edit a post
  get "/posts/:id/edit" $ do
    authenticate
    identifier_ <- param "id"
    res <- getPost identifier_
    case res of
      Nothing -> next
      Just post_ -> beginHtml $ do
        renderHead (appendedBlogTitle $ TL.fromStrict $ postTitle post_) $ do
          renderMeta "robots" "noindex, nofollow"
          renderStylesheet "/assets/css/editor.css"
          renderStylesheet "/assets/css/wordlist.css"
        renderBody $ do
          renderPostEditor $ Just post_

  get "/login" $ do
    maybeUser <- getUser
    when (isJust maybeUser) (redirect "/")
    merrmsg <- lookup "err" <$> params
    beginHtml $ do
      renderHead (appendedBlogTitle "Login") $ do
        renderMeta "robots" "noindex, nofollow"
      renderBody $ do
        renderTitle "Login"
        when (isJust merrmsg) (renderSubtitle $ fromJust merrmsg)
        H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
          input ! type_ "hidden" ! A.name "source" ! value "form"
          renderInput "text" ! A.id "username" ! placeholder "Username" ! A.name "username"
          renderInput "password" ! A.id "password" ! placeholder "Password" ! A.name "password"
        renderButton "Login" "submit" Nothing
        renderScript "/assets/js/login.js"

  get "/logout" $ deleteAuth >> redirect "/"
  
  post "/login" $ do
    mUser <- param "username" >>= getUserWithUsername
    case mUser of
      Nothing -> redirect "/login?err=Username%20does%20not%20exist%2E"
      Just user@(User _ _ _ (Just phash)) -> do
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
      Just authUser -> do
        ps <- params
        let pid = read . TL.unpack <$> lookup "id" ps
            ptitle   = TL.toStrict <$> lookup "title" ps
            pbody    = TL.toStrict <$> lookup "body" ps
            ptags    = map TL.toStrict . TL.splitOn "," <$> lookup "tags" ps
            pdeltags = map TL.toStrict . TL.splitOn "," <$> lookup "deleted_tags" ps
            pisdraft = maybe True truthy $ lookup "draft" ps
        mPostID <- upsertPost authUser pid ptitle pbody ptags pdeltags pisdraft
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
  get (regex "/favicon.*") $ loadAsset "images/philly_skyline.svg"

  defaultHandler $ \err -> do
    beginHtml $ do
      renderHead (appendedBlogTitle "Internal Error") $ do
        renderMeta "robots" "noindex, nofollow"
      renderBody $ do
        renderTitle "Something happened..."
        renderSubtitle err

  -- TODO: add picture of fudge
  notFound $ do
    beginHtml $ do
      renderHead (appendedBlogTitle "Not Found") $ do
        renderMeta "robots" "noindex, nofollow"
      renderBody $ do
        renderTitle "Oh fudge!"
        renderSubtitle "The page you're looking for does not exist."

loadAsset :: (ScottyError e) => FilePath -> ActionT e WebM ()
loadAsset assetsPath = do
  cache <- webM $ gets stateCache
  exists <- liftIO $ doesFileExist relPath
  if not exists
    then doesntExist $ B.pack relPath
    else loadFromCache cache
  where
    mimetype = getMimeAtPath relPath -- TODO: get mimetype from file contents (or use OS)
    relPath = "assets/" ++ assetsPath
    doesntExist pth = status . Status 404 $ mconcat ["File ", pth, " does not exist."]
    loadFromCache cache = (liftIO $ FC.lookup cache assetsPath) >>= (f cache)
    f _     (Just (Left err)) = status . Status 500 . B.pack $ err
    f _     (Just (Right cached)) = do
      setHeader "Content-Type" $ mconcat [TL.pack mimetype, "; charset=utf-8"]
      setBody $ BL.fromStrict cached
    f cache Nothing = do
      liftIO $ FC.register' cache assetsPath (g mimetype)
      loadFromCache cache
    builderToBS = BL.toStrict . TL.encodeUtf8 . TL.toLazyText
    g "application/javascript" = fmap BL.toStrict . JS.minifym . BL.fromStrict
    g "text/css" = fmap (builderToBS . CSS.renderNestedBlocks) . CSS.parseNestedBlocks . T.decodeUtf8
    g _ = Right

setBody :: (ScottyError e) => BL.ByteString -> ActionT e WebM ()
setBody str = do
  production $ Scotty.setHeader "Cache-Control" ccontrol
  -- Scotty.setHeader "Vary" "Accept-Encoding"
  Scotty.header "If-None-Match" >>= f . maybe False (== hashSum)
  where
    ccontrol = "public,max-age=3600,s-max-age=3600,no-cache,must-revalidate,proxy-revalidate,no-transform"
    calcMD5sum = BL.fromStrict . B16.encode . MD5.hashlazy
    hashSum = TL.decodeUtf8 . calcMD5sum $ str
    f True = Scotty.status $ Status 304 ""
    f False = Scotty.setHeader "ETag" hashSum >> Scotty.raw str

renderKeywords :: [T.Text] -> Html
renderKeywords = renderMeta "keywords" . TL.fromStrict . T.intercalate ", "

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

blogTitle :: TL.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: TL.Text
blogSubtitle = "a blog about code."

appendedBlogTitle :: TL.Text -> TL.Text
appendedBlogTitle s = mconcat [s, " | ", blogTitle]

description :: TL.Text
description = "Rants and raves about functional programming, politics, and everything in between."