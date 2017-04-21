{-# LANGUAGE OverloadedStrings, BangPatterns #-}

{-|
Module      : Blog.Page
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

HTML page abstraction.
-}

module Blog.Page
(
  Head(..),
  Section(..),
  page,
  cssFile,
  svgFile
)
where
  
import Blog.AppState
import Blog.User
import Blog.Post
import Blog.Comment
import Blog.CSS as CSS (blog)
import Blog.SVG as SVG
import Blog.Util.Markdown

import Web.App hiding (body)
import Data.Niagra (NiagraT, css, css')

import Data.Text (Text)
import qualified Data.Text as T

import Data.Bool
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class

import Data.Time.Format

import Cheapskate (markdown, def)
import Cheapskate.Html (renderDoc)
import Prelude as P hiding (head, div)
import qualified Text.Blaze.Html5 as H (title, body, head)
import Text.Blaze.Html5 as H hiding (style, param, map, title, body, head)
import qualified Text.Blaze.Html5 as H (style)
import Text.Blaze.Html5.Attributes as A hiding (title)
import Text.Blaze.Renderer.Utf8

-- | Data structure abstracting the <head> section.
data Head = Head {
  _headTitle :: Text,
  _headDesc :: Text,
  _headKeywords :: [Text],
  _headHidden :: !Bool,
  _headStylesheets :: [Text] -- contains rendered CSS
}
    
-- | A section of a webpage on the blog
data Section = Header {
  _shShort :: Bool,
  _shTitle :: Maybe Text
}
             | PageControls {
  _spcShowNext :: Bool,
  _spcPageNumber :: Integer
}
             | PostRepr {
  _sprShort :: Bool,
  _sprPost :: Post
}
             | Footer {
  _sfString :: Text
}
             | Editor {
  _sePost :: Maybe Post
}
             | Login {
  _slError :: Maybe Text,
  _slUsername :: Maybe Text
}
             | Error {
  _seError :: Text
}
             | CommentEditor {
  _scePostId :: Integer,
  _sceParentId :: Maybe Integer
}
             | CommentRepr {
  _scrDepth :: Integer,
  _scrReprComment :: Comment
}

cssFile :: (MonadIO m) => NiagraT (RouteT AppState m) () -> RouteT AppState m ()
cssFile c = (css c >>= writeBody) >> addHeader "Content-Type" "text/css"

svgFile :: (MonadIO m) => SVG.Svg -> RouteT AppState m ()
svgFile s = writeBody (renderMarkupBuilder s) >> addHeader "Content-Type" "image/svg+xml"

page :: (MonadIO m) => Head -> [Section] -> RouteT AppState m ()
page h secs = do
  addHeader "Content-Type" "text/html"
  htmlDoc <- (<>) (headToHtml h) <$> (H.body . mconcat <$> mapM sectionToHtml secs)
  writeBody $ stream True $ renderMarkupBuilder $ docTypeHtml htmlDoc

headToHtml :: Head -> Html
headToHtml (Head t desc kws hide styles) = H.head $ do
  H.title $ toHtml t
  link ! rel "icon" ! type_ "image/png" ! href "/assets/images/favicon.png"
  meta ! name "keywords"             ! content (toValue $ T.intercalate ", " kws)
  meta ! name "description"          ! content (toValue desc)
  meta ! name "revisit-after"        ! content "2 days"
  meta ! name "viewport"             ! content "width=device-width,initial-scale=1"
  meta ! httpEquiv "Content-Type"    ! content "text/html; charset=UTF-8"
  when hide $ meta ! name "robots" ! content "noindex, nofollow"
  H.style $ toHtml $ css' CSS.blog
  mapM_ (H.style . toHtml) styles

sectionToHtml :: (MonadIO m) => Section -> RouteT AppState m Html
sectionToHtml (Header short t) = do
  user <- getAuthenticatedUser
  pure $ div ! class_ headerClass $ do
    a ! href "/" $ SVG.phillySkyline
    maybe defaultHeader (\v -> h1 ! class_ "title" $ toHtml v) t
    maybe mempty (f . userUsername) user
  where headerClass = stringValue $ "header" ++ bool mempty " nopadding" short
        f uname = do
          a ! class_ "button" ! rel "nofollow" ! href "/logout"    $ "Log Out"
          a ! class_ "button" ! rel "nofollow" ! href "/posts/new" $ "New Post"
          a ! class_ "button" ! rel "nofollow" ! href "/drafts"    $ "Drafts"
          p ! class_ "tagline" $ toHtml uname  
        defaultHeader = when (not short) $ do
          h1 ! class_ "title" ! A.id "name-title" $ "NATE SYMER"
          h3 ! class_ "subtitle" $ "artisanal software development"
          h3 ! class_ "tagline" $ "nate@symer.io • 856-419-7654"
sectionToHtml (PageControls hasNext pageNum) = pure $ do
  when (pageNum > 0) $ a ! class_ "button" ! rel "nofollow" ! A.id "prevbutton" ! href (mkHref $ pageNum-1) $ "Newer"
  when hasNext       $ a ! class_ "button" ! rel "nofollow" ! A.id "nextbutton" ! href (mkHref $ pageNum+1) $ "Older"
  where mkHref = toValue . (++) "/?page=" . show
sectionToHtml (PostRepr short (Post pid title body ts tags _ (User aid aun _))) = do
  user <- getAuthenticatedUser
  pure $ div ! class_ "post" $ do
    div ! class_ "post-header" $ do
      div ! class_ "post-headerbox" $ do
        a ! class_ "post-title" ! href postURL ! A.id (toValue pid) $ toHtml title
        p ! class_ "post-subtitle" $ toHtml $ formatTime defaultTimeLocale timeFormat ts
      div ! class_ "post-headerbox" $ forM_ tags $ \t -> do
        a ! class_ "taglink" ! href (toValue $ "/posts/by/tag/" <> t) $ toHtml t
      when ((maybe False ((==) aid . userUID) user) && not short) $ do
        a ! class_ "post-edit-button" ! rel "nofollow" ! href (postURL <> "/edit") $ "edit"
    div ! class_ "post-content" $ do
      renderDoc $ bool P.id (truncateMarkdown 500) short $ markdown def body
      when short $ a ! class_ "read-more" ! href postURL $ "read more..."
  where postURL = toValue $ "/posts/" ++ show pid
        timeFormat = "%-m • %-e • %-y  " ++ T.unpack aun
sectionToHtml (Footer str) = pure $ H.span ! class_ "footer" $ toHtml str
sectionToHtml (Login merr uname) = pure $ do
  maybe (pure ()) (\v -> h3 ! class_ "subtitle" $ toHtml v) merr
  H.form ! A.id "loginform" ! action "/login" ! A.method "POST" $ do
    input ! type_ "hidden" ! A.name "source" ! value "form"
    textField "username" ! type_ "text" ! placeholder "Username" ! value (toValue $ fromMaybe mempty uname)
    textField "password" ! type_ "password" ! placeholder "Password"
    input ! A.id "submit" ! class_ "button" ! type_ "submit" ! value "Login"
  where
    textField n = input
                  ! class_ "textfield"
                  ! A.id n
                  ! A.name n
                  ! customAttribute "autocorrect" "off"
                  ! customAttribute "autocapitalize" "off"
                  ! customAttribute "spellcheck" "false"
sectionToHtml (Error err) = return $ do
  h1 ! class_ "title" $ "Whoops!"
  h3 ! class_ "subtitle" $ toHtml err
sectionToHtml (Editor pst) = return $ do
  H.form ! A.id "post-form" ! action "/posts" ! A.method "POST" $ do
    maybe (pure ()) idParam pst
    input    ! A.id "input-tags"  ! type_ "hidden" ! name "tags"  ! value (toValue $ T.intercalate "," $ maybe [] postTags pst)
    input    ! A.id "title-field" ! type_ "text"   ! name "title" ! value (toValue $ maybe mempty postTitle pst) ! placeholder "Post title" 
    textarea ! A.id "editor" ! A.form "post-form"  ! name "body" $ toHtml $ maybe mempty postBody pst
    
    H.label ! A.id "checkbox" ! for "public" $ do
      if maybe True postDraft pst
        then input ! type_ "checkbox" ! A.id "public" ! A.form "post-form" ! name "draft"
        else input ! type_ "checkbox" ! A.id "public" ! A.form "post-form" ! name "draft" ! A.checked ""
      "Public"
    
    input ! A.id "save-button" ! class_ "button" ! type_ "submit" ! value "Save"
  
  maybe (pure ()) deleteForm pst
  
  script ! src "/assets/js/wordlist-pure.js" $ ""
  script $ mconcat [
    "var e=document.getElementById('editor');",
    "var i=document.getElementById('input-tags');",
    "e.parentNode.insertBefore((new WordList(i)).div,e.nextSibling);"]
  where
    idParam pst' = input ! type_ "hidden" ! name "id" ! value (toValue $ postID pst')
    deleteForm pst' = H.form ! A.id "delete-form" ! enctype "text/plain" ! action "/posts" ! A.method "POST" $ do
      input ! type_ "hidden" ! name "id"     ! value (toValue $ postID pst')
      input ! type_ "hidden" ! name "method" ! value "DELETE"
      input ! A.id "delete-button" ! class_ "button" ! type_ "submit" ! value "Delete"
sectionToHtml (CommentEditor postid parentid) = pure $ do
  div ! class_ "editor" ! style "text-align:center;display:block" $ do
    H.form ! A.id fname
           ! action (toValue $ "/posts/" <> show postid <> "/comments")
           ! A.method "POST" $ do
      maybe (pure ()) parentIdInput parentid
      textarea ! A.form fname ! name "body" ! class_ "comment-textarea textarea" ! placeholder "Write a comment" $ ""
      input ! class_ "gobutton" ! type_ "submit" ! value ""
  where parentIdInput pid = input ! type_ "hidden" ! name "parent_id" ! value (toValue $ show pid)
        fname = toValue $ "form" <> show postid <> maybe "" show parentid
sectionToHtml (CommentRepr depth (Comment cid _ pid body _ cs)) = mconcat <$> ((:) <$> editorDiv <*> cs')
  where
    cs' = mapM (sectionToHtml . CommentRepr (depth + 1)) cs
    editorDiv = do
      editor <- sectionToHtml $ CommentEditor pid (Just cid)
      pure $ H.div
        ! class_ "comment"
        ! A.id (toValue $ "comment" <> show cid)
        ! style (toValue $ "margin-left:" <> (show (depth * 50)) <> "px;") $ do
          H.p ! class_ "comment-body" $ toHtml body
          H.details $ H.summary "Reply" <> editor