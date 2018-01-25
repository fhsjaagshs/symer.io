{-# LANGUAGE OverloadedStrings, BangPatterns, ExistentialQuantification #-}

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
  Header(..),
  PageControls(..),
  PostRepr(..),
  Footer(..),
  Login(..),
  Error(..),
  Editor(..),
  pack,
  page,
  cssFile,
  svgFile
)
where
  
import Blog.AppState
import Blog.User
import Blog.Post
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

data Sectionable = forall a . Section a => Sectionable a

pack :: Section a => a -> Sectionable
pack = Sectionable

class Section a where
  render :: Maybe User -> a -> Html

data Header = Header {
  _shShort :: Bool,
  _shShowUser :: Bool,
  _shTitle :: Maybe Text
} deriving (Show)

instance Section Header where
  render user (Header short showUser t) = do
    div ! class_ headerClass $ do
      a ! href "/" $ SVG.phillySkyline
      maybe defaultHeader (\v -> h1 ! class_ "title" $ toHtml v) t
      maybe mempty (f . userUsername) user
      when ((not showUser) || short) $ p ""
    where headerClass = stringValue $ "header" ++ bool mempty " nopadding" short
          f uname = when showUser $ do
              a ! class_ "button" ! rel "nofollow" ! href "/logout"    $ "Log Out"
              a ! class_ "button" ! rel "nofollow" ! href "/posts/new" $ "New Post"
              a ! class_ "button" ! rel "nofollow" ! href "/drafts"    $ "Drafts"
              p ! class_ "tagline" $ toHtml uname  
          defaultHeader = when (not short) $ do
              p ! class_ "title" ! A.id "name-title" $ "NATE SYMER"
              p ! class_ "subtitle" $ "[ Software Engineer ]"
              p ! class_ "tagline" $ "nate@symer.io • 856-419-7654"

data PageControls = PageControls {
  pageControlsShowNext :: Bool,
  pageControlsPageNumber :: Integer,
  pageControlsIsDrafts :: Bool
} deriving (Show)

instance Section PageControls where
  render _ (PageControls hasNext pageNum isdrafts) = do
    when (pageNum > 0) $ a ! class_ "button" ! rel "nofollow" ! A.id "prevbutton" ! href (mkhref (pageNum - 1)) $ "Newer"
    when hasNext       $ a ! class_ "button" ! rel "nofollow" ! A.id "nextbutton" ! href (mkhref (pageNum + 1)) $ "Older"
    where mkhref pg = toValue $ bool path (path ++ "?page=" ++ show pg) (pg /= 0)
          path = bool "/" "drafts" isdrafts

data PostRepr = PostRepr {
  _sprShort :: Bool,
  _sprPost :: Post
} deriving (Show)

instance Section PostRepr where
  render user (PostRepr short (Post pid title body ts tags _ (User aid aun _))) = do
    div ! class_ "post" $ do
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

data Footer = Footer {
  footerText :: Text
} deriving (Show)

instance Section Footer where
  render _ (Footer str) = H.span ! class_ "footer" $ toHtml str

data Login = Login {
  loginError :: Maybe Text,
  loginUsername :: Maybe Text
} deriving (Show)

instance Section Login where
  render _ (Login merr uname) = do
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

data Error = Error {
  errorMessage :: Text
} deriving (Show)

instance Section Error where
  render _ (Error err) = do
    h1 ! class_ "title" $ "Whoops!"
    h3 ! class_ "subtitle" $ toHtml err

data Editor = Editor {
  editorPost :: Maybe Post
} deriving (Show)

instance Section Editor where
  render _ (Editor pst) = do
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
    script ! src "/assets/js/editor.js" $ ""
    where
      idParam pst' = input ! type_ "hidden" ! name "id" ! value (toValue $ postID pst')
      deleteForm pst' = H.form ! A.id "delete-form" ! enctype "text/plain" ! action "/posts" ! A.method "POST" $ do
        input ! type_ "hidden" ! name "id"     ! value (toValue $ postID pst')
        input ! type_ "hidden" ! name "method" ! value "DELETE"
        input ! A.id "delete-button" ! class_ "button" ! type_ "submit" ! value "Delete"

cssFile :: (MonadIO m) => NiagraT (RouteT AppState m) () -> RouteT AppState m ()
cssFile c = (css c >>= writeBody) >> addHeader "Content-Type" "text/css"

svgFile :: (MonadIO m) => SVG.Svg -> RouteT AppState m ()
svgFile s = writeBody (renderMarkupBuilder s) >> addHeader "Content-Type" "image/svg+xml"

page :: (MonadIO m) => Head -> [Sectionable] -> RouteT AppState m ()
page h secs = do
  addHeader "Content-Type" "text/html"
  user <- getAuthenticatedUser
  let htmlDoc = headToHtml h <> (H.body $ mconcat $ map (renderSectionable user) secs)
  writeBody $ stream True $ renderMarkupBuilder $ docTypeHtml htmlDoc
  where
    renderSectionable user (Sectionable x) = render user x

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

