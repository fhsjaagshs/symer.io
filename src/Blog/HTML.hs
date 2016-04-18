{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

{-|
Module      : Blog.HTML
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

HTML pages for the blog
-}

module Blog.HTML
(
  -- * HTML Pages
  root,
  drafts,
  postDetail,
  postsByTag,
  postEditor,
  login,
  internalError,
  notFound,
  -- * Exported Types
  Html
)
where
  
import Blog.User
import Blog.Post
import Blog.Comment
import Blog.HTML.Common
import Blog.HTML.CSS as CSS
import Blog.HTML.SVG as SVG
import Blog.Util.Markdown

import Web.App.Stream

import Data.Niagra (css')

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Data.Maybe
import Data.Monoid
import Data.String
import Control.Monad

import Cheapskate
import Cheapskate.Html

import Data.Time.Format

import Prelude as P hiding (head,div,id)
import qualified Text.Blaze.Html5 as H (title,body,head)
import Text.Blaze.Html5 as H hiding (style,param,map,title,body,head)
import qualified Text.Blaze.Html5 as H (style)
import Text.Blaze.Html5.Attributes as A hiding (title)
import Text.Blaze.Renderer.Utf8

instance ToStream Html where
  stream = stream . renderMarkupBuilder

root :: (Maybe User) -> [Post] -> Integer -> Html
root user posts pageNumber = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title "Nate Symer"
    renderMeta "description" "Nate Symer website & blog. Nate is a software engineer & designer."
    renderMeta "keywords" $ toValue $ T.intercalate ", " keywords
  H.body $ do
    renderHeader user True Nothing
    mapM_ (renderPost True user Nothing) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)

drafts :: User -> [Post] -> Integer -> Html
drafts user posts pageNumber = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title "Drafts"
    renderMeta "robots" "noindex, nofollow"
  H.body $ do
    renderHeader (Just user) False (Just "Drafts")
    mapM_ (renderPost True (Just user) Nothing) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)

postDetail :: (Maybe User) -> Post -> [Comment] -> Html
postDetail user pst@(Post pid title _ _ tags _ _) commnts = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title $ toHtml title
    H.style $ toHtml $ css' CSS.comments
    renderMeta "keywords" $ toValue $ T.intercalate ", " $ tags ++ keywords
    renderMeta "description" $ toValue $ postDescription pst
  H.body $ do
    renderHeader user True Nothing
    renderPost False user Nothing pst
    commentEditor pid Nothing
    mapM_ (renderComment 0) commnts
    where
      replyOffset = 50
      renderComment depth (Comment cid _ _ body _ children) = do
        H.div
          ! class_ "comment"
          ! A.id (fromString $ "comment" <> show cid)
          ! style (fromString $ "margin-left:" <> (show (depth*replyOffset)) <> "px;") $ do
            H.p ! class_ "comment-body" $ toHtml body
            H.details $ do
              H.summary "Reply"
              commentEditor pid (Just cid)
        mapM_ (renderComment $ depth + 1) children

commentEditor :: (Show a, Integral a) => a -> Maybe a -> Html
commentEditor postid parentid = do
  containerDiv $ do
    submitForm $ do
      when (isJust parentid) $ do
        input ! type_ "hidden" ! name "parent_id" ! value (fromString $ show $ fromJust parentid)
      textarea ! A.form formname ! name "body" ! class_ "comment-textarea textarea" ! placeholder "Write a comment" $ ""
      input ! class_ "gobutton" ! type_ "submit" ! value ""
  where
    containerDiv = div ! class_ "editor"
                       ! style (fromString $ "text-align:center;display:block")
    submitForm = H.form ! id formname ! action (fromString $ "/posts/" <> (show postid) <> "/comments") ! method "POST"
    formname = fromString $ "form" <> (show postid) <> fromMaybe "" (show <$> parentid)

postsByTag :: (Maybe User) -> Text -> [Post] -> Integer -> Html
postsByTag user tag posts pageNum = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title $ toHtml tag
    renderMeta "robots" "noindex, nofollow"
  H.body $ do
    renderHeader user True Nothing
    mapM_ (renderPost True user (Just tag)) (take postsPerPage posts)
    renderPageControls pageNum (length posts > postsPerPage)
    
postEditor :: Maybe Post -> Html
postEditor Nothing = renderEditor Nothing "" "" [] True
postEditor (Just (Post pid t bdy _ tg d _)) = renderEditor (Just pid) t bdy tg d

renderEditor :: Maybe Integer -> T.Text -> T.Text -> [T.Text] -> Bool -> Html
renderEditor pid title body tags draft = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title $ toHtml $ maybe "New Post" (const title) pid
    renderMeta "robots" "noindex, nofollow"
    H.style $ toHtml $ css' CSS.editor
    H.style $ toHtml $ css' CSS.wordlist
  H.body $ do
    renderHeader Nothing False Nothing
    upsertForm pid
    deleteForm pid
    renderScript "/assets/js/wordlist-pure.js"
    script $ mconcat [
      "var e=document.getElementById('editor');",
      "var i=document.getElementById('input-tags');",
      "e.parentNode.insertBefore((new WordList(i)).div,e.nextSibling);"]
  where
    deleteForm Nothing = return ()
    deleteForm (Just pid') = do
      H.form ! id "delete-form" ! action "/posts" ! method "POST" $ do
        input ! type_ "hidden" ! name "id"     ! value (toValue pid')
        input ! type_ "hidden" ! name "method" ! value "DELETE"
        input ! id "delete-button" ! class_ "button" ! type_ "submit" ! value "Delete"
    upsertForm pid' = H.form ! id "post-form" ! action "/posts" ! method "POST" $ f pid'
      where
        f (Just pid'') = do
          input ! type_ "hidden" ! name "id" ! value (toValue pid'')
          f Nothing
        f Nothing = do
          input    ! id "input-tags"  ! type_ "hidden" ! name "tags"  ! value (toValue . T.intercalate "," $ tags)
          input    ! id "title-field" ! type_ "text"   ! name "title" ! value (toValue title) ! placeholder "Post title" 
          textarea ! id "editor" ! A.form "post-form"  ! name "body" $ toHtml body
    
          H.label ! id "checkbox" ! for "public" $ do
            if draft
              then input ! type_ "checkbox" ! id "public" ! A.form "post-form" ! name "draft"
              else input ! type_ "checkbox" ! id "public" ! A.form "post-form" ! name "draft" ! A.checked ""
            toHtml $ ("Public" :: String)
            
          input ! id "save-button" ! class_ "button" ! type_ "submit" ! value "Save"
       
login :: Maybe Text -> Html
login merrmsg = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title "Login"
    renderMeta "robots" "noindex, nofollow"
  H.body $ do
    renderHeader Nothing False Nothing
    h1 ! class_ "title" $ "Login"
    when (isJust merrmsg) (h3 ! class_ "subtitle" $ toHtml $ fromJust merrmsg)
    H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
      input ! type_ "hidden" ! A.name "source" ! value "form"
      renderTextField False "username" ! placeholder "Username"
      renderTextField True "password" ! placeholder "Password"
      input ! id "submit" ! class_ "button" ! type_ "submit" ! value "Login"

internalError :: Text -> Html
internalError err = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title "Internal Error"
    renderMeta "robots" "noindex, nofollow"
  H.body $ do
    renderHeader Nothing True Nothing
    h1 ! class_ "title" $ "Something happened..."
    h3 ! class_ "subtitle" $ toHtml err

-- TODO: add picture of fudge
notFound :: Html
notFound = docTypeHtml $ do
  H.head $ do
    pageAttributes
    H.title "Not Found"
    renderMeta "robots" "noindex, nofollow"
  H.body $ do
    renderHeader Nothing True Nothing
    h1 ! class_ "title" $ "Oh fudge!"
    h3 ! class_ "subtitle" $ "The page you're looking for does not exist."
    
--------------------------------------------------------------------------------
-- |render a 'Post'
renderPost :: Bool -- ^ Whether or not to truncate the post body
           -> Maybe User -- ^ Whether or not the user is authenticated
           -> Maybe Text -- ^ The currently selected tag
           -> Post -- ^ The post to render
           -> Html
renderPost short user tag (Post pid title body ts tags _ (User aid aun _)) = do
  div ! class_ "post" $ do
    div ! class_ "post-header" $ do
      div ! class_ "post-headerbox" $ do
        a ! href postURL $ do
          h1 ! class_ "post-title"
             ! id (toValue pid)
             $ toHtml title
        h4 ! class_ "post-subtitle" $ toHtml $ subtitle
      div ! class_ "post-headerbox" $ mapM_ taglink tags
      when (maybe False ((== aid) . userUID) user) $ do
        a ! class_ "post-edit-button"
          ! rel "nofollow"
          ! href (toValue $ "/posts/" ++ show pid ++ "/edit")
          $ "edit"
    div ! class_ "post-content" $ renderContent short
  where
    -- values
    postURL = toValue $ "/posts/" ++ show pid
    timeFormat = "%-m • %-e • %-y  " ++ (T.unpack aun)
    subtitle = formatTime defaultTimeLocale timeFormat ts
    -- functions
    taglink t = a ! class_ (if maybe False ((==) t . TL.toStrict) tag
                              then "taglink selected-tag"
                              else "taglink")
                  ! href (toValue $ T.append "/posts/by/tag/" t)
                  $ toHtml t
    renderContent False = renderDoc $ markdown def body
    renderContent True  = do
      renderDoc . truncateMarkdown 500 . markdown def $ body
      a ! class_ "read-more" ! href postURL $ "read more..."
      
-- |Default keywords.
keywords :: [T.Text]
{-# INLINE keywords #-}
keywords = ["nate", "nathaniel", "symer", "computer", "science", "software",
            "functional", "programming", "web", "haskell", "ruby", "art",
            "studio", "lean", "startup"]
            
pageAttributes :: Html
{-# INLINE pageAttributes #-}
pageAttributes = do
  renderMeta "revisit-after" "2 days"
  renderMeta "viewport" "width=device-width,initial-scale=1"
  link ! rel "icon" ! type_ "image/png" ! href "/assets/images/favicon.png"
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  H.style $ toHtml $ css' CSS.blog

-- |Render the page header. Contains things like the "about" information,
-- the admin controls, etc
renderHeader :: (Maybe User) -- ^ the authenticated user
             -> Bool -- ^ whether or not the "about" info is rendered
             -> Maybe Html -- ^ an optional title
             -> Html
renderHeader user showAbout ttl = do
  div ! class_ (stringValue $ "header" ++ (if not showAbout then " nopadding" else "")) $ do
    a ! href "/" $ SVG.phillySkyline
      
    when showAbout $ do
      h1 ! class_ "title" ! A.id "name-title" $ "NATE SYMER"
      h3 ! class_ "subtitle" $ "artisinal software development"
      h3 ! class_ "tagline" $ "nate@symer.io • 856-419-7654"
      
    renderTitle ttl
    renderControls user
  where
    renderTitle v = maybe (return ()) (h1 ! class_ "title") v
    renderControls Nothing = return ()
    renderControls (Just (User _ uname _)) = do
      renderButton ! href "/logout"    $ "Log Out"
      renderButton ! href "/posts/new" $ "New Post"
      renderButton ! href "/drafts"    $ "Drafts"
      h3 ! class_ "tagline" $ toHtml $ mconcat ["Logged in as ",uname]
    

renderPageControls :: Integer -> Bool -> Html
renderPageControls pageNum hasNext = do
  when (pageNum > 0)  $ renderButton ! A.id "prevbutton" ! href (mkHref $ pageNum-1) $ "Newer"
  when hasNext        $ renderButton ! A.id "nextbutton" ! href (mkHref $ pageNum+1) $ "Older"
  where mkHref = toValue . (++) "/?page=" . show