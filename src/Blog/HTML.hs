{-# LANGUAGE OverloadedStrings #-}

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
import Blog.HTML.Common
import Blog.Util.Markdown

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Data.Maybe
import Control.Monad

import Cheapskate
import Cheapskate.Html

import Data.Time.Format

import Prelude as P hiding (head,div,id)
import qualified Text.Blaze.Html5 as H (title,body,head)
import Text.Blaze.Html5 as H hiding (style,param,map,title,body,head)
import Text.Blaze.Html5.Attributes as A hiding (title)

root :: (Maybe User) -> [Post] -> Integer -> Html
root user posts pageNumber = docTypeHtml $ do
  H.head $ do
    H.title "Nate Symer"
    renderMeta "description" "Nate Symer website & blog. Nate is a software engineer & designer."
    renderMeta "keywords" $ toValue $ T.intercalate ", " keywords
    pageAttributes
  H.body $ do
    renderHeader user True
    mapM_ (renderPost True user Nothing) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)

drafts :: (Maybe User) -> [Post] -> Integer -> Html
drafts user posts pageNumber = docTypeHtml $ do
  H.head $ do
    H.title "Drafts"
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader user False
    h1 ! class_ "title" $ "Drafts"
    mapM_ (renderPost True user Nothing) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)
  
postDetail :: (Maybe User) -> Post -> Html
postDetail user pst@(Post _ title _ _ tags _ _) = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml title
    renderMeta "keywords" $ toValue $ T.intercalate ", " $ tags ++ keywords
    renderMeta "description" $ toValue $ postDescription pst
    pageAttributes
  H.body $ do
    renderHeader user True
    renderPost False user Nothing pst
    renderScript "/assets/js/common.js"
    renderScript "/assets/js/comments.js"
      
postsByTag :: (Maybe User) -> Text -> [Post] -> Integer -> Html
postsByTag user tag posts pageNum = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml tag
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader user True
    mapM_ (renderPost True user (Just tag)) (take postsPerPage posts)
    renderPageControls pageNum (length posts > postsPerPage)
      
postEditor :: Maybe Post -> Html
postEditor post = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml $ maybe "New Post" postTitle post
    renderMeta "robots" "noindex, nofollow"
    renderStylesheet "/assets/css/editor.css"
    renderStylesheet "/assets/css/wordlist.css"
    pageAttributes
  H.body $ do
    renderHeader Nothing False
    input
      ! type_ "text"
      ! id "title-field"
      ! placeholder "Post title"
      ! value (textValue ptitle)
    
    div ! id "preview" $ ""
    textarea
      ! id "editor"
      ! customAttribute "post-id" (stringValue $ show pid)
      $ H.text pbody
  
    div ! id "checkbox-container" $ do
      renderCheckbox "public-checkbox" "Public" $ not isDraft
    
    renderButton "Delete" "delete-button" Nothing
    renderButton "Preview" "preview-button" Nothing
    renderButton "Save" "save-button" Nothing
  
    renderScript "/assets/js/marked.min.js"
    renderScript "/assets/js/wordlist-pure.js"
    renderScript "/assets/js/common.js"
    script $ toHtml $ tagJS -- supply tags to editor.js
    renderScript "/assets/js/editor.js"
  where
    tagJS = mconcat ["var tags = [", T.intercalate ", " $ map quote tags, "];"]
    quote s = mconcat ["'", s, "'"]
    ptitle = maybe "" postTitle post
    pbody = maybe "" postBody post
    pid = maybe (-1) postID post
    tags = maybe [] postTags post
    isDraft = maybe True postIsDraft post
    
login :: Maybe Text -> Html
login merrmsg = docTypeHtml $ do
  H.head $ do
    H.title "Login"
    renderMeta "robots" "noindex, nofollow"
    renderStylesheet "/assets/css/login.css"
    pageAttributes
  H.body $ do
    renderHeader Nothing False
    h1 ! class_ "title" $ "Login"
    when (isJust merrmsg) (h3 ! class_ "subtitle" $ toHtml $ fromJust merrmsg)
    H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
      input ! type_ "hidden" ! A.name "source" ! value "form"
      renderTextField False "username" ! placeholder "Username"
      renderTextField True "password" ! placeholder "Password"
    renderButton "Login" "submit" Nothing
    renderScript "/assets/js/login.js"
    
internalError :: Text -> Html
internalError err = docTypeHtml $ do
  H.head $ do
    H.title "Internal Error"
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader Nothing True
    h1 ! class_ "title" $ "Something happened..."
    h3 ! class_ "subtitle" $ toHtml err

-- TODO: add picture of fudge
notFound :: Html
notFound = docTypeHtml $ do
  H.head $ do
    H.title "Not Found"
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader Nothing True
    h1 ! class_ "title" $ "Oh fudge!"
    h3 ! class_ "subtitle" $ "The page you're looking for does not exist."
    
--------------------------------------------------------------------------------
-- |render a 'Post'
renderPost :: Bool -- ^ Whether or not to truncate the post body
           -> Maybe User -- ^ Whether or not the user is authenticated
           -> Maybe Text -- ^ The currently selected tag
           -> Post -- ^ The post to render
           -> Html
renderPost isShort user tag (Post pid title body ts tags _ author) = do
  div ! class_ "post" $ do
    div ! class_ "post-header" $ do
      div ! class_ "post-headerbox" $ do
        a ! href postURL $ do
          h1 ! class_ "post-title" ! A.id (stringValue $ show pid) $ toHtml title
        h4 ! class_ "post-subtitle" $ toHtml subtitle
      div ! class_ "post-headerbox" $ do
        mapM_ taglink tags
      when canEdit renderEditButton
    div ! class_ "post-content" $ if isShort
      then do
        renderDoc . truncateMarkdown 500 . markdown def $ body
        a ! class_ "read-more" ! href postURL $ "read more..."
      else toHtml $ markdown def body
  where
    -- values
    editURL = stringValue $ mconcat ["/posts/", show pid, "/edit"]
    postURL = stringValue $ "/posts/" ++ show pid
    canEdit = isJust user && author == (fromJust user)
    timeFormat = "%-m • %-e • %-y | %l:%M %p %Z"
    subtitle = mconcat [formatDate ts, " | ", userDisplayName author]
    renderEditButton = a ! class_ "post-edit-button"
                         ! href editURL
                         ! rel "nofollow"
                         $ "edit"
    -- functions
    formatDate = T.pack . formatTime defaultTimeLocale timeFormat
    taglink t = a ! class_ (stringValue $ cls tag)
                  ! href (toValue $ T.append "/posts/by/tag/" t)
                  $ toHtml t
      where
        cls Nothing = "taglink"
        cls (Just selectedTag)
          | (TL.toStrict selectedTag) == t = "taglink selected-tag"
          | otherwise = "taglink"

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
  renderStylesheet "/assets/css/blog.css"

-- |Render the page header. Contains things like the "about" information,
-- the admin controls, etc
renderHeader :: (Maybe User) -- ^ The authenticated user
             -> Bool -- ^ whether or not the "about" info is rendered
             -> Html
renderHeader user showAbout = do
  div ! class_ (stringValue $ "header" ++ (if not showAbout then " nopadding" else "")) $ do
    a ! href "/" $ do
      img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200"
    when showAbout $ do
      h1 ! class_ "title" ! A.id "name-title" $ "Nate Symer"
      h3 ! class_ "subtitle" $ "Software Engineer & Designer"
      h3 ! class_ "tagline" $ "nate@symer.io • 856-419-7654"
      
    -- TODO: render user displayName/username
    when (isJust user) $ do
      renderButton "Log Out" "" $ Just "/logout"
      renderButton "New Post" "" $ Just "/posts/new"
      renderButton "Drafts" "" $ Just "/drafts"

renderPageControls :: Integer -> Bool -> Html
renderPageControls pageNum hasNext = do
  when (pageNum > 0)  $ renderButton "Newer" "prevbutton" $ mkHref $ pageNum-1
  when hasNext        $ renderButton "Older" "nextbutton" $ mkHref $ pageNum+1
  where mkHref = Just . toValue . (++) "/?page=" . show