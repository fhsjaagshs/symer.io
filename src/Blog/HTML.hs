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
    renderHeader user True Nothing
    mapM_ (renderPost True user Nothing) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)

drafts :: (Maybe User) -> [Post] -> Integer -> Html
drafts user posts pageNumber = docTypeHtml $ do
  H.head $ do
    H.title "Drafts"
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader user False (Just "Drafts")
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
    renderHeader user True Nothing
    renderPost False user Nothing pst
    renderScript "/assets/js/comments.js"
      
postsByTag :: (Maybe User) -> Text -> [Post] -> Integer -> Html
postsByTag user tag posts pageNum = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml tag
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader user True Nothing
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
    renderHeader Nothing False Nothing

    H.form ! id "post-form" ! action "/posts" ! method "POST" $ do
      when (isJust post) $ input  ! type_ "hidden" ! name "id"    ! value (toValue $ postID $ fromJust post)
      input    ! id "input-tags"  ! type_ "hidden" ! name "tags"  ! value (toValue $ T.intercalate "," tags)
      input    ! id "title-field" ! type_ "text"   ! name "title" ! value (textValue ptitle) ! placeholder "Post title" 
      textarea ! id "editor" ! A.form "post-form"  ! name "body"  ! customAttribute "post-id" (stringValue $ show pid) $ H.text pbody
  
      H.label ! A.id "checkbox" ! customAttribute "for" "public-checkbox" $ do
        if not isDraft then checkbox ! A.checked "" else checkbox
        toHtml $ ("Public" :: String)
        
      input ! id "save-buton" ! class_ "button" ! type_ "submit" ! value "Save"

    when (isJust post) $ do
      H.form ! id "delete-form" ! action "/posts" ! method "POST" $ do
        input ! type_ "hidden" ! name "id"     ! value (toValue $ postID $ fromJust post)
        input ! type_ "hidden" ! name "method" ! value "DELETE"
        input ! id "delete-buton" ! class_ "button" ! type_ "submit" ! value "Delete"
      
    renderScript "/assets/js/wordlist-pure.js"
    script $ mconcat [
      "var editor = document.getElementById('editor');",
      "var taginput = document.getElementById('input-tags');",
      "editor.parentNode.insertBefore((new WordList(taginput)).div, editor.nextSibling);"
      ]
  where
    -- html
    checkbox = input ! type_ "checkbox" ! id "public-checkbox" ! A.form "post-form" ! name "draft" ! value "f"
    -- values
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
    pageAttributes
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
    H.title "Internal Error"
    renderMeta "robots" "noindex, nofollow"
    pageAttributes
  H.body $ do
    renderHeader Nothing True Nothing
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
renderHeader :: (Maybe User) -- ^ the authenticated user
             -> Bool -- ^ whether or not the "about" info is rendered
             -> Maybe Html -- ^ an optional title
             -> Html
renderHeader user showAbout ttl = do
  div ! class_ (stringValue $ "header" ++ (if not showAbout then " nopadding" else "")) $ do
    a ! href "/" $ do
      img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200"
      
    when showAbout $ do
      h1 ! class_ "title" ! A.id "name-title" $ "Nate Symer"
      h3 ! class_ "subtitle" $ "Software Engineer & Designer"
      h3 ! class_ "tagline" $ "nate@symer.io • 856-419-7654"
      
    renderTitle ttl
    renderControls user
  where
    renderTitle v = maybe (return ()) (h1 ! class_ "title") v
    renderControls Nothing = return ()
    renderControls (Just (User _ uname _ _)) = do
      renderButton ! href "/logout"    $ "Log Out"
      renderButton ! href "/posts/new" $ "New Post"
      renderButton ! href "/drafts"    $ "Drafts"
      h3 ! class_ "tagline" $ toHtml $ mconcat ["Logged in as ",uname]
    

renderPageControls :: Integer -> Bool -> Html
renderPageControls pageNum hasNext = do
  when (pageNum > 0)  $ renderButton ! A.id "prevbutton" ! href (mkHref $ pageNum-1) $ "Newer"
  when hasNext        $ renderButton ! A.id "nextbutton" ! href (mkHref $ pageNum+1) $ "Older"
  where mkHref = toValue . (++) "/?page=" . show