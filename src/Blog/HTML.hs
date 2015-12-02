{-# LANGUAGE OverloadedStrings #-}

module Blog.HTML
(
  root,
  drafts,
  postDetail,
  postsByTag,
  postEditor,
  login,
  internalError,
  notFound,
  Html
)
where
  
import Blog.User
import Blog.Post
import Blog.HTML.Common
import Blog.Util.Markdown
import Blog.Postgres (postsPerPage)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Maybe
import Control.Monad

import Cheapskate
import Cheapskate.Html

import Data.Time.Format

import Prelude as P hiding (head,div,id)
import Text.Blaze.Html5 as H hiding (style,param,map,title,body,head)
import Text.Blaze.Html5.Attributes as A hiding (title)

root :: (Maybe User) -> [Post] -> Integer -> Html
root user posts pageNumber = docTypeHtml $ do
  renderHead "Nate Symer" $ do
    renderMeta "description" description
    renderKeywords keywords
  renderBody True $ do
    when (isJust user) renderAdminControls
    mapM_ (renderPost True user) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)
      
drafts :: (Maybe User) -> [Post] -> Integer -> Html
drafts muser posts pageNumber = docTypeHtml $ do
  renderHead "Drafts" $ do
    renderMeta "robots" "noindex, nofollow"
  renderBody False $ do
    renderTitle "Drafts"
    when (isJust muser) renderAdminControls
    mapM_ (renderPost True muser) (take postsPerPage posts)
    renderPageControls pageNumber (length posts > postsPerPage)
  
postDetail :: (Maybe User) -> Post -> Html
postDetail user pst@(Post _ title _ _ tags _ _) = docTypeHtml $ do
  renderHead (TL.fromStrict title) $ do
    renderKeywords $ (map TL.fromStrict tags) ++ keywords
    renderMeta "description" $ TL.fromStrict $ postDescription pst
  renderBody True $ do
    when (isJust user) renderAdminControls
    renderPost False user pst
    renderScript "/assets/js/common.js"
    renderScript "/assets/js/comments.js"
      
postsByTag :: (Maybe User) -> TL.Text -> [Post] -> Integer -> Html
postsByTag user tag posts pageNum = docTypeHtml $ do
  renderHead tag $ do
    renderMeta "description" description
    renderKeywords keywords
  renderBody True $ do
    renderTitle $ mconcat ["Posts tagged '", tag, "'"]
    when (isJust user) renderAdminControls
    mapM_ (renderPost True user) (take postsPerPage posts)
    renderPageControls pageNum (length posts > postsPerPage)
      
postEditor :: Maybe Post -> Html
postEditor post = docTypeHtml $ do
  renderHead (maybe "New Post" (TL.fromStrict . postTitle) post) $ do
    renderMeta "robots" "noindex, nofollow"
    renderStylesheet "/assets/css/editor.css"
    renderStylesheet "/assets/css/wordlist.css"
  renderBody False $ do
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
    
login :: Maybe TL.Text -> Html
login merrmsg = docTypeHtml $ do
  renderHead "Login" $ do
    renderMeta "robots" "noindex, nofollow"
    renderStylesheet "/assets/css/login.css"
  renderBody False $ do
    renderTitle "Login"
    when (isJust merrmsg) (renderSubtitle $ fromJust merrmsg)
    H.form ! A.id "loginform" ! action "/login" ! method "POST" $ do
      input ! type_ "hidden" ! A.name "source" ! value "form"
      renderInput "text" ! A.id "username" ! placeholder "Username" ! A.name "username"
      renderInput "password" ! A.id "password" ! placeholder "Password" ! A.name "password"
    renderButton "Login" "submit" Nothing
    renderScript "/assets/js/login.js"
    
internalError :: TL.Text -> Html
internalError err = docTypeHtml $ do
  renderHead "Internal Error" $ do
    renderMeta "robots" "noindex, nofollow"
  renderBody True $ do
    renderTitle "Something happened..."
    renderSubtitle err

-- TODO: add picture of fudge
notFound :: Html
notFound = docTypeHtml $ do
  renderHead "Not Found" $ do
    renderMeta "robots" "noindex, nofollow"
  renderBody True $ do
    renderTitle "Oh fudge!"
    renderSubtitle "The page you're looking for does not exist."
    
--------------------------------------------------------------------------------
renderPost :: Bool -> Maybe User -> Post -> Html
renderPost isShort user (Post pid title body ts tags _ author) = do
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
    taglink t = a ! class_ "taglink"
                  ! href (textValue $ T.append "/posts/by/tag/" t)
                  $ toHtml t

renderKeywords :: [TL.Text] -> Html
{-# INLINE renderKeywords #-}
renderKeywords = renderMeta "keywords" . TL.intercalate ", "

keywords :: [TL.Text]
keywords = ["nate",
            "nathaniel",
            "symer",
            "nate symer",
            "computer",
            "science",
            "software",
            "functional",
            "programming",
            "web",
            "haskell",
            "ruby",
            "art",
            "studio",
            "lean",
            "startup"]

description :: TL.Text
description = "Nate Symer website & blog. Nate is a software engineer & designer."