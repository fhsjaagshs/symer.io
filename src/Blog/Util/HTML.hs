{-# LANGUAGE OverloadedStrings #-}

module Blog.Util.HTML
(
  beginHtml,
  renderMeta,
  renderStylesheet,
  renderScript,
  renderHead,
  renderBody,
  renderButton,
  renderCheckbox,
  renderInput,
  renderTitle,
  renderSubtitle,
  renderAdminControls,
  renderPageControls,
  renderPostEditor
)
where

import           Blog.Post

import           Control.Monad
import           Data.Maybe

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html.Renderer.Text as R
import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Prelude as P hiding (head, div, id)

import qualified Web.Scotty.Trans as Scotty (html)
import           Web.Scotty.Trans (ActionT, ScottyError)

--------------------------------------------------------------------------------
--- | Scotty Helper

beginHtml :: (ScottyError e, Monad m) => Html -> ActionT e m ()
beginHtml = Scotty.html . R.renderHtml . docTypeHtml

--------------------------------------------------------------------------------
--- | HTML primitives

renderMeta :: Text -> Text -> Html
renderMeta k v = meta ! name (lazyTextValue k) ! content (lazyTextValue v)

renderStylesheet :: Text -> Html
renderStylesheet x = link ! href (lazyTextValue x) ! rel "stylesheet" ! type_ "text/css"

renderScript :: Text -> Html
renderScript scriptSrc = script ! src (lazyTextValue scriptSrc) $ ""

renderHead :: Text -> Html -> Html
renderHead pageTitle htmlAction = H.head $ do
  H.title $ toHtml pageTitle
  renderMeta "revisit-after" "2 days"
  renderMeta "viewport" "width=device-width, initial-scale=1"
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  renderStylesheet "/assets/css/blog.css"
  htmlAction

renderBody :: Html -> Html
renderBody bodyHtml = do
  H.body ! style "text-align: center;" $ do
    a ! href "/" $ do
      img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200"
    div ! id "content" $ bodyHtml

--------------------------------------------------------------------------------
--- | Controls

renderInput :: String -> Html
renderInput kind = input
                   ! class_ "blogtextfield"
                   ! customAttribute "autocorrect" "off"
                   ! customAttribute "autocapitalize" "off"
                   ! customAttribute "spellcheck" "false"
                   ! type_ (stringValue kind)
                   
renderCheckbox :: Text -> Text -> Bool -> Html
renderCheckbox boxId txt isChecked = do
  H.label ! customAttribute "for" "public-checkbox" $ do
    if isChecked
      then checkbox ! A.checked ""
      else checkbox
    toHtml txt
  where
    checkbox = input ! type_ "checkbox" ! id (lazyTextValue boxId)

renderButton :: Text -> Text -> Maybe Text -> Html
renderButton btnTitle btnId btnHref = a
                                      ! class_ "blogbutton"
                                      ! rel "nofollow"
                                      ! id (lazyTextValue btnId)
                                      ! f btnHref
                                      $ toHtml btnTitle
  where
    f (Just anHref) = href (lazyTextValue anHref)
    f Nothing = mempty
                                      
--------------------------------------------------------------------------------
--- | Composite HTML DRY

renderTitle :: Text -> Html
renderTitle = (h2 ! class_ "title" ! id "blog-title") . toHtml

renderSubtitle :: Text -> Html
renderSubtitle = (h3 ! id "blog-subtitle") . toHtml

renderAdminControls :: Html
renderAdminControls = do
  renderButton "Log Out" "" $ Just "/logout"
  renderButton "New Post" "" $ Just "/posts/new"
  renderButton "Drafts" "" $ Just "/drafts"
  
renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls mPageNum hasNext = do
  when (pageNum > 0)  $ renderButton "Newer" "prevbutton" $ mkHref $ pageNum-1
  when hasNext        $ renderButton "Older" "nextbutton" $ mkHref $ pageNum+1
  where pageNum = fromMaybe 0 mPageNum
        mkHref = Just . TL.pack . (++) "/?page=" . show

renderPostEditor :: Maybe Post -> Html
renderPostEditor post = do
  input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue ptitle)

  div ! id "preview" $ ""
  textarea ! id "editor" ! customAttribute "post-id" (stringValue $ show pid) $ H.text pbody
  textarea ! id "tags" ! class_ "wordlist" $ toHtml . TL.intercalate ", " . map TL.fromStrict $ tags
  
  div ! id "checkbox-container" $ do
    renderCheckbox "public-checkbox" "Public" $ not isDraft

  renderButton "Delete" "delete-button" Nothing
  renderButton "Preview" "preview-button" Nothing
  renderButton "Save" "save-button" Nothing
  
  renderScript "/assets/js/jquery-2.1.3.min.js"
  renderScript "/assets/js/marked.min.js"
  renderScript "/assets/js/wordlist.js"
  renderScript "/assets/js/common.js"
  renderScript "/assets/js/editor.js"
  where
    ptitle = maybe "" postTitle post
    pbody = maybe "" postBody post
    pid = maybe (-1) postID post
    tags = maybe [] postTags post
    isDraft = maybe True postIsDraft post