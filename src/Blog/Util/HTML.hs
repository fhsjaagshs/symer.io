{-# LANGUAGE OverloadedStrings #-}

module Blog.Util.HTML
(
  renderTitle,
  renderSubtitle,
  renderAdminControls,
  beginHtml,

  renderHead,
  renderHead',
  renderHiddenHead,
  renderHiddenHead',
  renderStylesheet,
  renderMeta,
  renderScript,
  renderBody,
  renderPostEditor,
  renderPageControls,
  renderInput,
  renderButton,
  renderButton',                        
  renderButton''
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
--- | DRY Rendering

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
  
renderHead' :: Text -> Html
renderHead' pageTitle = renderHead pageTitle (return ())

renderHiddenHead :: Text -> Html -> Html
renderHiddenHead pageTitle htmlAction = renderHead pageTitle $ do
  renderMeta "robots" "noindex, nofollow"
  htmlAction
    
renderHiddenHead' :: Text -> Html
renderHiddenHead' = flip renderHiddenHead (return ())

renderTitle :: Text -> Html
renderTitle = (h2 ! class_ "title" ! id "blog-title") . toHtml

renderSubtitle :: Text -> Html
renderSubtitle = (h3 ! id "blog-subtitle") . toHtml

renderAdminControls :: Html
renderAdminControls = do
  (renderButton' "Log Out" "/logout")
  (renderButton' "New Post" "/posts/new")
  (renderButton' "Drafts" "/drafts")

renderBody :: Html -> Html
renderBody bodyHtml = do
  H.body ! style "text-align: center;" $ do
    a ! href "/" $ do
      img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200"
    div ! id "content" $ bodyHtml

renderPostEditor :: Maybe Post -> Html
renderPostEditor maybePost = do
  input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue $ maybe "" postTitle maybePost)

  div ! id "preview" $ ""
  textarea ! id "editor" ! customAttribute "post-id" (stringValue $ show $ maybe (-1) postID maybePost) $ H.text $ maybe "" postBody maybePost
  textarea ! id "tags" ! class_ "wordlist" $ toHtml . TL.intercalate ", " . map TL.fromStrict . maybe [] postTags $ maybePost
  
  div ! id "checkbox-container" $ do
    renderCheckbox "public-checkbox" (not $ maybe True postIsDraft maybePost)
    H.label ! customAttribute "for" "public-checkbox" $ "Public"
    
  renderButton'' "Delete" "delete-button"
  renderButton'' "Preview" "preview-button"
  renderButton'' "Save" "save-button"
  
  renderScript "/assets/js/jquery-2.1.3.min.js"
  renderScript "/assets/js/marked.min.js"
  renderScript "/assets/js/wordlist.js"
  renderScript "/assets/js/common.js"
  renderScript "/assets/js/editor.js"

renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls mPageNum hasNext = do
  when (pageNum > 0)  $ renderButton "Newer" "prevbutton" (TL.pack $ "/?page=" ++ (show $ pageNum-1))
  when hasNext        $ renderButton "Older" "nextbutton" (TL.pack $ "/?page=" ++ (show $ pageNum+1))
  where pageNum = fromMaybe 0 mPageNum
  
renderInput :: String -> Html
renderInput kind = input
                   ! class_ "blogtextfield"
                   ! customAttribute "autocorrect" "off"
                   ! customAttribute "autocapitalize" "off"
                   ! customAttribute "spellcheck" "false"
                   ! type_ (stringValue kind)
                   
renderCheckbox :: Text -> Bool -> Html
renderCheckbox boxId False = input ! type_ "checkbox" ! id (lazyTextValue boxId)
renderCheckbox boxId True = (renderCheckbox boxId False) ! A.checked ""

renderButton :: Text -> Text -> Text -> Html
renderButton btnTitle btnId btnHref = a
                                      ! class_ "blogbutton"
                                      ! rel "nofollow"
                                      ! id (lazyTextValue btnId)
                                      ! href (lazyTextValue btnHref)
                                      $ toHtml btnTitle

renderButton' :: Text -> Text -> Html
renderButton' btnTitle btnHref = a
                                 ! class_ "blogbutton"
                                 ! rel "nofollow"
                                 ! href (lazyTextValue btnHref)
                                 $ toHtml btnTitle
                                 
renderButton'' :: Text -> Text -> Html
renderButton'' btnTitle btnId = a
                                ! class_ "blogbutton"
                                ! rel "nofollow"
                                ! id (lazyTextValue btnId)
                                $ toHtml btnTitle
