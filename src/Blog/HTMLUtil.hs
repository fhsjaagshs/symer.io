{-# LANGUAGE OverloadedStrings #-}

module Blog.HTMLUtil
(
  blogTitle,
  blogSubtitle,
  postsPerPage,
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
  appendedBlogTitle
)
where

import           Blog.User
import           Blog.Post as Post
import           Blog.Database.Config (postsPerPage)

import           Control.Monad
import           Data.Maybe

import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Prelude as P hiding (head, div, id)

blogTitle :: TL.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: TL.Text
blogSubtitle = "a blog about code."

appendedBlogTitle :: TL.Text -> TL.Text
appendedBlogTitle s = mconcat [s, " | ", blogTitle]

--------------------------------------------------------------------------------
--- | DRY Rendering

renderMeta :: TL.Text -> TL.Text -> Html
renderMeta k v = meta ! name (lazyTextValue k) ! content (lazyTextValue v)

renderStylesheet :: TL.Text -> Html
renderStylesheet x = link ! href (lazyTextValue x) ! rel "stylesheet" ! type_ "text/css"

renderScript :: TL.Text -> Html
renderScript scriptSrc = script ! src (lazyTextValue scriptSrc) $ ""

renderHead :: TL.Text -> Html -> Html
renderHead pageTitle htmlAction = H.head $ do
  H.title $ toHtml pageTitle
  renderMeta "revisit-after" "2 days"
  renderMeta "viewport" "width=device-width, initial-scale=1"
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  renderStylesheet "/assets/css/blog.css"
  htmlAction
  
renderHead' :: TL.Text -> Html
renderHead' pageTitle = renderHead pageTitle (return ())

renderHiddenHead :: TL.Text -> Html -> Html
renderHiddenHead pageTitle htmlAction = renderHead pageTitle $ do
  renderMeta "robots" "noindex, nofollow"
  htmlAction
    
renderHiddenHead' :: TL.Text -> Html
renderHiddenHead' pageTitle = renderHiddenHead pageTitle (return ())

renderBody :: Maybe TL.Text -> Maybe TL.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200"
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) $ do
    (renderButton' "Log Out" "/logout")
    (renderButton' "New Post" "/posts/new")
    (renderButton' "Drafts" "/drafts")
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
                   
renderCheckbox :: TL.Text -> Bool -> Html
renderCheckbox boxId False = input ! type_ "checkbox" ! id (lazyTextValue boxId)
renderCheckbox boxId True = (renderCheckbox boxId False) ! A.checked ""

renderButton :: TL.Text -> TL.Text -> TL.Text -> Html
renderButton btnTitle btnId btnHref = a
                                      ! class_ "blogbutton"
                                      ! rel "nofollow"
                                      ! id (lazyTextValue btnId)
                                      ! href (lazyTextValue btnHref)
                                      $ toHtml btnTitle

renderButton' :: TL.Text -> TL.Text -> Html
renderButton' btnTitle btnHref = a
                                 ! class_ "blogbutton"
                                 ! rel "nofollow"
                                 ! href (lazyTextValue btnHref)
                                 $ toHtml btnTitle
                                 
renderButton'' :: TL.Text -> TL.Text -> Html
renderButton'' btnTitle btnId = a
                                ! class_ "blogbutton"
                                ! rel "nofollow"
                                ! id (lazyTextValue btnId)
                                $ toHtml btnTitle
