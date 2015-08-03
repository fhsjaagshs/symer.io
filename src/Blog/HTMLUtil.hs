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
  renderBody,
  renderPostEditor,
  renderPageControls,
  appendedBlogTitle
)
where

import           Blog.User
import           Blog.Post as Post
import           Blog.Database.Config (postsPerPage)

import           Control.Monad
import           Data.Maybe

import qualified Data.List as L
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Prelude as P hiding (head, div, id)

blogTitle :: TL.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: TL.Text
blogSubtitle = "a blog about code."

-------------------------------------------------------------------------------
--- | DRY Rendering

blogButton :: TL.Text -> TL.Text -> Html
blogButton btnTitle btnHref = a ! class_ "blogbutton" ! rel "nofollow" ! href (lazyTextValue btnHref) $ toHtml btnTitle

renderMeta :: TL.Text -> TL.Text -> Html
renderMeta k v = meta ! A.name (lazyTextValue k) ! content (lazyTextValue v)

renderStylesheet :: TL.Text -> Html
renderStylesheet x = link ! href (lazyTextValue x) ! rel "stylesheet" ! type_ "text/css"

renderHead :: TL.Text -> Html -> Html
renderHead pageTitle htmlAction = H.head $ do
  H.title $ toHtml pageTitle
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  renderStylesheet "/assets/css/blog.css"
  renderMeta "revisit-after" "2 days"
  htmlAction
  
renderHead' :: TL.Text -> Html
renderHead' pageTitle = renderHead pageTitle (return ())

renderHiddenHead :: TL.Text -> Html -> Html
renderHiddenHead pageTitle htmlAction = renderHead pageTitle $ do
  renderMeta "robots" "noindex, nofollow"
  htmlAction
    
renderHiddenHead' :: TL.Text -> Html
renderHiddenHead' pageTitle = renderHiddenHead pageTitle (return ())

-- renderHead :: [TL.Text] -> [(TL.Text, TL.Text)] -> TL.Text -> Html
-- renderHead cssFiles metaTags title_ = H.head $ do
--   H.title $ toHtml title_
--   meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
--   meta ! name "viewport" ! content "width=device-width, initial-scale=1"
--   mapM_ linkify ("/assets/css/blog.css":cssFiles)
--   mapM_ metafy (L.nubBy eqltest $ metaTags ++ seoTags)
--   where
--     eqltest (a1, _) (b1, _) = a1 == b1
--     linkify x = link ! href (lazyTextValue x) ! rel "stylesheet" ! type_ "text/css"
--     metafy x = meta ! A.name (lazyTextValue $ fst x) ! content (lazyTextValue $ snd x)

renderBody :: Maybe TL.Text -> Maybe TL.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (lazyTextValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) $ do
    (blogButton "Log Out" "/logout")
    (blogButton "New Post" "/posts/new")
    (blogButton "Drafts" "/drafts")
  div ! id "content" $ bodyHtml

renderPostEditor :: Maybe Post -> Html
renderPostEditor maybePost = do
  input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue $ maybe "" postTitle maybePost)

  div ! id "preview" $ ""
  textarea ! id "editor" ! customAttribute "post-id" (stringValue $ show $ maybe (-1) postID maybePost) $ H.text $ maybe "" postBody maybePost
  textarea ! id "tags" ! class_ "wordlist" $ toHtml $ L.intercalate ", " $ maybe [] postTags maybePost
  
  div ! id "checkbox-container" $ do
    case maybePost of
      Just (Post _ _ _ _ _ False _) -> input ! type_ "checkbox" ! id "public-checkbox" ! A.checked ""
      _                             -> input ! type_ "checkbox" ! id "public-checkbox"
    H.label ! customAttribute "for" "public-checkbox" $ "Public"
    
  a ! class_ "blogbutton" ! rel "nofollow" ! id "delete-button" $ "Delete"
  a ! class_ "blogbutton" ! rel "nofollow" ! id "preview-button" $ "Preview"
  a ! class_ "blogbutton" ! rel "nofollow" ! id "save-button" $ "Save"
  
  script ! src "/assets/js/jquery-2.1.3.min.js" $ ""
  script ! src "/assets/js/marked.min.js" $ ""
  script ! src "/assets/js/wordlist.js" $ ""
  script ! src "/assets/js/common.js" $ ""
  script ! src "/assets/js/editor.js" $ ""

renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls mPageNum hasNext = do
  when (pageNum > 0)  (a ! class_ "blogbutton" ! id "prevbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum-1)) $ "Newer")
  when hasNext        (a ! class_ "blogbutton" ! id "nextbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum+1)) $ "Older")
  where pageNum = fromMaybe 0 mPageNum

-------------------------------------------------------------------------------
--- | Specialized Helpers

appendedBlogTitle :: TL.Text -> TL.Text
appendedBlogTitle s = mconcat [s, " | ", blogTitle]
    
-- mkPostTags :: [String] -> [(TL.Text, TL.Text)]
-- mkPostTags ts = [("keywords", TL.append (TL.pack $ (L.intercalate ", " ts) ++ ", ") (fromJust $ lookup "keywords" seoTags))]
