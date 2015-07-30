{-# LANGUAGE OverloadedStrings #-}

module Blog.HTMLUtil
(
  blogTitle,
  blogSubtitle,
  postsPerPage,
  seoTags,
  renderHead,
  renderBody,
  renderPostEditor,
  renderPageControls,
  appendedBlogTitle,
  postTags
)
where

import           Blog.Types as Types
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

seoTags :: [(TL.Text, TL.Text)]
seoTags = [
            ("revisit-after", "2 days"),
            ("description", "Rants and raves about functional programming, politics, and everything in between."),
            ("keywords", "computer science, functional programming, fp, politics, haskell, ruby, web development, art, blogs, money, computers, startups, tutorial, rails, ruby on rails")
            ]
            
-------------------------------------------------------------------------------
--- | DRY Rendering

renderHead :: [TL.Text] -> [(TL.Text, TL.Text)] -> TL.Text -> Html
renderHead cssFiles metaTags title_ = H.head $ do
  H.title $ toHtml title_
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  mapM_ linkify ("/assets/css/blog.css":cssFiles)
  mapM_ metafy (L.nubBy eqltest $ metaTags ++ seoTags)
  where
    eqltest (a, _) (b, _) = a == b
    linkify x = link ! href (lazyTextValue x) ! rel "stylesheet" ! type_ "text/css"
    metafy x = meta ! A.name (lazyTextValue $ fst x) ! content (lazyTextValue $ snd x)

renderBody :: Maybe TL.Text -> Maybe TL.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (lazyTextValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) (a ! href "/logout" ! class_ "blogbutton" ! rel "nofollow" $ "Logout")
  when (isJust maybeUser) (a ! href "/posts/new" ! class_ "blogbutton" ! rel "nofollow" $ "New Post")
  when (isJust maybeUser) (a ! href "/drafts" ! class_ "blogbutton" ! rel "nofollow" $ "Drafts")
  div ! id "content" $ bodyHtml

renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue $ maybe "" Types.title maybeBlogPost)

  div ! id "preview" $ ""
  textarea ! id "editor" ! customAttribute "post-id" (stringValue $ maybe "-1" (show . Types.identifier) maybeBlogPost) $ H.text $ maybe "" Types.body maybeBlogPost
  textarea ! id "tags" ! class_ "wordlist" $ toHtml $ L.intercalate ", " $ maybe [] Types.tags maybeBlogPost
  
  div ! id "checkbox-container" $ do
    case maybeBlogPost of
      Just (BlogPost _ _ _ _ _ _ False _) -> input ! type_ "checkbox" ! id "public-checkbox" ! A.checked ""
      _                                   -> input ! type_ "checkbox" ! id "public-checkbox"
    H.label ! customAttribute "for" "public-checkbox" $ "Public"
    
  a ! id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"
  
  script ! src "/assets/js/jquery-2.1.3.min.js" $ ""
  script ! src "/assets/js/marked.min.js" $ ""
  script ! src "/assets/js/wordlist.js" $ ""
  script ! src "/assets/js/common.js" $ ""
  script ! src "/assets/js/editor.js" $ ""

renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls Nothing hasNext = renderPageControls (Just 1) hasNext
renderPageControls (Just pageNum) hasNext = do
  when (pageNum > 2) (a ! id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum-1)) $ "Newer")
  when (pageNum == 2) (a ! id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/") $ "Newer")
  when hasNext (a ! id "nextbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum+1)) $ "Older")

-------------------------------------------------------------------------------
--- | Specialized Helpers

appendedBlogTitle :: TL.Text -> TL.Text
appendedBlogTitle s = TL.append s (TL.append " | " blogTitle)
    
postTags :: [String] -> [(TL.Text, TL.Text)]
postTags ts = [("keywords", TL.append (TL.pack $ (L.intercalate ", " ts) ++ ", ") (fromJust $ lookup "keywords" seoTags))]
