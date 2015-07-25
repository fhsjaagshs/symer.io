module Blog.HTMLUtil
(

)
where

import           Auth
import           Config
import           Types
import           Helpers

import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Maybe
import           Data.Default

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.List as List

import           Web.Scotty.Trans as Scotty
import           Web.Scotty.TLS as Scotty
import           Network.HTTP.Types.Status
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.Migration as PG.Migration

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Prelude as P hiding (head, div)

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: T.Text
blogSubtitle = "a blog about code."

postsPerPage :: Int
postsPerPage = 10

seoTags :: [(T.Text, T.Text)]
seoTags = [
            ("revisit-after", "2 days"),
            ("description", "Rants and raves about computer science, functional programming, politics, and everything in between."),
            ("keywords", "computer science, politics, haskell, ruby, web development, art, blogs, money, computers, startups, tutorial, rails, ruby on rails, scotty haskell, snap framework")
            ]
            
-------------------------------------------------------------------------------
--- | DRY Rendering

renderHead :: [T.Text] -> [(T.Text, T.Text)] -> T.Text -> Html
renderHead cssFiles metaTags title_ = H.head $ do
  H.title $ toHtml title_
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  forM_ ("/assets/css/blog.css":cssFiles) (\x -> link ! href (textValue x) ! rel "stylesheet" ! type_ "text/css")
  forM_ (List.nubBy (\(keyOne, _) (keyTwo, _) -> keyOne == keyTwo) $ metaTags ++ seoTags) (\x -> meta ! A.name (textValue $ fst x) ! content (textValue $ snd x))

renderBody :: Maybe T.Text -> Maybe T.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (textValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! A.id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! A.id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) (a ! href "/logout" ! class_ "blogbutton" ! rel "nofollow" $ "Logout")
  when (isJust maybeUser) (a ! href "/posts/new" ! class_ "blogbutton" ! rel "nofollow" $ "New Post")
  when (isJust maybeUser) (a ! href "/drafts" ! class_ "blogbutton" ! rel "nofollow" $ "Drafts")
  div ! A.id "content" $ bodyHtml

renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  input ! type_ "text" ! A.id "title-field" ! placeholder "Post title" ! value (textValue $ maybe "" Types.title maybeBlogPost)

  div ! A.id "preview" $ ""
  textarea ! A.id "editor" ! customAttribute "post-id" (stringValue $ maybe "-1" (show . Types.identifier) maybeBlogPost) $ H.text $ maybe "" Types.body maybeBlogPost
  textarea ! A.id "tags" ! class_ "wordlist" $ toHtml $ List.intercalate ", " $ maybe [] Types.tags maybeBlogPost
  
  div ! A.id "checkbox-container" $ do
    case maybeBlogPost of
      Just (BlogPost _ _ _ _ _ _ False _) -> input ! type_ "checkbox" ! A.id "public-checkbox" ! A.checked ""
      _                                   -> input ! type_ "checkbox" ! A.id "public-checkbox"
    H.label ! customAttribute "for" "public-checkbox" $ "Public"

  a ! A.id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! A.id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! A.id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"

  script ! src "/assets/js/jquery-2.1.3.min.js" $ ""
  script ! src "/assets/js/marked.min.js" $ ""
  script ! src "/assets/js/wordlist.js" $ ""
  script ! src "/assets/js/common.js" $ ""
  script ! src "/assets/js/editor.js" $ ""

renderPageControls :: Maybe Integer -> Bool -> Html
renderPageControls Nothing hasNext = renderPageControls (Just 1) hasNext
renderPageControls (Just pageNum) hasNext = do
  when (pageNum > 2) (a ! A.id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum-1)) $ "Newer")
  when (pageNum == 2) (a ! A.id "prevbutton" ! class_ "blogbutton" ! href (stringValue $ "/") $ "Newer")
  when hasNext (a ! A.id "nextbutton" ! class_ "blogbutton" ! href (stringValue $ "/?page=" ++ (show $ pageNum+1)) $ "Older")

-------------------------------------------------------------------------------
--- | Specialized Helpers

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle s = T.append s (T.append " | " blogTitle)

postTags :: [String] -> [(T.Text, T.Text)]
postTags ts = [("keywords", T.append (T.pack $ (List.intercalate ", " ts) ++ ", ") (fromJust $ lookup "keywords" seoTags))]
