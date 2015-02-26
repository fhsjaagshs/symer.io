{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Maybe
import           Data.DateTime -- fromSqlString :: String -> Maybe DateTime
import           Data.Text.Encoding as TE
import           Prelude hiding (head, id, div) -- hide the functions that may conflict wit Blaze
import qualified Prelude as P (head, id, div)

import           Web.Scotty as Scotty
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as Postgres
import           Database.PostgreSQL.Simple.FromRow as Postgres.FromRow
import           Database.PostgreSQL.Simple.Time as Postgres.Time
import           Network.Wai.Middleware.Static

import           Crypto.BCrypt
import           Cheapskate
import           Text.Blaze.Html5 as H hiding (style)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Text.Blaze.Internal

-- TODO:
-- 1. Finish editor page
--   a. load blog post into frontend
--   b. edit title
--   c. rework design (procrastinateable)
-- 2. Pagination
-- 3. Edit post control in posts page
-- 3. Tags - figure out how to 
--   a. Adding tags in editor
--   b. filter posts by tag
-- 4. Authentication (multiple users?? How will this affect BlogPosts in the DB?) THIS CAN COME LAST

data BlogPost = BlogPost {
  identifier :: Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: ZonedTimestamp
}

instance Show BlogPost where
  show (BlogPost i t bt ts) = 
    "BlogPost { identifier: " ++ (show i) ++ ", title: " ++ (T.unpack t) ++ ", body: " ++ (T.unpack bt) ++ " }"

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field


data PostTag = PostTag {
  postIdentifier :: Integer,
  tag :: T.Text
}

instance Show PostTag where
  show (PostTag i t) = "PostTag { postIdentifier" ++ (show i) ++ ", tag: " ++ (T.unpack t) ++ " }"
  
instance FromRow PostTag where
  fromRow = PostTag <$> field <*> field


main = scotty 3000 $ do
  pg <- liftIO $ Postgres.connectPostgreSQL "dbname='nathaniel' user='nathaniel' password='' port='5432'"
 -- redis <- liftIO $ Redis.connect Redis.defaultConnectInfo
  middleware $ staticPolicy (noDots >-> addBase "public")
 -- get "/assets" $ staticPolicy (addBase "public")
 
 -- get "/assets/epiceditor/:file" $ do
    
  
  get "/" $ do
    ps <- params
    let mfrom = ((read . T.unpack . TL.toStrict <$> (lookup "from" ps)) :: Maybe Integer)
    let muntil = ((read . T.unpack . TL.toStrict <$> (lookup "until" ps)) :: Maybe Integer)
    res <- liftIO $ getBlogPosts pg mfrom muntil
    
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop
        div ! style "width: 700px; margin: auto;" $ do
          renderPosts res
          
  post "/posts" $ do
    ps <- params
    liftIO $ print ps
    case (lookup "id" ps) of
      Nothing -> do
        res <- liftIO $ query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" ((TL.toStrict $ fromJust $ lookup "title" ps) :: T.Text, (TL.toStrict $ fromJust $ lookup "body" ps) :: T.Text)
        if (length res) == 0
          then redirect "/"
          else do
            redirect $ TL.pack $ "/posts/" ++ (show $ Main.identifier $ P.head res)
      Just (identifier) -> do
        res <- liftIO $ query pg "UPDATE blogposts SET title=? bodyText=? WHERE identifier=? RETURNING *" ((TL.toStrict $ fromJust $ lookup "title" ps) :: T.Text, (TL.toStrict $ fromJust $ lookup "body" ps) :: T.Text, (read $ TL.unpack identifier) :: Integer)
        if (length res) == 0
          then redirect "/"
          else do
            redirect $ TL.pack $ "/posts/" ++ (show $ Main.identifier $ P.head res)
  
  get "/posts/during/:year" $ do
    return ()
    
  get "/posts/during/:year/:month" $ do
    return ()
    
  get "/posts/during/:year/:month/:day" $ do
    return ()
    
  get "/posts/new" $ do
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop
        div ! style "width: 700px; margin: auto;" $ do
          renderPostEditor Nothing
            
  get "/posts/:id" $ do
    ps <- params
    case (lookup "id" ps) of
      Nothing -> Scotty.html $ R.renderHtml $ renderNotFound
      Just (identifier) -> do
        res <- liftIO $ query pg "SELECT * FROM blogposts WHERE identifier = ? LIMIT 1" [identifier]
      
        if (length res) == 0
          then Scotty.html $ R.renderHtml $ renderNotFound
          else do
           Scotty.html $ R.renderHtml $ docTypeHtml $ do
              renderHead blogTitle
              H.body ! style "text-align: center;" $ do
                renderTop
                div ! style "width: 700px; margin: auto;" $ do
                  renderPosts res
                  
  get "/posts/:id/edit" $ do
    ps <- params
    case (lookup "id" ps) of
      Nothing -> Scotty.html $ R.renderHtml $ renderNotFound
      Just (identifier) -> do
        res <- liftIO $ query pg "SELECT * FROM blogposts WHERE identifier = ? LIMIT 1" [identifier]
        if (length res) == 0
          then Scotty.html $ R.renderHtml $ renderNotFound
          else do
            Scotty.html $ R.renderHtml $ docTypeHtml $ do
              renderHead blogTitle
              H.body ! style "text-align: center;" $ do
                renderTop
                div ! style "width: 700px; margin: auto;" $ do
                  renderPostEditor (Just (P.head res))

-------------------------------------------------------------------------------
--- | Helpers

blogTitle :: String
blogTitle = "Segmentation Fault (core dumped)"

renderNotFound :: Html
renderNotFound = do
  return ()

renderHead :: String -> Html
renderHead title = H.head $ do
  H.title $ toHtml title
  link ! href "https://symer.io/assets/css/site.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/assets/vue.css" ! rel "stylesheet" ! type_ "text/css"
  
renderTop :: Html
renderTop = do
  img ! src "https://symer.io/assets/images/philly_skyline.svg" ! width "300" ! height "200"
  h2 ! class_ "title" ! style "font-size: 50px; margin-top: 25px" $ toHtml blogTitle
  
renderPost :: BlogPost -> Html
renderPost b = do
  div ! class_ "post" $ do
    h1 $ toHtml $ Main.title b -- BlogPost
    div ! style "text-align: left;" $ toHtml $ markdown def (Main.body b) -- BlogPost

renderPosts :: [BlogPost] -> Html
renderPosts [] = return ()
renderPosts [x] = renderPost x
renderPosts (x:xs) = do
  renderPost x
  hr ! style "text-align: right; border-top: dashed 3px; width: 700px"
  renderPosts xs
  
renderVue :: Maybe BlogPost -> Html
renderVue Nothing = do
  H.div ! style "width: 100%; height: 300px; text-align: left" $ do
    H.div ! A.id "editor" $ do
      textarea ! id "markdown-textarea" ! customAttribute "v-model" "input" $ ""
      H.div ! customAttribute "v-html" "input | marked" $ ""
  
renderVue (Just blogPost) = do
  H.div ! style "width: 100%; height: 300px; text-align: left" $ do
    H.div ! A.id "editor" ! customAttribute "post-id" (stringValue $ show $ Main.identifier blogPost) $ do
      textarea ! id "markdown-textarea" ! customAttribute "v-model" "input" $ ""
      H.div ! customAttribute "v-html" "input | marked" $ ""
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  script ! src "/assets/vue.min.js" $  ""
  script ! src "http://cdnjs.cloudflare.com/ajax/libs/marked/0.3.2/marked.min.js" $ ""
  renderVue maybeBlogPost
  button ! id "save-button" $ "Save"
  script ! src "/assets/editor.js" $ ""
  
getBlogPosts :: Postgres.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing Nothing = query_ pg "SELECT * FROM blogposts"
getBlogPosts pg (Just from) Nothing = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10" [from]
getBlogPosts pg Nothing (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10" (from, untill)
