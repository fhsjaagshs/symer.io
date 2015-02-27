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
import           Network.Wai
import           Network.Wai.Middleware.Static

import           Crypto.BCrypt
import           Cheapskate
import           Text.Blaze.Html5 as H hiding (style)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Text.Blaze.Internal

import qualified Network.HostName as HostName

-- TODO
-- 1. Finish editor page
--   a. delete post
--   b. rework design (procrastinateable)
-- 1.5. New post button
-- 2. Pagination
-- 3. Edit post control in posts page
-- 3. Tags - figure out how to 
--   a. Adding tags in editor
--   b. filter posts by tag
-- 4. Authentication (multiple users?? How will this affect BlogPosts in the DB?) THIS CAN COME LAST
-- 5. 'Top 5' tags map in side bar?

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
  middleware $ staticPolicy (noDots >-> (hasPrefix "assets") >-> addBase "public")
  
  get "/" $ do
    ps <- params
    let mfrom = ((read . T.unpack . TL.toStrict <$> (lookup "from" ps)) :: Maybe Integer)
    let muntil = ((read . T.unpack . TL.toStrict <$> (lookup "until" ps)) :: Maybe Integer)
    res <- liftIO $ getBlogPosts pg mfrom muntil
    
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop $ Just blogTitle
        div ! style "width: 700px; margin: auto;" $ do
          renderPosts res
          
  delete "/posts/:id" $ do
    ps <- params
    case (lookup "id" ps) of
      (Just identifier) -> do
        _ <- liftIO $ execute pg "DELETE FROM blogposts WHERE id=?" [identifier]
        return ()
      Nothing -> do
        return ()
          
  post "/posts" $ do
    ps <- params
    case (lookup "id" ps) of
      Nothing -> do
        res <- liftIO $ query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" ((TL.toStrict $ fromJust $ lookup "title" ps) :: T.Text, (TL.toStrict $ fromJust $ lookup "body" ps) :: T.Text)
        if (length res) == 0
          then redirect "/"
          else do
            redirect $ TL.pack $ "/posts/" ++ (show $ Main.identifier $ P.head res)
      Just (identifier) -> do
        res <- liftIO $ query pg "UPDATE blogposts SET title=?,bodyText=? WHERE identifier=? RETURNING *" ((TL.toStrict $ fromJust $ lookup "title" ps) :: T.Text, (TL.toStrict $ fromJust $ lookup "body" ps) :: T.Text, (read $ TL.unpack identifier) :: Integer)
        baseurl <- baseURL
        unless (Prelude.null res) $ addHeader "Location" (TL.pack ((T.unpack baseurl) ++ "/posts/" ++ (show $ Main.identifier $ P.head res)))
  
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
        renderTop $ Just blogTitle
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
              renderHead $ appendedBlogTitle (Main.title $ P.head res)
              H.body ! style "text-align: center;" $ do
                renderTop $ Just blogTitle
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
              renderHead $ appendedBlogTitle (Main.title $ P.head res)
              H.body ! style "text-align: center;" $ do
                renderTop Nothing
                div ! style "width: 700px; margin: auto;" $ do
                  renderPostEditor (Just (P.head res))
                  
  notFound $ do
    return ()

-------------------------------------------------------------------------------
--- | Helpers

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle text = T.append text (T.append " | " blogTitle)

-- This is a horrible function.
-- It's waaayyy to imperative in nature
baseURL :: ActionM T.Text
baseURL = do
  h <- Scotty.header "Host"
  req <- request
  return $ T.pack $ (if (isSecure req) then "https" else "http") ++ "://" ++ (TL.unpack $ fromJust h)
  
-------------------------------------------------------------------------------
--- | HTML rendering

renderNotFound :: Html
renderNotFound = do
  return ()

renderHead :: T.Text -> Html
renderHead title = H.head $ do
  H.title $ toHtml title
  link ! href "https://symer.io/assets/css/site.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/assets/blog.css" ! rel "stylesheet" ! type_ "text/css"

renderTop :: Maybe T.Text -> Html
renderTop Nothing = do
  a ! href "/" $ do
    img ! src "https://symer.io/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (stringValue $ T.unpack blogTitle)
renderTop (Just title) = do
  renderTop Nothing
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
  hr ! class_ "separator"
  renderPosts xs
  
renderMdEditor :: Maybe BlogPost -> Html
renderMdEditor Nothing = do
  H.div ! A.id "editor-wrapper" $ do
    H.div ! A.id "editor" $ do
      textarea ! id "markdown-textarea" $ ""
  
renderMdEditor (Just blogPost) = do
  H.div ! A.id "editor-wrapper" $ do
    H.div ! A.id "editor" ! customAttribute "post-id" (stringValue $ show $ Main.identifier blogPost) $ do
      textarea ! id "markdown-textarea" $ toHtml $ T.unpack $ Main.body blogPost
      
renderTitleField :: Maybe BlogPost -> Html
renderTitleField (Just blogPost) = input ! type_ "text" ! id "title-field" ! value (stringValue $ T.unpack $ Main.title blogPost)
renderTitleField Nothing = input ! type_ "text" ! id "title-field"
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  script ! src "/assets/epiceditor/epiceditor.min.js" $ ""
  renderTitleField maybeBlogPost
  renderMdEditor maybeBlogPost
  button ! id "save-button" $ "Save"
  script ! src "/assets/editor.js" $ ""
  
-------------------------------------------------------------------------------
--- | Database
  
getBlogPosts :: Postgres.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing Nothing = query_ pg "SELECT * FROM blogposts ORDER BY identifier"
getBlogPosts pg (Just from) Nothing = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10 ORDER BY identifier" [from]
getBlogPosts pg Nothing (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10 ORDER BY identifier" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier" (from, untill)
