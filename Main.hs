{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.List as List
import           Data.Maybe
import           Data.DateTime -- fromSqlString :: String -> Maybe DateTime
import           Data.Text.Encoding as TE
import           Data.Text.Lazy.Encoding as TLE
import qualified Data.HashMap as Map
import           Prelude hiding (head, id, div) -- hide the functions that may conflict wit Blaze
import qualified Prelude as P (head, id, div)

import           Web.Scotty as Scotty
import           Web.Cookie as Cookie
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as Postgres
import           Database.PostgreSQL.Simple.FromField as Postgres.FromField
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
import           Blaze.ByteString.Builder ( toLazyByteString )

-- TODO:
-- 1. Finish editor frontend (mostly styling)
-- 2. Pagination
-- 3. Post representations -> Where to put tags, posted time, poster, and edit button?
-- 4. Lock down blog with authentication
-- 5. Finish login & not found pages

-- Miscellania:
-- 1. 'Top 5' tags map in side bar?

parseArray :: String -> [String]
parseArray "{}" = []
parseArray ""   = []
parseArray (x:xs)
 | x == '{'  = parseArray $ init xs
 | x == '\"' = concat [[takeWhile (\c -> c /= '\"') xs], (parseArray $ tail $ dropWhile (\c -> c /= '\"') xs)]
 | x == ','  = parseArray xs
 | otherwise = concat [[takeWhile (\c -> c /= ',') (x:xs)], (parseArray $ dropWhile (\c -> c /= ',') (x:xs))]

textArrayOid :: Oid
textArrayOid = Oid 1009

data BlogPost = BlogPost {
  identifier :: Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: ZonedTimestamp,
  tags :: [String]
}

instance Show BlogPost where
  show (BlogPost i t bt ts tags) = 
    "BlogPost { identifier: " ++ (show i) ++ ", title: " ++ (T.unpack t) ++ ", body: " ++ (T.unpack bt) ++ " }"

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field

-- oid 1009, _text
instance FromField [String] where
  fromField f mdata
    | (typeOid f) /= textArrayOid = returnError Incompatible f "is not a text array"
    | otherwise = do
      case mdata of
        Nothing -> returnError UnexpectedNull f "is not a text array"
        (Just value) -> return $ parseArray $ B.unpack value

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
        
  -- this one is more webservice-y
  post "/posts/:id/tag" $ do
    ps <- params
    case (lookup "id" ps) of
      Nothing -> return ()
      Just identifier -> do
        case (lookup "tag" ps) of
          Nothing -> return ()
          Just tag -> do
            liftIO $ execute pg "UPDATE blogposts SET tags=array_append(tags,?) WHERE identifier = ? AND ? != all(tags);" (tag, ((read $ TL.unpack identifier) :: Integer), tag)
            return ()
            
  delete "/posts/:id/tag" $ do
    ps <- params
    case (lookup "id" ps) of
      Nothing -> return ()
      Just identifier -> do
        case (lookup "tag" ps) of
          Nothing -> return ()
          Just tag -> do
            liftIO $ execute pg "UPDATE blogposts SET tags=array_remove(tags,?) WHERE identifier = ? AND ? = any(tags);" (tag, ((read $ TL.unpack identifier) :: Integer), tag)
            return ()
      
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
              
  get "/token/login" $ do
    -- TODO: render login page
    return ()
    
  post "/token" $ do
    ps <- params
    case (lookup "username" ps) of
      Nothing -> Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound
      (Just username) -> case (lookup "password" ps) of
                           Nothing -> Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound
                           (Just password) -> do
                             return ()
    
  post "/token/invalidate" $ do
      return ()
      
  notFound $ do
    Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound 
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

accessToken :: ActionM (Maybe T.Text)
accessToken = do
  c <- Scotty.header "Cookie"
  return $ lookup "access_token" (parseCookiesText $ TE.encodeUtf8 $ TL.toStrict $ fromJust c)

setAccessToken :: T.Text -> ActionM ()
setAccessToken token = addHeader "Set-Cookie" (TL.fromStrict . TE.decodeUtf8 . BL.toStrict . toLazyByteString $ renderSetCookie def {
                                                                                                    setCookieName  = "token",
                                                                                                    setCookieValue = TE.encodeUtf8 token
                                                                                                  })
  
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
  script ! src "https://code.jquery.com/jquery-2.1.3.min.js" $ ""

renderTop :: Maybe T.Text -> Html
renderTop Nothing = do
  a ! href "/" $ do 
    img ! src "https://symer.io/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (stringValue $ T.unpack blogTitle)
renderTop (Just title) = do
  renderTop Nothing
  h2 ! class_ "title" ! A.id "blog-title" $ toHtml blogTitle
  h3 ! A.id "blog-subtitle" $ "a blog about code."
  
renderPost :: BlogPost -> Html
renderPost b = do
  div ! class_ "post" $ do
    a ! href (stringValue $ (++) "/posts/" $ show $ Main.identifier b) $ do
      h1 ! class_ "post-title" $ toHtml $ Main.title b -- BlogPost
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def (Main.body b) -- BlogPost

renderPosts :: [BlogPost] -> Html
renderPosts [] = return ()
renderPosts [x] = renderPost x
renderPosts (x:xs) = do
  renderPost x
  hr ! class_ "separator"
  renderPosts xs
  
renderMdEditor :: Maybe BlogPost -> Html
renderMdEditor Nothing = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" $ ""

renderMdEditor (Just blogPost) = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" ! customAttribute "post-id" (stringValue $ show $ Main.identifier blogPost) $ toHtml $ T.unpack $ Main.body blogPost
      
renderTitleField :: Maybe BlogPost -> Html
renderTitleField (Just blogPost) = input ! type_ "text" ! id "title-field" ! value (stringValue $ T.unpack $ Main.title blogPost)
renderTitleField Nothing = input ! type_ "text" ! id "title-field"

renderTagEditor :: Maybe BlogPost -> Html
renderTagEditor Nothing = return ()
renderTagEditor (Just (BlogPost identifier title body timestamp tags)) = do
  textarea ! A.id "tags" ! class_ "wordlist" $ do toHtml $ List.intercalate ", " tags
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  renderTitleField maybeBlogPost
  renderMdEditor maybeBlogPost
  renderTagEditor maybeBlogPost
  
  div ! A.id "buttons" $ do
    button ! A.id "cancel-button" $ "Cancel"
    button ! A.id "delete-button" $ "Delete"
    button ! A.id "preview-button" $ "Preview"
    button ! A.id "save-button" $ "Save"
    
  script ! src "/assets/marked.min.js" $ ""
  script ! src "/assets/editor.js" $ ""
  
-------------------------------------------------------------------------------
--- | Database
  
getBlogPosts :: Postgres.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing Nothing = query_ pg "SELECT * FROM blogposts ORDER BY identifier"
getBlogPosts pg (Just from) Nothing = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10 ORDER BY identifier" [from]
getBlogPosts pg Nothing (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10 ORDER BY identifier" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier" (from, untill)
