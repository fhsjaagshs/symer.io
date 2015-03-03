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
import qualified Data.List.Split as List
import           Data.Maybe
import           Data.DateTime -- fromSqlString :: String -> Maybe DateTime
import           Data.Text.Encoding as TE
import           Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
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

import           Cheapskate
import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Text.Blaze.Internal
import           Blaze.ByteString.Builder (toLazyByteString)
import           Data.Aeson as Aeson

-- TODO:
-- 1. Finish pages
--    a. Login
--    b. not found
--    c. post editor
--    d. post representation (Where to put tags, posted time, poster, and edit button?)
--    e. posts by time
-- 2. Pagination
-- 3. Lock down blog with authentication
-- 4. Asset pipeline (minify JS and CSS)

-- POST TODO:
-- 1. Refactor

-- Miscellania:
-- 1. 'Top 5' tags map in side bar?

textArrayOid :: Oid
textArrayOid = Oid 1009

emptyTextArrayOid :: Oid
emptyTextArrayOid = Oid 17233

data BlogPost = BlogPost {
  identifier :: Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: ZonedTimestamp,
  tags :: [String]
}

instance Show BlogPost where
  show (BlogPost i t bt ts tags) = 
    "BlogPost { identifier: " ++ (show i) ++ ", title: " ++ (T.unpack t) ++ ", body: " ++ (T.unpack bt) ++ ", tags:" ++ (show tags) ++ " }"

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field
  
instance ToJSON BlogPost where
  toJSON (BlogPost identifier title body timestamp tags) =
    Aeson.object [
      "id" .= identifier,
      "title" .= title,
      "body" .= body,
      "timestamp" .= ((Aeson.String $ TE.decodeUtf8 $ BL.toStrict $ toLazyByteString $ zonedTimestampToBuilder timestamp) :: Value),
      "tags" .= ((Aeson.Array $ Vector.fromList $ Prelude.map (Aeson.String . T.pack) tags) :: Value)
    ]
    
-- oid 1009, _text
instance FromField [String] where
  fromField f mdata
    | ((typeOid f) /= textArrayOid) && ((typeOid f) /= emptyTextArrayOid) = returnError Incompatible f "is not a text array"
    | otherwise = do
      case mdata of
        Nothing -> return []
        (Just value) -> return $ parseArray $ B.unpack value


main = scotty 3000 $ do
  pg <- liftIO $ Postgres.connectPostgreSQL "dbname='nathaniel' user='nathaniel' password='' port='5432'"
 -- redis <- liftIO $ Redis.connect Redis.defaultConnectInfo
 
  middleware $ staticPolicy (noDots >-> (hasPrefix "assets") >-> addBase "public")
  
  -- 
  -- Posts
  -- These are HTML pages
  --
  
  -- blog root
  get "/" $ do
    ps <- params
    let mfrom = ((read . T.unpack . TL.toStrict <$> (lookup "from" ps)) :: Maybe Integer)
    let muntil = ((read . T.unpack . TL.toStrict <$> (lookup "until" ps)) :: Maybe Integer)
    res <- liftIO $ getBlogPosts pg mfrom muntil
    
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop $ Just blogTitle
        div ! A.id "content" $ do
          renderPosts res
          
  -- create a post
  get "/posts/new" $ do
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop $ Just blogTitle
        div ! A.id "content" $ do
          renderPostEditor Nothing
          
  -- view a specific post
  get "/posts/:id" $ do
    identifier <- param "id"
    res <- liftIO $ query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
  
    if (Prelude.null res)
      then Scotty.html $ R.renderHtml $ renderNotFound
      else do
       Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead $ appendedBlogTitle (Main.title $ P.head res)
          H.body ! style "text-align: center;" $ do
            renderTop $ Just blogTitle
            div ! A.id "content" $ do
              renderPosts res
            
  -- edit a post      
  get "/posts/:id/edit" $ do
    identifier <- param "id"
    res <- liftIO $ query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
    if (Prelude.null res)
      then Scotty.html $ R.renderHtml $ renderNotFound
      else do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead $ appendedBlogTitle (Main.title $ P.head res)
          H.body ! style "text-align: center;" $ do
            renderTop Nothing
            div ! A.id "content" $ do
              renderPostEditor (Just (P.head res))
          
  --
  -- Post creation/deletion
  -- These endpoints will return JSON or an empty string
  --
  
  -- deletes a BlogPost from the database
  delete "/posts/:id" $ do
    identifier <- param "id"
    res <-  liftIO $ listToMaybe <$> query pg "DELETE FROM blogposts WHERE id=? RETURNING *" [identifier :: Integer]
    case res of
      Nothing -> emptyResponse
      Just bp -> Scotty.json (bp :: BlogPost)

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    ps <- params
    maybeBlogPost <- liftIO $ (upsertBlogPost pg
                                             (((read . TL.unpack) <$> lookup "id" ps) :: Maybe Integer)
                                             (TL.toStrict <$> lookup "title" ps)
                                             (TL.toStrict <$> lookup "body" ps)
                                             (((List.splitOn ","). TL.unpack) <$> (lookup "tags" ps))
                                            )
    case maybeBlogPost of
      Nothing -> emptyResponse
      Just bp -> do
        baseurl <- T.unpack <$> baseURL
        addHeader "Location" (TL.pack (baseurl ++ "/posts/" ++ (show $ Main.identifier bp)))
        Scotty.json (bp :: BlogPost)
        
  --
  -- Tags
  -- each endpoint returns no body
  --
  post "/posts/:id/tag" $ do
    identifier <- param "id"
    maybeTag <- (lookup "tag") <$> params
    case maybeTag of
      Nothing -> emptyResponse
      Just tag -> do
        liftIO $ execute pg "UPDATE blogposts SET tags=array_append(tags,?) WHERE identifier = ? AND ? != all(tags);" (tag, ((read $ TL.unpack identifier) :: Integer), tag)
        emptyResponse
            
  delete "/posts/:id/tag" $ do
    identifier <- param "id"
    maybeTag <- (lookup "tag") <$> params
    case maybeTag of
      Nothing -> emptyResponse
      Just tag -> do
        liftIO $ execute pg "UPDATE blogposts SET tags=array_remove(tags,?) WHERE identifier = ? AND ? = any(tags);" (tag, ((read $ TL.unpack identifier) :: Integer), tag)
        emptyResponse
        
      
  -- get "/posts/on/:year" $ do
  --   return ()
  --
  -- get "/posts/on/:year/:month" $ do
  --   return ()
  --
  -- get "/posts/on/:year/:month/:day" $ do
  --   return ()
              
  -- get "/token/login" $ do
  --   return ()
  --
  -- post "/token" $ do
  --   ps <- params
  --   case (lookup "username" ps) of
  --     Nothing -> Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound
  --     (Just username) -> case (lookup "password" ps) of
  --                          Nothing -> Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound
  --                          (Just password) -> do
  --                            return ()
  --
  -- post "/token/invalidate" $ do
  --     return ()
      
  -- not found handler
  notFound $ do Scotty.html $ R.renderHtml $ docTypeHtml $ renderNotFound 

-------------------------------------------------------------------------------
--- | Helpers

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle text = T.append text (T.append " | " blogTitle)

baseURL :: ActionM T.Text
baseURL = do
  h <- Scotty.header "Host"
  req <- request
  return $ T.pack $ (if (isSecure req) then "https" else "http") ++ "://" ++ (TL.unpack $ fromJust h)
  
emptyResponse :: ActionM ()
emptyResponse = Scotty.text ""

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
    button ! A.id "delete-button" $ "Delete"
    button ! A.id "preview-button" $ "Preview"
    button ! A.id "save-button" $ "Save"
    
  script ! src "/assets/marked.min.js" $ ""
  script ! src "/assets/editor.js" $ ""
  
-------------------------------------------------------------------------------
--- | Database

parseArray :: String -> [String]
parseArray "{}" = []
parseArray ""   = []
parseArray (x:xs)
 | x == '{'  = parseArray $ init xs
 | x == '\"' = concat [[takeWhile (\c -> c /= '\"') xs], (parseArray $ tail $ dropWhile (\c -> c /= '\"') xs)]
 | x == ','  = parseArray xs
 | otherwise = concat [[takeWhile (\c -> c /= ',') (x:xs)], (parseArray $ dropWhile (\c -> c /= ',') (x:xs))]
 
serializeArray :: [String] -> String
serializeArray [] = "Array[]"
serializeArray values = "Array[" ++ (List.intercalate "," (map stringLiteral values)) ++ "]"

stringLiteral :: String -> String
stringLiteral s = "'" ++ (List.intercalate [] (map quoteFormat s)) ++ "'"

quoteFormat :: Char -> String
quoteFormat '\'' = "''"
quoteFormat c = [c]

upsertBlogPost :: Postgres.Connection -> Maybe Integer -> Maybe T.Text -> Maybe T.Text -> Maybe [String] -> IO (Maybe BlogPost)
upsertBlogPost pg (Just identifier) Nothing Nothing Nothing = listToMaybe <$> query pg "UDPATE blogposts WHERE identifier = ? RETURNING *" [identifier]
upsertBlogPost pg (Just identifier) (Just title) Nothing Nothing = listToMaybe <$> query pg "UDPATE blogposts SET title = ? WHERE identifier = ? RETURNING *" (title, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) Nothing = listToMaybe <$> query pg "UDPATE blogposts SET title = ?, bodyText = ? WHERE identifier = ? RETURNING *" (title, body, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) (Just tags) = listToMaybe <$> query pg "UDPATE blogposts SET title = ?, bodyText = ?, tags = ? WHERE identifier = ? RETURNING *" (title, body, (serializeArray tags), identifier)
upsertBlogPost pg Nothing (Just title) (Just body) Nothing = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" (title, body)
upsertBlogPost pg Nothing (Just title) (Just body) (Just tags) = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText, tags) VALUES (?, ?, ?) RETURNING *" (title, body, (serializeArray tags))
upsertBlogPost _ _ _ _ _ = return Nothing
  
getBlogPosts :: Postgres.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing Nothing = query_ pg "SELECT * FROM blogposts ORDER BY identifier"
getBlogPosts pg (Just from) Nothing = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10 ORDER BY identifier" [from]
getBlogPosts pg Nothing (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10 ORDER BY identifier" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier" (from, untill)
