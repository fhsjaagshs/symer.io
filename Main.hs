{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}

module Main (main) where

import           GHC.Generics

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import           Data.Maybe
import           Data.Time.Calendar as Calendar
import           Data.Time.Lens
import           Data.Time.Clock

import qualified Data.Vector as Vector
import qualified Data.HashMap as Map
import           Prelude hiding (head, id, div) -- hide the functions that may conflict wit Blaze
import qualified Prelude as P (head, id, div)

import           Text.Jasmine as Jasmine
import           Web.Scotty as Scotty
import           Web.Cookie as Cookie
import           Network.Wai
import           Network.Wai.Middleware.Static
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as Postgres
import           Database.PostgreSQL.Simple.FromField as Postgres.FromField
import           Database.PostgreSQL.Simple.ToField as Postgres.ToField
import           Database.PostgreSQL.Simple.FromRow as Postgres.FromRow
import           Database.PostgreSQL.Simple.Time as Postgres.Time
import           Database.PostgreSQL.Simple.Arrays as Postgres.Arrays

import           Auth

import           Cheapskate
import           Data.Aeson as Aeson
import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Text.Blaze.Internal
import           Blaze.ByteString.Builder (toLazyByteString)
import           Data.Text.Lazy.Builder

-- TODO:
-- 1. Finish pages
--    a. Login
--    b. not found
--    d. post representation (Where to put tags, poster)
--    e. posts by time
-- 2. Pagination
-- 3. Lock down blog with authentication

-- Nitpick todo:
-- 1. upsertBlogPost: more parameter combinations
-- 2. fix timezone

-- Miscellania:
-- 1. 'Top 5' tags map in side bar?

-------------------------------------------------------------------------------
--- | Make more Postgres types available

-- oid 1009, _text
instance FromField [String] where
  fromField f mdata
    | (typeOid f) /= (Oid 1009) = returnError Incompatible f "Field is not a text array."
    | otherwise = (if (isJust mdata) then return (parseArray $ B.unpack $ fromJust mdata) else return [])

instance ToField [String] where
  toField strValue = Many $ map Postgres.ToField.Escape (["{"] ++ (map B.pack strValue) ++ ["}"])

-------------------------------------------------------------------------------
--- | Typeclasses

class Composable a where
  render :: a -> Maybe AuthUser -> Html
  renderBasic :: a -> Html
  renderBasic comp = render comp Nothing
  
-------------------------------------------------------------------------------
--- | AuthUser data type
  
data AuthUser = AuthUser {
  username :: T.Text,
  uid :: Integer
} deriving (Eq, Show, Generic)

instance FromJSON AuthUser
instance ToJSON AuthUser

-------------------------------------------------------------------------------
--- | BlogPost data type

data BlogPost = BlogPost {
  identifier :: Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: UTCTime,
  tags :: [String]
}

instance Eq BlogPost where
  (==) (BlogPost idOne _ _ _ _) (BlogPost idTwo _ _ _ _) = ((==) idOne idTwo)

instance Show BlogPost where
  show (BlogPost i t bt ts tags) = 
    "BlogPost { identifier: " ++ (show i) ++ ", title: " ++ (T.unpack t) ++ ", body: " ++ (show $ T.length bt) ++ " characters, tags:" ++ (show tags) ++ " }"

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field
  
instance ToJSON BlogPost where
  toJSON (BlogPost identifier title body timestamp tags) =
    Aeson.object [
      "id" .= identifier,
      "title" .= title,
      "body" .= body,
      "timestamp" .= ((Aeson.String $ T.decodeUtf8 $ BL.toStrict $ toLazyByteString $ utcTimeToBuilder timestamp) :: Value),
      "tags" .= ((Aeson.Array $ Vector.fromList $ Prelude.map (Aeson.String . T.pack) tags) :: Value)
    ]
    
instance Composable BlogPost where
  render (BlogPost identifier title body timestamp tags) Nothing = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier) $ do
      h1 ! class_ "post-title" $ toHtml title
    h4 ! class_ "post-subtitle" $ toHtml $ formatDate timestamp
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body
  render (BlogPost identifier title body timestamp tags) (Just authUser) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier) $ do
      h1 ! class_ "post-title" $ toHtml title
    h4 ! class_ "post-subtitle" $ toHtml $ formatDate timestamp
    a ! class_ "post-edit-button" ! href (stringValue $ ("/posts/" ++ (show identifier) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body
    
instance Composable [BlogPost] where
  render [] _ = return ()
  render [x] authUser = render x authUser
  render (x:xs) authUser = do
    render x authUser
    hr ! class_ "separator"
    render xs authUser
    
main = scotty 3000 $ do
  pg <- liftIO $ Postgres.connectPostgreSQL "dbname='nathaniel' user='nathaniel' password='' port='5432'"
  redis <- liftIO $ Redis.connect Redis.defaultConnectInfo
 
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
    posts <- liftIO $ getBlogPosts pg mfrom muntil
    
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] blogTitle
      H.body ! style "text-align: center;" $ do
        renderTop (Just blogTitle) (Just blogSubtitle)
        div ! A.id "content" $ do
          renderBasic posts
          
  -- create a post
  get "/posts/new" $ do
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] $ appendedBlogTitle "New Post"
      H.body ! style "text-align: center;" $ do
        renderTop Nothing Nothing
        div ! A.id "content" $ do
          renderPostEditor Nothing
          
  -- view a specific post
  get "/posts/:id" $ do
    identifier <- param "id"
    res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
  
    case res of
      Nothing -> Scotty.html $ R.renderHtml $ renderNotFound
      Just post -> do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead [] (postTags $ Main.tags post) $ appendedBlogTitle $ Main.title post
          H.body ! style "text-align: center;" $ do
            renderTop (Just blogTitle) (Just blogSubtitle)
            div ! A.id "content" $ do
              renderBasic post

  -- edit a post      
  get "/posts/:id/edit" $ do
    identifier <- param "id"
    res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
    case res of
      Nothing -> Scotty.html $ R.renderHtml $ renderNotFound
      Just post -> do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] (appendedBlogTitle $ Main.title post)
          H.body ! style "text-align: center;" $ do
            renderTop Nothing Nothing
            div ! A.id "content" $ do
              renderPostEditor $ Just post
        
          
  --
  -- Post creation/deletion
  -- These endpoints will return JSON or an empty string
  --
  
  -- deletes a BlogPost from the database
  delete "/posts/:id" $ do
    identifier <- param "id"
    res <-  liftIO $ listToMaybe <$> query pg "DELETE FROM blogposts WHERE identifier=? RETURNING *" [identifier :: Integer]
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
                                             (((List.splitOn ",") . TL.unpack) <$> (lookup "tags" ps))
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
        
  --
  -- assets
  --
  
  get "/assets/js/:filename" $ do
    filename <- param "filename"
    setHeader "Content-Type" "application/x-javascript"
    minified <- liftIO $ TL.decodeUtf8 <$> Jasmine.minifyFile ("public/assets/" ++ (T.unpack filename))
    Scotty.text minified
    
  get "/assets/css/:filename" $ do
    filename <- param "filename"
    setHeader "Content-Type" "text/css"
    f <- liftIO $ BL.readFile ("public/assets/" ++ (T.unpack filename))
    case (CSS.renderNestedBlocks <$> (CSS.parseNestedBlocks $ TL.toStrict $ TL.decodeUtf8 f)) of
      Left string -> Scotty.text $ TL.decodeUtf8 f
      Right cssbuilder -> Scotty.text $ toLazyText cssbuilder

  -- get "/posts/on/:year" $ do
  --   return ()
  --
  -- get "/posts/on/:year/:month" $ do
  --   return ()
  --
  -- get "/posts/on/:year/:month/:day" $ do
  --   return ()
  
  get "/token/login" $ do
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [("robots","noindex, nofollow")] $ appendedBlogTitle "Login"
      H.body ! style "text-align: center;" $ do
        renderTop (Just "Login") Nothing
        div ! A.id "content" $ do
          H.form ! action "/token" ! method "POST" $ do
            input ! A.id "usernameinput" ! type_ "text" ! A.name "username" ! placeholder "Username"
            input ! A.id "passwordinput" ! type_ "password" ! A.name "password" ! placeholder "Password"

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
--- | Authentication

accessToken :: ActionM (Maybe T.Text)
accessToken = do
  c <- Scotty.header "Cookie"
  return $ lookup "access_token" $ parseCookiesText $ BL.toStrict $ TL.encodeUtf8 $ fromJust c

setAccessToken :: T.Text -> ActionM ()
setAccessToken token = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie def { setCookieName  = "token", setCookieValue = T.encodeUtf8 token })

-------------------------------------------------------------------------------
--- | Helpers

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: T.Text
blogSubtitle = "a blog about code."

seoTags :: [(T.Text, T.Text)]
seoTags = [
            ("revisit-after", "2 days"),
            ("description", "Rants and raves about computer science, functional programming, politics, and everything in between."),
            ("keywords", "computer science, politics, haskell, ruby, web development, art, blogs, money, computers, startups, tutorial, rails, ruby on rails, scotty haskell, snap framework")
            ]

postTags :: [String] -> [(T.Text, T.Text)]       
postTags tags = [("keywords", T.append (T.pack $ (List.intercalate ", " tags) ++ ", ") (fromJust $ lookup "keywords" seoTags))]

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle text = T.append text (T.append " | " blogTitle)

baseURL :: ActionM T.Text
baseURL = do
  h <- Scotty.header "Host"
  req <- request
  return $ T.pack $ (if (isSecure req) then "https" else "http") ++ "://" ++ (TL.unpack $ fromJust h)
  
emptyResponse :: ActionM ()
emptyResponse = Scotty.text ""

showInteger :: Int -> Int -> String
showInteger numPlaces integer = (replicate (numPlaces-(length $ show integer)) '0') ++ (show integer)
                                                                                                   
formatDate :: UTCTime -> String
formatDate date = (show $ getL month date) ++ " • " ++ (show $ getL day date) ++ " • " ++ (show $ getL year date) ++ " | " ++ (showInteger 2 (getL hours date)) ++ ":" ++ (showInteger 2 (getL minutes date)) ++  " UTC"

-------------------------------------------------------------------------------
--- | HTML rendering

renderNotFound :: Html
renderNotFound = do
  return ()

renderTags :: [(T.Text, T.Text)] -> Html
renderTags [] = return ()
renderTags (x:xs) = do
  meta ! A.name (textValue $ fst x) ! content (textValue $ snd x)
  renderTags xs
  
renderCssLinks :: [T.Text] -> Html
renderCssLinks [] = return ()
renderCssLinks (x:xs) = do
  link ! href (textValue x) ! rel "stylesheet" ! type_ "text/css"
  renderCssLinks xs

renderHead :: [T.Text] -> [(T.Text, T.Text)] -> T.Text -> Html
renderHead cssFiles metaTags title = H.head $ do
  H.title $ toHtml title
  renderCssLinks $ cssFiles ++ ["https://symer.io/assets/css/site.css", "/assets/css/blog.css"]
  script ! src "https://code.jquery.com/jquery-2.1.3.min.js" $ ""
  
  meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"

  renderTags $ List.nubBy (\(keyOne, _) (keyTwo, _) -> keyOne == keyTwo) $ metaTags ++ seoTags

renderTop :: Maybe T.Text -> Maybe T.Text -> Html
renderTop Nothing Nothing = do
  a ! href "/" $ do 
    img ! src "https://symer.io/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (textValue blogTitle)
renderTop (Just title) Nothing = do
  renderTop Nothing Nothing
  h2 ! class_ "title" ! A.id "blog-title" $ toHtml title
renderTop (Just title) (Just subtitle) = do
  renderTop (Just title) Nothing
  h3 ! A.id "blog-subtitle" $ toHtml subtitle
  
renderMdEditor :: Maybe BlogPost -> Html
renderMdEditor Nothing = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" $ ""
renderMdEditor (Just (BlogPost identifier _ body _ _)) = do
  div ! A.id "preview" $ ""
  textarea ! A.id "editor" ! customAttribute "post-id" (stringValue $ show identifier) $ toHtml body
      
renderTitleField :: Maybe BlogPost -> Html
renderTitleField (Just (BlogPost _ title _ _ _)) = input ! type_ "text" ! id "title-field" ! placeholder "Post title" ! value (textValue title)
renderTitleField Nothing = input ! type_ "text" ! id "title-field" ! placeholder "Post title"

renderTagEditor :: Maybe BlogPost -> Html
renderTagEditor Nothing = textarea ! A.id "tags" ! class_ "wordlist" $ do ""
renderTagEditor (Just (BlogPost _ _ _ _ tags)) = do
  textarea ! A.id "tags" ! class_ "wordlist" $ do toHtml $ List.intercalate ", " tags
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  renderTitleField maybeBlogPost
  renderMdEditor maybeBlogPost
  renderTagEditor maybeBlogPost
  
  a ! A.id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! A.id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! A.id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"

  script ! src "/assets/marked.min.js" $ ""
  script ! src "/assets/js/editor.js" $ ""
  
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

upsertBlogPost :: Postgres.Connection -> Maybe Integer -> Maybe T.Text -> Maybe T.Text -> Maybe [String] -> IO (Maybe BlogPost)
upsertBlogPost pg (Just identifier) Nothing      Nothing     Nothing     = listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier]
upsertBlogPost pg (Just identifier) (Just title) Nothing     Nothing     = listToMaybe <$> query pg "UPDATE blogposts SET title=? WHERE identifier=? RETURNING *" (title, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) Nothing     = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=? WHERE identifier = ? RETURNING *" (title, body, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) (Just tags) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=? WHERE identifier = ? RETURNING *" (title, body, tags, identifier)
upsertBlogPost pg Nothing           (Just title) (Just body) Nothing     = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" (title, body)
upsertBlogPost pg Nothing           (Just title) (Just body) (Just tags) = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText, tags) VALUES (?, ?, ?) RETURNING *" (title, body, tags)
upsertBlogPost _  _                 _            _           _           = return Nothing
  
getBlogPosts :: Postgres.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing     Nothing       = query_ pg "SELECT * FROM blogposts ORDER BY identifier"
getBlogPosts pg (Just from) Nothing       = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10 ORDER BY identifier" [from]
getBlogPosts pg Nothing     (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10 ORDER BY identifier" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier" (from, untill)
