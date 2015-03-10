{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}

module Main (main) where

import           GHC.Generics

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Exception
import           Data.Monoid
import           Data.Maybe

import           Data.Char
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Vector as Vector
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import           Text.Jasmine as Jasmine

import           Data.Time.Lens
import           Data.Time.Clock

import           Prelude hiding (head, id, div) -- hide the functions that may conflict wit Blaze
import qualified Prelude as P (head, id, div)

import           Web.Scotty as Scotty
import           Web.Cookie as Cookie
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import qualified Database.Redis as Redis
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.Time as PG.Time
import           Database.PostgreSQL.Simple.Migration as PG.Migration
import           Crypto.BCrypt

import           Auth
import           Config

import           Cheapskate
import           Data.Aeson as Aeson
import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text as R
import           Blaze.ByteString.Builder (toLazyByteString, fromByteString)
import           Data.Text.Lazy.Builder

-- TODO:
-- 1. Finish pages
--    a. login
--    b. not found
--    c. unauthorized
-- 2. Pagination

-- Nitpick todo:
-- 1. fix timezone
-- 2. Rewrite using a state monad?
-- 3. blog post author

-- Miscellania:
-- 1. 'Top 5' tags map in side bar?

-------------------------------------------------------------------------------
--- | Make more PG types available

-- oid 1009, _text
instance FromField [String] where
  fromField f mdata
    | (typeOid f) /= (Oid 1009) = returnError Incompatible f "Field is not a text array."
    | otherwise = (if (isJust mdata) then return (parseArray $ B.unpack $ fromJust mdata) else return [])

instance ToField [String] where
  toField [] = PG.ToField.Plain $ fromByteString "ARRAY[]"
  toField v = Many [PG.ToField.Plain $ fromByteString "ARRAY[",
                    Many $ map (\str -> do
                      Many [(Escape $ B.pack str), PG.ToField.Plain $ fromByteString ","]
                    ) (Prelude.init v),
                    Escape $ B.pack $ last v,
                    PG.ToField.Plain $ fromByteString "]"
                    ]

-------------------------------------------------------------------------------
--- | Typeclasses

class Composable a where
  render :: a -> Maybe User -> Html
  renderBasic :: a -> Html
  renderBasic comp = render comp Nothing
  
-------------------------------------------------------------------------------
--- | User data type
  
data User = User {
  uid :: Integer,
  username :: T.Text,
  displayName :: T.Text,
  passwordHash :: T.Text
} deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

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
    h4 ! class_ "post-subtitle" $ toHtml $ mconcat tags
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body
  render (BlogPost identifier title body timestamp tags) (Just user) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier) $ do
      h1 ! class_ "post-title" $ toHtml title
    h4 ! class_ "post-subtitle" $ toHtml $ formatDate timestamp
    h4 ! class_ "post-subtitle" $ toHtml $ List.intercalate ", " tags
    a ! class_ "post-edit-button" ! href (stringValue $ ("/posts/" ++ (show identifier) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body
    
instance Composable [BlogPost] where
  render [] _ = return ()
  render [x] user = render x user
  render (x:xs) user = do
    render x user
    hr ! class_ "separator"
    render xs user
    
main = scotty 3000 $ do
  pg <- liftIO $ PG.connectPostgreSQL $ B.pack $ Config.postgresConnStr
  redis <- liftIO $ Redis.connect Redis.defaultConnectInfo
  
  liftIO $ withTransaction pg $ runMigration $ MigrationContext MigrationInitialization True pg
  liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "blog.sql" "./migrations/blog.sql") True pg
  liftIO $ withTransaction pg $ runMigration $ MigrationContext (MigrationFile "array_funcs.sql" "./migrations/array_funcs.sql") True pg

  -- Serve files like a Ruby Rack app
  middleware $ staticPolicy (noDots >-> (hasPrefix "assets") >-> addBase "public")
  
  -- shortcuts for auth
  let protected = checkAuth redis
  let authenticatedUser = getUser redis
  
  -- 
  -- Posts
  -- These are HTML pages
  --
  
  -- blog root
  get "/" $ do
    maybeUser <- authenticatedUser
    ps <- params
    let mfrom = ((read . T.unpack . TL.toStrict <$> (lookup "from" ps)) :: Maybe Integer)
    let muntil = ((read . T.unpack . TL.toStrict <$> (lookup "until" ps)) :: Maybe Integer)
    posts <- liftIO $ getBlogPosts pg mfrom muntil
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] blogTitle
      renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
        render posts maybeUser
        
  -- create a post
  get "/posts/new" $ do
    protected
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] $ appendedBlogTitle "New Post"
      renderBody Nothing Nothing Nothing $ do
        renderPostEditor Nothing
  
  -- view a specific post
  get "/posts/:id" $ do
    identifier <- param "id"
    maybeUser <- authenticatedUser
    res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
    case res of
      Nothing -> next
      Just post -> do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead [] (postTags $ Main.tags post) $ appendedBlogTitle $ Main.title post
          renderBody (Just blogTitle) (Just blogSubtitle) maybeUser $ do
            render post maybeUser
  
  get "/posts/by/tag/:tag" $ do
    tag <- param "tag"
    maybeUser <- authenticatedUser
    posts <- liftIO $ getBlogPostsByTag pg tag Nothing Nothing
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [] blogTitle
      renderBody Nothing Nothing Nothing $ do
        render (posts :: [BlogPost]) maybeUser

  -- edit a post
  get "/posts/:id/edit" $ do
    protected
    identifier <- param "id"
    res <- liftIO $ listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier :: Integer]
    case res of
      Nothing -> next
      Just post -> do
        Scotty.html $ R.renderHtml $ docTypeHtml $ do
          renderHead ["/assets/css/editor.css","/assets/css/wordlist.css"] [("robots","noindex, nofollow")] (appendedBlogTitle $ Main.title post)
          renderBody Nothing Nothing Nothing $ do
            renderPostEditor $ Just post

  --
  -- Post creation/deletion
  -- Returns json or nothing
  --
  
  -- deletes a BlogPost from the database
  delete "/posts/:id" $ do
    protected
    identifier <- param "id"
    res <- liftIO $ listToMaybe <$> query pg "DELETE FROM blogposts WHERE identifier=? RETURNING *" [identifier :: Integer]
    case res of
      Nothing -> emptyResponse
      Just bp -> Scotty.json (bp :: BlogPost)

  -- creates/updates a BlogPost in the database
  post "/posts" $ do
    protected
    ps <- params
    maybeBlogPost <- liftIO $ (upsertBlogPost pg
                                             (((read . TL.unpack) <$> lookup "id" ps) :: Maybe Integer)
                                             (TL.toStrict <$> lookup "title" ps)
                                             (TL.toStrict <$> lookup "body" ps)
                                             (((List.splitOn ",") . TL.unpack) <$> (lookup "tags" ps))
                                             (((List.splitOn ",") . TL.unpack) <$> (lookup "deleted_tags" ps))
                                            )
    case maybeBlogPost of
      Nothing -> emptyResponse
      Just bp -> do
        addHeader "Location" $ TL.pack $ "/posts/" ++ (show $ Main.identifier bp)
        liftIO $ print bp
        Scotty.json (bp :: BlogPost)
        -- redirect $ TL.pack $ "/posts/" ++ (show $ Main.identifier bp)

  --
  -- assets
  -- JS & CSS
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

  --
  -- Login and Auth
  -- HTML pages
  --
  get "/login" $ do
    maybeUser <- (getUser redis)
    when (isJust maybeUser) (redirect "/")
    ps <- params
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [("robots","noindex, nofollow")] $ appendedBlogTitle "Login"
      renderBody (Just "Login") Nothing Nothing $ do
        case (lookup "error_message" ps) of
          Nothing -> return ()
          (Just msg) -> h5 $ toHtml msg
        H.form ! action "/login" ! method "POST" $ do
          input ! type_ "hidden" ! A.name "source" ! value "form" 
          input ! A.id "usernameinput" ! type_ "text" ! A.name "username" ! placeholder "Username" 
          input ! A.id "passwordinput" ! type_ "password" ! A.name "password" ! placeholder "Password"
          input ! A.id "submitinput" ! type_ "submit" ! value "Submit"
          
  get "/logout" $ do
    atoken <- accessToken
    liftIO $ Auth.deleteObject redis (T.encodeUtf8 <$> atoken)
    redirect "/"

  post "/login" $ do
    pUsername <- param "username" :: ActionM T.Text
    pPassword <- param "password" :: ActionM T.Text

    res <- liftIO $ listToMaybe <$> (query pg "SELECT * FROM users WHERE username=? LIMIT 1" [pUsername] :: IO [User])
    
    case res of
      (Just user) -> do
        if validatePassword (T.encodeUtf8 $ Main.passwordHash user) (T.encodeUtf8 pPassword)
          then do
            token <- liftIO $ Auth.saveObject redis user
            setAccessToken (T.decodeUtf8 token)
            redirect "/"
          else redirect "/login?error_message=Invalid%20password%2E"
      _           -> redirect "/login?error_message=Username%20not%20found%2E"
      
  matchAny "/unauthorized" $ do
    status $ Status 401 "Unauthorized"
    Scotty.html $ R.renderHtml $ docTypeHtml $ do
      renderHead [] [("robots","noindex, nofollow")] (appendedBlogTitle "Unauthorized")
      renderBody (Just "Unauthorized") Nothing Nothing $ do
        h1 $ "Authorization is required beyond this point."
        
  defaultHandler (\e -> liftIO $ print e)

  notFound $ Scotty.html $ R.renderHtml $ docTypeHtml $ h1 $ toHtml $ ("Not Found." :: T.Text)

-------------------------------------------------------------------------------
--- | Authentication

accessToken :: ActionM (Maybe T.Text)
accessToken = (Scotty.header "Cookie") >>= \v -> return $ if isNothing v then Nothing else (lookup "token" $ parseCookiesText $ BL.toStrict $ TL.encodeUtf8 $ fromJust v)

setAccessToken :: T.Text -> ActionM ()
setAccessToken token = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie def { setCookieName  = "token", setCookieValue = T.encodeUtf8 token })

getUser :: Redis.Connection -> ActionM (Maybe User)
getUser redis = do
  atoken <- accessToken
  liftIO $ Auth.refreshObject redis (T.encodeUtf8 <$> atoken)
  maybeUser <- liftIO $ Auth.getObject redis (T.encodeUtf8 <$> atoken)
  return maybeUser
  
checkAuth :: Redis.Connection -> ActionM (Maybe User)
checkAuth redis = do
  maybeUser <- (getUser redis)
  case maybeUser of
    (Just user) -> return $ Just user
    _ -> do
      return Nothing
      redirect "/unauthorized"

-------------------------------------------------------------------------------
--- | Helpers

blogTitle :: T.Text
blogTitle = "Segmentation Fault (core dumped)"

blogSubtitle :: T.Text
blogSubtitle = "a blog about code."

appendedBlogTitle :: T.Text -> T.Text
appendedBlogTitle text = T.append text (T.append " | " blogTitle)

seoTags :: [(T.Text, T.Text)]
seoTags = [
            ("revisit-after", "2 days"),
            ("description", "Rants and raves about computer science, functional programming, politics, and everything in between."),
            ("keywords", "computer science, politics, haskell, ruby, web development, art, blogs, money, computers, startups, tutorial, rails, ruby on rails, scotty haskell, snap framework")
            ]

postTags :: [String] -> [(T.Text, T.Text)]       
postTags tags = [("keywords", T.append (T.pack $ (List.intercalate ", " tags) ++ ", ") (fromJust $ lookup "keywords" seoTags))]

baseURL :: ActionM T.Text
baseURL = do
  h <- Scotty.header "Host"
  req <- request
  return $ T.pack $ (if (isSecure req) then "https" else "http") ++ "://" ++ (TL.unpack $ fromJust h)
  
emptyResponse :: ActionM ()
emptyResponse = Scotty.text ""
                                                                                                   
formatDate :: UTCTime -> String
formatDate date = (show $ getL month date) ++ " • " ++ (show $ getL day date) ++ " • " ++ (show $ getL year date) ++ " | " ++ (showInteger 2 (getL hours date)) ++ ":" ++ (showInteger 2 (getL minutes date)) ++  " UTC"

showInteger :: Int -> Int -> String
showInteger numPlaces integer = (replicate (numPlaces-(length $ show integer)) '0') ++ (show integer)

-------------------------------------------------------------------------------
--- | HTML rendering

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

renderBody :: Maybe T.Text -> Maybe T.Text -> Maybe User -> Html -> Html
renderBody maybeTitle maybeSubtitle maybeUser bodyHtml = H.body ! style "text-align: center;" $ do
  a ! href "/" $ do
    img ! src "https://symer.io/assets/images/philly_skyline.svg" ! width "300" ! height "200" ! alt (textValue blogTitle)
  
  when (isJust maybeTitle) (h2 ! class_ "title" ! A.id "blog-title" $ toHtml $ fromJust maybeTitle)
  when (isJust maybeSubtitle) (h3 ! A.id "blog-subtitle" $ toHtml $ fromJust maybeSubtitle)
  when (isJust maybeUser) (a ! href "/logout" ! class_ "blogbutton" ! rel "nofollow" $ "Logout")
  when (isJust maybeUser) (a ! href "/posts/new" ! class_ "blogbutton" ! rel "nofollow" $ "New Post")
  div ! A.id "content" $ bodyHtml
  
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
renderTagEditor (Just (BlogPost _ _ _ _ tags)) = textarea ! A.id "tags" ! class_ "wordlist" $ do toHtml $ List.intercalate ", " tags
  
renderPostEditor :: Maybe BlogPost -> Html
renderPostEditor maybeBlogPost = do
  renderTitleField maybeBlogPost
  renderMdEditor maybeBlogPost
  renderTagEditor maybeBlogPost
  
  a ! A.id "delete-button" ! class_ "blogbutton" ! rel "nofollow" $ "Delete"
  a ! A.id "preview-button" ! class_ "blogbutton" ! rel "nofollow" $ "Preview"
  a ! A.id "save-button" ! class_ "blogbutton" ! rel "nofollow" $ "Save"

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

upsertBlogPost :: PG.Connection -> Maybe Integer -> Maybe T.Text -> Maybe T.Text -> Maybe [String] -> Maybe [String] -> IO (Maybe BlogPost)
upsertBlogPost pg (Just identifier) Nothing      Nothing     Nothing     Nothing            = listToMaybe <$> query pg "SELECT * FROM blogposts WHERE identifier=? LIMIT 1" [identifier]
upsertBlogPost pg (Just identifier) (Just title) Nothing     Nothing     Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET title=? WHERE identifier=? RETURNING *" (title, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) Nothing     Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=? WHERE identifier=? RETURNING *" (title, body, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) (Just tags) Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=uniq_cat(tags,?)  WHERE identifier=? RETURNING *" (title, body, tags, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) (Just tags) (Just deletedTags) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(uniq_cat(tags, ?),?) WHERE identifier = ? RETURNING *" (title, body, tags, deletedTags, identifier)
upsertBlogPost pg (Just identifier) Nothing      (Just body) Nothing     Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET bodyText=? WHERE identifier=? RETURNING *" (body, identifier)
upsertBlogPost pg (Just identifier) Nothing      Nothing     (Just tags) Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET tags=uniq_cat(tags,?) WHERE identifier=? RETURNING *" (tags, identifier)
upsertBlogPost pg (Just identifier) (Just title) Nothing     (Just tags) Nothing            = listToMaybe <$> query pg "UPDATE blogposts SET title=?, tags=uniq_cat(tags,?) WHERE identifier=? RETURNING *" (title, tags, identifier)
upsertBlogPost pg (Just identifier) Nothing      Nothing     Nothing     (Just deletedTags) = listToMaybe <$> query pg "UPDATE blogposts SET tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (deletedTags, identifier)
upsertBlogPost pg (Just identifier) (Just title) Nothing     Nothing     (Just deletedTags) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (title, deletedTags, identifier)
upsertBlogPost pg (Just identifier) (Just title) (Just body) Nothing     (Just deletedTags) = listToMaybe <$> query pg "UPDATE blogposts SET title=?, bodyText=?, tags=array_diff(tags,?) WHERE identifier=? RETURNING *" (title, body, deletedTags, identifier)
upsertBlogPost pg Nothing           (Just title) (Just body) Nothing     _                  = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText) VALUES (?, ?) RETURNING *" (title, body)
upsertBlogPost pg Nothing           (Just title) (Just body) (Just tags) _                  = listToMaybe <$> query pg "INSERT INTO blogposts (title, bodyText, tags) VALUES (?, ?, ?) RETURNING *" (title, body, tags)
upsertBlogPost _  _                 _            _           _           _                  = return Nothing

getBlogPostsByTag :: PG.Connection -> T.Text -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPostsByTag pg tag Nothing     Nothing       = query pg "SELECT * FROM blogposts WHERE ?=any(tags) ORDER BY identifier DESC" [tag]
getBlogPostsByTag pg tag (Just from) Nothing       = query pg "SELECT * FROM blogposts WHERE ?=any(tags) AND identifier > ? LIMIT 10 ORDER BY identifier DESC" (tag, from)
getBlogPostsByTag pg tag Nothing     (Just untill) = query pg "SELECT * FROM blogposts WHERE ?=any(tags) AND identifier < ? LIMIT 10 ORDER BY identifier DESC" (tag, untill)
getBlogPostsByTag pg tag (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE ?=any(tags) AND identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier DESC" (tag, from, untill)

getBlogPosts :: PG.Connection -> Maybe Integer -> Maybe Integer -> IO [BlogPost]
getBlogPosts pg Nothing     Nothing       = query_ pg "SELECT * FROM blogposts ORDER BY identifier DESC"
getBlogPosts pg (Just from) Nothing       = query pg "SELECT * FROM blogposts WHERE identifier > ? LIMIT 10 ORDER BY identifier DESC" [from]
getBlogPosts pg Nothing     (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier < ? LIMIT 10 ORDER BY identifier DESC" [untill]
getBlogPosts pg (Just from) (Just untill) = query pg "SELECT * FROM blogposts WHERE identifier > ? AND identifier < ? LIMIT 10 ORDER BY identifier DESC" (from, untill)
