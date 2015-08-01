{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Blog.Types where
  
import           Blog.Database.PGExtensions()

import           GHC.Generics
import           Control.Monad
import           Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import           Data.Time.Clock
import           Data.List
import qualified Data.Vector as V
import           Data.Time.Format

import           Cheapskate
import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style, param, map, option)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString, fromByteString)

import           Data.Attoparsec.ByteString.Char8 as A

import           Prelude as P hiding (div)
import           Data.Typeable

--------------------------------------------------------------------------------
--- | Typeclasses

-- renders records into HTML using blaze
class Composable a where
  render :: a -> Maybe User -> Html
  renderBasic :: a -> Html
  renderBasic comp = render comp Nothing
  
--------------------------------------------------------------------------------
--- | Comment data type

-- Used to structure comments into a
-- 'family tree', similar to how they
-- would appear on the web.
data Node = Node {
  comment :: Comment,
  children :: [Node],
  parent :: Maybe Node
} deriving (Eq, Show)

instance ToJSON Node where
 toJSON node@(Node (Comment cid_ _ postId_ email_ displayName_ tstamp_ commentBody_) children_ _) =
    Aeson.object [
                  "id" .= cid_,
                  "post_id" .= postId_,
                  "email" .= email_,
                  "display_name" .= displayName_,
                  "timestamp" .= tstamp_,
                  "body" .= commentBody_,
                  "parentage" .= (parentage node),
                  "children" .= map toJSON children_
                  ]
            
instance ToJSON [Node] where
  toJSON nodes = Aeson.Array $ V.fromList $ map toJSON nodes

data Comment = Comment {
  cid :: !Integer,
  parentId :: Maybe Integer,
  postId :: !Integer,
  email :: T.Text,
  commentDisplayName :: T.Text,
  tstamp :: UTCTime,
  commentBody :: T.Text
} deriving (Eq, Show, Generic)

instance ToJSON Comment
instance FromJSON Comment

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field <*> field <*> field

parentage :: Node -> Integer
parentage (Node _ _ Nothing) = 0
parentage (Node _ _ (Just prnt)) = 1 + parentage prnt

--          parent  child
isParent :: Node -> Node -> Bool
isParent prnt child
  | (((==) (cid $ comment prnt)) <$> (parentId $ comment child)) == (Just True) = True
  | elem child (children prnt) = True
  | otherwise = False

--              anc     desc
isDescendent :: Node -> Node -> Bool
isDescendent _       (Node (Comment _ Nothing _ _ _ _ _) _ _) = False
isDescendent nodeOne nodeTwo -- nodeTwo is nodeOne's child OR nodeTwo is a child of a child of a child of nodeOne
  | isParent nodeOne nodeTwo = True
  | otherwise = any ((flip isDescendent) nodeTwo) $ children nodeOne

isRoot :: Node -> Bool
isRoot = isNothing . parentId . comment

-- Adds child to the first node's children, appling func to each of the children
appendChild :: Node -> Node -> ([Node] -> [Node]) -> Node
appendChild (Node c children_ parent_) child func = Node c (func ((Node (comment child) (children child) (Just (Node c children_ parent_))):children_)) parent_

nestComments :: [Node] -> [Node]
nestComments [] = []
nestComments [x] = [x]
nestComments (c:xs) = case c of
                        (Node (Comment _ (Just _) _ _ _ _ _) _ _) -> do
                          case (filter ((flip isDescendent) c) xs) of
                            [] -> c:nestComments xs
                            prnts -> nestComments $ (map (\prnt -> appendChild prnt c nestComments) prnts) ++ (xs \\ prnts)
                        _ -> if all isRoot (c:xs)
                              then (c:xs)
                              else nestComments $ xs ++ [c]

--------------------------------------------------------------------------------
--- | User data type
  
data User = User {
  userUID :: !Integer,
  userUsername :: T.Text,
  userDisplayName :: T.Text,
  userPasswordHash :: Maybe T.Text
} deriving (Show, Generic, Typeable)

instance ToJSON User where
  toJSON User{..} = Aeson.object [
    "uid" .= userUID,
    "username" .= userUsername,
    "display_name" .= userDisplayName,
    "password_hash" .= userPasswordHash]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .:  "uid"
    <*> o .:  "username"
    <*> o .:  "display_name"
    <*> o .:? "password_hash"
  parseJSON _ = mzero

instance Eq User where
  (User a_ _ _ _) == (User b_ _ _ _) = a_ == b_

instance FromField User where
  fromField f (Just fdata) = do
    tinfo <- typeInfo f
    case tinfo of
      (Composite _ _ _ "users" _ _) -> do
        case parseUserRow fdata of
          Just user -> return user
          Nothing -> returnError ConversionFailed f "Failed to read users field."
      _ -> returnError ConversionFailed f "Failed to read users field."
  fromField f Nothing = do
    tinfo <- typeInfo f
    case tinfo of
      (Composite _ _ _ "users" _ _) -> returnError Incompatible f "Field is a user, but is empty."
      _ -> returnError ConversionFailed f "Failed to read users field."

--------------------------------------------------------------------------------
instance ToField User where
  toField (User uid uname dname phash) =
    Many [Plain . fromByteString $ "ROW(",
          toField uid, comma, toField uname, comma, toField dname, comma,
          fromMaybe nullv (toField <$> phash),
          Plain $ fromByteString ")"]
    where
      comma = Plain $ fromByteString ","
      nullv = (Plain $ fromByteString "NULL")
  
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
  
instance ToRow User where
  toRow (User uid username displayName passwordHash) =
    [toField uid,
    toField username,
    toField displayName,
    toField passwordHash]
    
parseUserRow :: B.ByteString -> Maybe User
parseUserRow = maybeResult . parse userRowParser

userRowParser :: Parser User
userRowParser = User
  <$> (skipUntakeable >> decimal)
  <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
  <*> (skipUntakeable >> T.decodeUtf8 <$> takeTakeable)
  <*> (skipUntakeable >> option Nothing (Just . T.decodeUtf8 <$> takeTakeable))
  where
    takeable ',' = False
    takeable '"' = False
    takeable ')' = False
    takeable '(' = False
    takeable _   = True
    skipUntakeable = skipWhile (not . takeable)
    takeTakeable = A.takeWhile takeable
    
-------------------------------------------------------------------------------
--- | BlogPost data type

data BlogPost = BlogPost {
  identifier :: !Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: UTCTime,
  tags :: [String],
  author_id :: !Integer,
  isDraft :: !Bool,
  author :: User
} deriving (Show)

instance Eq BlogPost where
  (==) a_ b_ = (identifier a_) == (identifier b_)

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance ToRow BlogPost where
  toRow (BlogPost identifier_ title_ body_ timestamp_ tags_ author_id_ isDraft_ _) =
    [toField identifier_,
    toField title_,
    toField body_,
    toField timestamp_,
    Many [Plain $ fromByteString "uniq_cat(tags,",
          toField tags_,
          Plain $ fromByteString ")"],
    toField author_id_,
    toField isDraft_]
  
instance ToJSON BlogPost where
  toJSON (BlogPost identifier_ title_ body_ timestamp_ tags_ _ isDraft_ author_) =
    Aeson.object [
      "id" .= identifier_,
      "title" .= title_,
      "body" .= body_,
      "timestamp" .= (timestampToValue timestamp_),
      "tags" .= (tagsToValue tags_),
      "draft" .= isDraft_,
      "author" .= toJSON author_
    ]
    where
      timestampToValue :: UTCTime -> Value
      timestampToValue = Aeson.String . T.decodeUtf8 . BL.toStrict . toLazyByteString . utcTimeToBuilder
      tagsToValue :: [String] -> Value
      tagsToValue = Aeson.Array . Vector.fromList . map (Aeson.String . T.pack)
    
--------------------------------------------------------------------------------
instance Composable BlogPost where
  -- Unauthenticated
  render (BlogPost id_ title_ body_ ts_ tags_ _ _ author_) Nothing = do
    a ! href (stringValue $ "/posts/" ++ show id_) $ do
      h1 ! class_ "post-title" ! A.id (stringValue $ show id_) $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ formatSubtitle ts_ $ userDisplayName author_
    mapM_ taglink tags_
    div ! class_ "post-content" $ toHtml $ markdown def body_
  -- Authenticated, draft
  render (BlogPost id_ title_ body_ ts_ tags_ _ True author_) (Just user_) = do
    a ! href (stringValue $ "/posts/" ++ show id_) $ do
      h1 ! class_ "post-title post-draft" ! A.id (stringValue $ show id_) $ toHtml title_
    h4 ! class_ "post-subtitle post-draft" $ toHtml $ formatSubtitle ts_ $ userDisplayName author_
    mapM_ taglink tags_
    when (author_ == user_) $ do
      a ! class_ "post-edit-button post-draft" ! href (stringValue $ "/posts/" ++ show id_ ++ "/edit") ! rel "nofollow" $ "edit"
    div ! class_ "post-content post-draft" $ toHtml $ markdown def body_
  -- Authenticated, published
  render (BlogPost id_ title_ body_ ts_ tags_ _ False author_) (Just user_) = do
    a ! href (stringValue $ "/posts/" ++ show id_) $ do
      h1 ! class_ "post-title" ! A.id (stringValue $ show id_) $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ formatSubtitle ts_ $ userDisplayName author_
    mapM_ taglink tags_
    when (author_ == user_) $ do
      a ! class_ "post-edit-button" ! href (stringValue $ "/posts/" ++ show id_ ++ "/edit") ! rel "nofollow" $ "edit"
    div ! class_ "post-content" $ toHtml $ markdown def body_
    
taglink :: String -> Html
taglink t = a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ do
  h4 ! class_ "post-subtitle" $ toHtml $ t

formatDate :: FormatTime t => t -> String
formatDate = formatTime defaultTimeLocale "%-m • %-e • %-y | %l:%M %p %Z" 

formatSubtitle :: FormatTime t => t -> T.Text -> T.Text
formatSubtitle t authorName = mconcat [T.pack $ formatDate t, " | ", authorName]
    
instance Composable [BlogPost] where
  render [] _ = return ()
  render [x] user = render x user
  render (x:xs) user = do
    render x user
    unless (null xs) $ do
      hr ! class_ "separator"
      render xs user
