{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}

module Types where
  
import           Helpers
import           PGExtensions()

import           GHC.Generics
import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import           Data.Time.Clock
import           Data.List
import qualified Data.Vector as V

import           Cheapskate
import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString, fromByteString)

import           Prelude as P hiding (div)
import           Data.Typeable

-------------------------------------------------------------------------------
--- | Typeclasses

-- renders records into HTML using blaze
class Composable a where
  render :: a -> Maybe User -> Html
  renderBasic :: a -> Html
  renderBasic comp = render comp Nothing
  
-------------------------------------------------------------------------------
--- | Comment data type

-- Used to structure comments into a
-- 'family tree', similar to how they
-- would appear on the web.
data Node = Node {
  data_ :: Comment,
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
  | (((==) (cid $ Types.data_ prnt)) <$> (parentId $ Types.data_ child)) == (Just True) = True
  | elem child (children prnt) = True
  | otherwise = False

--              anc     desc
isDescendent :: Node -> Node -> Bool
isDescendent _       (Node (Comment _ Nothing _ _ _ _ _) _ _) = False
isDescendent nodeOne nodeTwo -- nodeTwo is nodeOne's child OR nodeTwo is a child of a child of a child of nodeOne
  | isParent nodeOne nodeTwo = True
  | otherwise = any ((flip isDescendent) nodeTwo) $ children nodeOne

isRoot :: Node -> Bool
isRoot = isNothing . parentId . Types.data_

-- Adds child to the first node's children, appling func to each of the children
appendChild :: Node -> Node -> ([Node] -> [Node]) -> Node
appendChild (Node c children_ parent_) child func = Node c (func ((Node (Types.data_ child) (children child) (Just (Node c children_ parent_))):children_)) parent_

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

-------------------------------------------------------------------------------
--- | User data type
  
data User = User {
  uid :: !Integer,
  username :: T.Text,
  displayName :: T.Text,
  passwordHash :: Maybe T.Text
} deriving (Show, Generic, Typeable)

instance ToJSON User
instance FromJSON User

instance Eq User where
  (==) (User uidOne _ _ _) (User uidTwo _ _ _) = ((==) uidOne uidTwo)

instance FromField User where
  fromField f (Just fdata) = do
    tinfo <- (typeInfo f)
    case tinfo of
      (Composite _ _ _ "users" _ _) -> return $ parseUserRow $ parseRow $ T.decodeUtf8 fdata
      _ -> returnError ConversionFailed f "Failed to read users field."
  fromField f Nothing = do
    tinfo <- (typeInfo f)
    case tinfo of
      (Composite _ _ _ "users" _ _) -> returnError Incompatible f "Field is a user, but is empty."
      _ -> returnError ConversionFailed f "Failed to read users field."

instance ToField User where
  toField u  = Many [Plain $ fromByteString "ROW(",
                     Plain $ fromByteString $ B.pack $ show $ Types.uid u,
                     Plain $ fromByteString ",",
                     Escape $ T.encodeUtf8 $ Types.username u,
                     Plain $ fromByteString ",",
                     Escape $ T.encodeUtf8 $ Types.displayName u,
                     Plain $ fromByteString ",",
                     (maybe (Plain $ fromByteString "NULL") P.id (Escape . T.encodeUtf8 <$> Types.passwordHash u)),
                     Plain $ fromByteString ")"
                     ]
  
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
  
instance ToRow User where
  toRow (User uid_ username_ displayName_ passwordHash_) = [toField uid_, toField username_, toField displayName_, toField passwordHash_]

parseUserRow :: [T.Text] -> User
parseUserRow (uid_:username_:dispName_:pwh_:[]) = User (read $ T.unpack uid_) username_ dispName_ (Just pwh_)
parseUserRow (uid_:username_:dispName_:[]) = User (read $ T.unpack uid_) username_ dispName_ Nothing
parseUserRow _ = error "Invalid user row."

parseRow :: T.Text -> [T.Text]
parseRow "()" = []
parseRow ""   = []
parseRow str
  | (T.head str) == '(' && (T.last str) == ')'  = parseRow $ T.init $ T.tail str
  | (T.head str) == '\"'                        = (T.takeWhile (\c -> c /= '\"') (T.tail str)):(parseRow $ T.tail $ T.dropWhile (\c -> c /= '\"') (T.tail str))
  | (T.head str) == ','                         = parseRow $ T.tail str
  | otherwise                                   = (T.takeWhile (\c -> c /= ',') str):(parseRow $ T.dropWhile (\c -> c /= ',') str)

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
  (==) one two = (identifier one) == (identifier two)

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance ToRow BlogPost where
  toRow (BlogPost identifier_ title_ body_ timestamp_ tags_ author_id_ isDraft_ _) =
    [toField identifier_, toField title_, toField body_, toField timestamp_, Many [Plain $ fromByteString "uniq_cat(tags,", toField tags_, Plain $ fromByteString ")"], toField author_id_, toField isDraft_]
  
instance ToJSON BlogPost where
  toJSON (BlogPost identifier_ title_ body_ timestamp_ tags_ _ isDraft_ author_) =
    Aeson.object [
      "id" .= identifier_,
      "title" .= title_,
      "body" .= body_,
      "timestamp" .= ((Aeson.String $ T.decodeUtf8 $ BL.toStrict $ toLazyByteString $ utcTimeToBuilder timestamp_) :: Value),
      "tags" .= ((Aeson.Array $ Vector.fromList $ P.map (Aeson.String . T.pack) tags_) :: Value),
      "draft" .= isDraft_,
      "author" .= toJSON author_
    ]
    
instance Composable BlogPost where
  -- Unauthenticated
  render (BlogPost identifier_ title_ body_ timestamp_ tags_ _ _ author_) Nothing = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title" ! A.id (stringValue $ show identifier_) $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ T.append (T.pack $ formatDate timestamp_) (T.append " | " $ Types.displayName author_)
    toHtml $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ h4 ! class_ "post-subtitle" $ toHtml $ t) tags_
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body_
  -- Authenticated, draft
  render (BlogPost identifier_ title_ body_ timestamp_ tags_ _ True author_) (Just user_) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title post-draft" ! A.id (stringValue $ show identifier_) $ toHtml title_
    h4 ! class_ "post-subtitle post-draft" $ toHtml $ T.append (T.pack $ formatDate timestamp_) (T.append " | " $ Types.displayName author_)
    toHtml $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ toHtml $ h4 ! class_ "post-subtitle" $ toHtml $ t) tags_
    when (author_ == user_) $  a ! class_ "post-edit-button post-draft" ! href (stringValue $ ("/posts/" ++ (show identifier_) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content post-draft" ! style "text-align: left;" $ toHtml $ markdown def body_
  -- Authenticated, published
  render (BlogPost identifier_ title_ body_ timestamp_ tags_ _ False author_) (Just user_) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title" ! A.id (stringValue $ show identifier_) $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ T.append (T.pack $ formatDate timestamp_) (T.append " | " $ Types.displayName author_)
    toHtml $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ toHtml $ h4 ! class_ "post-subtitle" $ toHtml $ t) tags_
    when (author_ == user_) $  a ! class_ "post-edit-button" ! href (stringValue $ ("/posts/" ++ (show identifier_) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body_
    
instance Composable [BlogPost] where
  render [] _ = return ()
  render [x] user = render x user
  render (x:xs) user = do
    render x user
    unless (null xs) $ do
      hr ! class_ "separator"
      render xs user
