{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}

module Types where
  
import           Helpers
import           PGExtensions()

import           GHC.Generics
import           Control.Applicative
import           Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import           Data.Time.Clock

import           Cheapskate
import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple.FromField as PG.FromField
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString, fromByteString)

import           Prelude as P hiding (div)
import           Data.Typeable

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
  identifier :: Integer,
  title :: T.Text,
  body :: T.Text,
  timestamp :: UTCTime,
  tags :: [String],
  author_id :: Integer,
  isDraft :: Bool,
  author :: User
} deriving (Show)

instance Eq BlogPost where
  (==) one two = (identifier one) == (identifier two)

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
  
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
      h1 ! class_ "post-title" $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ T.append (T.pack $ formatDate timestamp_) (T.append " | " $ Types.displayName author_)
    toHtml $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ h4 ! class_ "post-subtitle" $ toHtml $ t) tags_
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body_
  -- Authenticated, draft
  render (BlogPost identifier_ title_ body_ timestamp_ tags_ _ True author_) (Just user_) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title post-draft" $ toHtml title_
    h4 ! class_ "post-subtitle post-draft" $ toHtml $ T.append (T.pack $ formatDate timestamp_) (T.append " | " $ Types.displayName author_)
    toHtml $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ toHtml $ h4 ! class_ "post-subtitle" $ toHtml $ t) tags_
    when (author_ == user_) $  a ! class_ "post-edit-button post-draft" ! href (stringValue $ ("/posts/" ++ (show identifier_) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content post-draft" ! style "text-align: left;" $ toHtml $ markdown def body_
  -- Authenticated, published
  render (BlogPost identifier_ title_ body_ timestamp_ tags_ _ False author_) (Just user_) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title" $ toHtml title_
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
