{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}

module Types where
  
import           Helpers
import           PGExtensions()
  
import           GHC.Generics
import           Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import qualified Data.List as List
import           Data.Time.Clock
import           Data.Monoid

import           Cheapskate
import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString)

import           Prelude as P hiding (head, id, div)

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
} deriving (Show)

instance Eq BlogPost where
  (==) (BlogPost idOne _ _ _ _) (BlogPost idTwo _ _ _ _) = ((==) idOne idTwo)

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field
  
instance ToJSON BlogPost where
  toJSON (BlogPost identifier_ title_ body_ timestamp_ tags_) =
    Aeson.object [
      "id" .= identifier_,
      "title" .= title_,
      "body" .= body_,
      "timestamp" .= ((Aeson.String $ T.decodeUtf8 $ BL.toStrict $ toLazyByteString $ utcTimeToBuilder timestamp_) :: Value),
      "tags" .= ((Aeson.Array $ Vector.fromList $ P.map (Aeson.String . T.pack) tags_) :: Value)
    ]
    
instance Composable BlogPost where
  render (BlogPost identifier_ title_ body_ timestamp_ tags_) Nothing = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title" $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ formatDate timestamp_
    h4 ! class_ "post-subtitle post-tags" $ toHtml $ List.intersperse (H.span $ ", ") $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ toHtml t) tags_
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body_
  render (BlogPost identifier_ title_ body_ timestamp_ tags_) (Just _) = do
    a ! href (stringValue $ (++) "/posts/" $ show identifier_) $ do
      h1 ! class_ "post-title" $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ formatDate timestamp_
    h4 ! class_ "post-subtitle post-tags" $ toHtml $ List.intersperse (H.span $ ", ") $ map (\t -> a ! class_ "taglink" ! href (stringValue $ "/posts/by/tag/" ++ t) $ toHtml t) tags_
    a ! class_ "post-edit-button" ! href (stringValue $ ("/posts/" ++ (show identifier_) ++ "/edit")) ! rel "nofollow" $ "edit"
    div ! class_ "post-content" ! style "text-align: left;" $ toHtml $ markdown def body_
    
instance Composable [BlogPost] where
  render [] _ = return ()
  render [x] user = render x user
  render (x:xs) user = do
    render x user
    hr ! class_ "separator"
    render xs user