{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Post
(
  Post(..)
)
where
  
import Blog.Composable
import Blog.User

import Control.Monad

import           Cheapskate
import           Data.Aeson as Aeson

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import           Data.List
import           Data.Time.Format
import           Data.Time.Clock

import           Blog.Database.PGExtensions()
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style, param, map, option)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString, fromByteString)
import           Prelude as P hiding (div)

data Post = Post {
  postID :: !Integer,
  postTitle :: T.Text,
  postBody :: T.Text,
  postTimestamp :: UTCTime,
  postTags :: [String],
  postAuthorID :: !Integer,
  postIsDraft :: !Bool,
  postAuthor :: User
} deriving (Show)

instance Eq Post where
  (==) a_ b_ = (postID a_) == (postID b_)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance ToRow Post where
  toRow (Post identifier_ title_ body_ timestamp_ tags_ author_id_ isDraft_ _) =
    [toField identifier_,
    toField title_,
    toField body_,
    toField timestamp_,
    Many [Plain $ fromByteString "uniq_cat(tags,",
          toField tags_,
          Plain $ fromByteString ")"],
    toField author_id_,
    toField isDraft_]
  
instance ToJSON Post where
  toJSON (Post identifier_ title_ body_ timestamp_ tags_ _ isDraft_ author_) =
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
instance Composable Post where
  -- Unauthenticated
  render (Post id_ title_ body_ ts_ tags_ _ _ author_) Nothing = do
    a ! href (stringValue $ "/posts/" ++ show id_) $ do
      h1 ! class_ "post-title" ! A.id (stringValue $ show id_) $ toHtml title_
    h4 ! class_ "post-subtitle" $ toHtml $ formatSubtitle ts_ $ userDisplayName author_
    mapM_ taglink tags_
    div ! class_ "post-content" $ toHtml $ markdown def body_
  -- Authenticated, draft
  render (Post id_ title_ body_ ts_ tags_ _ True author_) (Just user_) = do
    a ! href (stringValue $ "/posts/" ++ show id_) $ do
      h1 ! class_ "post-title post-draft" ! A.id (stringValue $ show id_) $ toHtml title_
    h4 ! class_ "post-subtitle post-draft" $ toHtml $ formatSubtitle ts_ $ userDisplayName author_
    mapM_ taglink tags_
    when (author_ == user_) $ do
      a ! class_ "post-edit-button post-draft" ! href (stringValue $ "/posts/" ++ show id_ ++ "/edit") ! rel "nofollow" $ "edit"
    div ! class_ "post-content post-draft" $ toHtml $ markdown def body_
  -- Authenticated, published
  render (Post id_ title_ body_ ts_ tags_ _ False author_) (Just user_) = do
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
    
instance Composable [Post] where
  render [] _ = return ()
  render [x] user = render x user
  render (x:xs) user = do
    render x user
    unless (null xs) $ do
      hr ! class_ "separator"
      render xs user
