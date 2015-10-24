{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Post
(
  Post(..),
  postDescription,
  renderPost
)
where

import Blog.User
import Blog.Util.Markdown

import Control.Monad

import           Cheapskate
import           Cheapskate.Html
import           Data.Aeson as Aeson

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import           Data.List
import           Data.Time.Format
import           Data.Time.Clock
import           Data.Maybe

import           Blog.Database.PGExtensions()
import           Database.PostgreSQL.Simple.ToField as PG.ToField
import           Database.PostgreSQL.Simple.FromRow as PG.FromRow
import           Database.PostgreSQL.Simple.ToRow as PG.ToRow
import           Database.PostgreSQL.Simple.Time as PG.Time

import           Text.Blaze.Html5 as H hiding (style,param,map,option,body,title)
import           Text.Blaze.Html5.Attributes as A hiding (title)
import           Blaze.ByteString.Builder (toByteString,fromByteString)
import           Prelude as P hiding (div)

data Post = Post {
  postID :: !Integer,
  postTitle :: Text,
  postBody :: Text,
  postTimestamp :: UTCTime,
  postTags :: [Text],
  postIsDraft :: !Bool,
  postAuthor :: User
} deriving (Show)

instance Eq Post where
  (==) a_ b_ = (postID a_) == (postID b_)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance ToRow Post where
  toRow (Post pid title body ts tags isDraft (User authorId _ _ _)) =
    [toField pid,
    toField title,
    toField body,
    toField ts,
    Many [Plain $ fromByteString "uniq_cat(tags,",
          toField tags,
          Plain $ fromByteString ")"],
    toField authorId,
    toField isDraft]
  
instance ToJSON Post where
  toJSON (Post pid title body ts tags isDraft author) =
    Aeson.object [
      "id" .= pid,
      "title" .= title,
      "body" .= body,
      "timestamp" .= (timestampToValue ts),
      "tags" .= (tagsToValue tags),
      "draft" .= isDraft,
      "author" .= toJSON author
    ]
    where
      timestampToValue = Aeson.String . T.decodeUtf8 . toByteString . utcTimeToBuilder
      tagsToValue = Aeson.Array . Vector.fromList . map Aeson.String
    
--------------------------------------------------------------------------------

postDescription :: Post -> Text
postDescription = T.take 150 . stripMarkdown . markdown def . postBody

renderPost :: Bool -> Maybe User -> Post -> Html
renderPost truncateBody user (Post pid title body ts tags _ author) = do
  a ! href (stringValue $ "/posts/" ++ show pid) $ do
    h1 ! class_ "post-title" ! A.id (stringValue $ show pid) $ toHtml title
  h4 ! class_ "post-subtitle" $ toHtml $ formatSubtitle ts $ userDisplayName author
  mapM_ taglink tags
  when (isJust user) $ when (author == (fromJust user)) $ do
    a ! class_ "post-edit-button" ! href editURL ! rel "nofollow" $ "edit"
  div ! class_ "post-content" $ renderBody truncateBody
  where
    editURL = stringValue $ mconcat ["/posts/", show pid, "/edit"]
    renderBody True = do
      renderDoc . truncateMarkdown 500 . markdown def $ body
      a ! class_ "read-more" ! href (stringValue $ "/posts/" ++ show pid) $ "read more..."
    renderBody False = toHtml $ markdown def body
    formatSubtitle t authorName = mconcat [formatDate t, " | ", authorName]
    formatDate = T.pack . formatTime defaultTimeLocale "%-m • %-e • %-y | %l:%M %p %Z"
    taglink t = a ! class_ "taglink" ! href (textValue $ T.append "/posts/by/tag/" t) $ do
      h4 ! class_ "post-subtitle" $ toHtml t