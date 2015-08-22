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
import           Text.Blaze.Truncate
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
instance Composable Post where
  render = renderPost False
  
instance Composable [Post] where
  render [] _ = return ()
  render [x] user = renderPost True x user
  render (x:xs) user = do
    renderPost True x user
    hr ! class_ "separator"
    render xs user
    
renderPost :: Bool -> Post -> Maybe User -> Html
renderPost truncateBody (Post pid title body ts tags _ author) mAuthUser = do
  a ! href (stringValue $ "/posts/" ++ show pid) $ do
    h1 ! class_ "post-title" ! A.id (stringValue $ show pid) $ toHtml title
  h4 ! class_ "post-subtitle" $ toHtml $ formatSubtitle ts $ userDisplayName author
  mapM_ taglink tags
  when (isJust mAuthUser) $ when (author == (fromJust mAuthUser)) $ do
    a ! class_ "post-edit-button" ! href editURL ! rel "nofollow" $ "edit"
  div ! class_ "post-content" $ renderBody truncateBody
  where
    editURL = stringValue $ mconcat ["/posts/", show pid, "/edit"]
    renderBody True = truncated 500 $ toHtml $ markdown def body
    renderBody False = toHtml $ markdown def body
    formatSubtitle t authorName = mconcat [formatDate t, " | ", authorName]
    formatDate = T.pack . formatTime defaultTimeLocale "%-m • %-e • %-y | %l:%M %p %Z"
    taglink t = a ! class_ "taglink" ! href (textValue $ mconcat ["/posts/by/tag/", t]) $ do
      h4 ! class_ "post-subtitle" $ toHtml t
        
truncated :: Int -> Html -> Html
truncated len h = case truncateHtml len h of
  Just trunc -> trunc
  Nothing -> h