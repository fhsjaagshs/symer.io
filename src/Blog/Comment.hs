{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Comment
(
  Comment(..),
  nestComments
)
where
  
import Data.Maybe
import Data.Time.Clock
import Data.Text (Text)
import Data.Aeson as Aeson

import Cheapskate
import Cheapskate.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Database.PostgreSQL.Simple.FromRow

data Comment = Comment {
  commentID :: !Integer,
  commentParentID :: Maybe Integer,
  commentPostID :: !Integer,
  commentEmail :: Text,
  commentDisplayName :: Text,
  commentTimestamp :: UTCTime,
  commentBody :: Text,
  commentChildren :: [Comment],
  commentParent :: Maybe Comment
} deriving (Show)

instance Eq Comment where
  a == b = commentID a == commentID b

instance ToJSON Comment where
 toJSON (Comment cid _ postId email dname ts body children _) =
    Aeson.object ["id" .= cid,
                  "post_id" .= postId,
                  "email" .= email,
                  "display_name" .= dname,
                  "timestamp" .= ts,
                  "body" .= (renderHtml $ renderDoc $ markdown def body),
                  "children" .= map toJSON children]

instance FromRow Comment where
  fromRow = Comment
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> return []
    <*> return Nothing

-- TODO: add strictness ($!)
-- TODO: fix children ordering (it's reverse)
nestComments :: [Comment] -> [Comment]
nestComments cmnts = map (f antiroots) roots
  where
    roots = filter (isNothing . commentParentID) cmnts
    antiroots = filter (isJust . commentParentID) cmnts
    f [] a = a
    f (x:xs) a = f xs (addHierarchical a x)
    modifyChildren n g = n { commentChildren = (g $ commentChildren n) }
    isParent p c = maybe False ((==) (commentID p)) (commentParentID c)
    addHierarchical a c
      | isParent a c = modifyChildren a ((:) c)
      | otherwise = modifyChildren a (map (\a' -> addHierarchical a' c))
