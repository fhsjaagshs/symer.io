{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Comment
(
  Comment(..),
  nestComments
)
where
  
import Data.Maybe
import Data.List
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
    
isParent :: Comment -> Comment -> Bool
isParent prnt child = maybe False ((==) (commentID prnt)) (commentParentID child)
     
appendChild :: Comment -> Comment -> Comment
appendChild p c = p { commentChildren = (c:commentChildren p) }

addChildIfChild :: Comment -> Comment -> Comment
addChildIfChild p c = if isParent p c then appendChild p c else p

-- tail recursive nestComments
-- f ancestors currentParents xs = f (roots xs ++ ancestors) (roots xs) (xs \\ roots xs)
-- 1. find all "root" parents in xs
-- 2. match them to currentParents
-- 3. add comments from step 1 to ancestors

-- TODO: TAIL RECURSIVE
nestComments :: [Comment] -> [Comment]
nestComments [] = []
nestComments [x] = [x]
nestComments comments
  | length leaves == length comments = comments
  | otherwise = nestComments $ processed ++ singletonLeaves
  where
    isLeaf cmnt = isNothing $ find (isParent cmnt) (delete cmnt comments) -- O(n)
    leaves = filter isLeaf comments -- O(n^2)
    singletonLeaves = filter (isNothing . commentParentID) leaves -- O(l)
    processed = map (\p -> foldl addChildIfChild p leaves) (comments \\ leaves) -- O((n-l)*l)
