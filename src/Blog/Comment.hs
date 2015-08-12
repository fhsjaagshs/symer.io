{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Blog.Comment
(
  Comment(..),
  nestComments
)
where
  
import           Data.Maybe
import           Data.List
import qualified Data.Vector as V
import           Data.Time.Clock
import qualified Data.Text as T
import           Data.Aeson as Aeson

import           Database.PostgreSQL.Simple.FromRow as PG.FromRow

data Comment = Comment {
  commentID :: !Integer,
  commentParentID :: Maybe Integer,
  commentPostID :: !Integer,
  commentEmail :: T.Text,
  commentDisplayName :: T.Text,
  commentTimestamp :: UTCTime,
  commentBody :: T.Text,
  commentChildren :: [Comment],
  commentParent :: Maybe Comment
} deriving (Show)

instance Eq Comment where
  a == b = (commentID a) == (commentID b)

instance ToJSON Comment where
 toJSON (Comment cid _ postId email dname ts body children _) =
    Aeson.object ["id" .= cid,
                  "post_id" .= postId,
                  "email" .= email,
                  "display_name" .= dname,
                  "timestamp" .= ts,
                  "body" .= body,
                  "children" .= map toJSON children]
                  
instance ToJSON [Comment] where
  toJSON = Aeson.Array . V.fromList . map toJSON
                  
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
isParent prnt child
  | fmap ((==) (commentID prnt)) (commentParentID child) == (Just True) = True
  | elem child (commentChildren prnt) = True
  | otherwise = False
  
isRoot :: Comment -> Bool
isRoot = isNothing . commentParentID

hasParentID :: Comment -> Bool
hasParentID (Comment _ (Just _) _ _ _ _ _ _ _) = True
hasParentID _ = False

isDescendent :: Comment -> Comment -> Bool
isDescendent _ (Comment _ Nothing _ _ _ _ _ _ _) = False
isDescendent anc desc
  | isParent anc desc = True
  | otherwise = any f $ commentChildren anc
    where
      f = flip isDescendent $ desc
      
appendChild :: Comment -> Comment -> Comment
appendChild p c = p { commentChildren = (c:commentChildren p) }

-- TODO: properly nest children of a node
nestComments :: [Comment] -> [Comment]
nestComments [] = []
nestComments [x] = [x]
nestComments (c:xs)
  | hasParentID c = case filter (flip isDescendent $ c) xs of
                      [] -> c:nestComments xs
                      prnts -> nestComments $ (map (flip appendChild $ c) prnts) ++ (xs \\ prnts)
  | otherwise = if all isRoot (c:xs) then (c:xs) else nestComments $ xs ++ [c]

-- -- Adds child to the first node's children, appling func to each of the children
-- appendChild :: Node -> Node -> ([Node] -> [Node]) -> Node
-- appendChild (Node c children_ parent_) child func = Node c (func ((Node (comment child) (children child) (Just (Node c children_ parent_))):children_)) parent_
--
-- nestComments :: [Node] -> [Node]
-- nestComments [] = []
-- nestComments [x] = [x]
-- nestComments (c:xs) = case c of
--                         (Node (Comment _ (Just _) _ _ _ _ _) _ _) -> do
--                           case (filter ((flip isDescendent) c) xs) of
--                             [] -> c:nestComments xs
--                             prnts -> nestComments $ (map (\prnt -> appendChild prnt c nestComments) prnts) ++ (xs \\ prnts)
--                         _ -> if all isRoot (c:xs)
--                               then (c:xs)
--                               else nestComments $ xs ++ [c]