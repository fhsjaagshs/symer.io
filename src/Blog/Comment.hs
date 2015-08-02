{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Blog.Comment
(
  Comment(..),
  Node(..),
  nestComments
)
where
  
import Data.Maybe

import           Data.List
import qualified Data.Vector as V
import           Data.Time.Clock

import           GHC.Generics

import qualified Data.Text as T
  
import           Data.Aeson as Aeson

import           Database.PostgreSQL.Simple.FromRow as PG.FromRow

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