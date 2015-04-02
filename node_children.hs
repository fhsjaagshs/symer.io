{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.IO.Class

data Comment = Comment {
  cid :: Integer,
  parentId :: Maybe Integer
}

instance Show Comment where
  show (Comment cid parentId) = "Comment " ++ (show cid) ++ " " ++ (show parentId)
  
instance Eq Comment where
  (==) (Comment c pid) (Comment cp pidp) = (c == cp) && (pid == pidp)

data Node = Node {
  data_ :: Comment,
  children :: [Node]
}

instance Eq Node where
  (==) (Node d c) (Node dp cp) = (d == dp) && (c == cp)

instance Show Node where
  show (Node d c) = "Node (" ++ (show d) ++ ") " ++ (show c)

--          parent  child
isParent :: Node -> Node -> Bool
isParent parent child
  | elem child (children parent) = True
  | (((==) (cid $ data_ parent)) <$> (parentId $ data_ child)) == (Just True) = True
  | otherwise = False
  
--              anc     desc
isDescendent :: Node -> Node -> Bool
isDescendent _       (Node (Comment _ Nothing) _) = False
isDescendent nodeOne nodeTwo -- nodeTwo is nodeOne's child OR nodeTwo is a child of a child of a child of nodeOne
  | isParent nodeOne nodeTwo = True
  | otherwise = any ((flip isDescendent) nodeTwo) $ children nodeOne

isRoot :: Node -> Bool
isRoot = isNothing . parentId . data_

appendChild :: Node -> Node -> ([Node] -> [Node]) -> Node
appendChild (Node c children) child func = Node c $ func (child:children)

nestComments :: [Node] -> [Node]
nestComments [] = []
nestComments [a] = [a]
nestComments (c:xs) = case c of
                        (Node (Comment _ (Just pid_)) _) -> do
                          case (filter ((flip isDescendent) c) xs) of
                            [] -> c:nestComments xs
                            prnts -> nestComments $ (map (\p -> appendChild p c nestComments) prnts) ++ (xs \\ prnts)
                        _ -> if all isRoot (c:xs)
                              then (c:xs)
                              else nestComments $ xs ++ [c]

main = print $ nestComments $ map (flip Node $ []) [
                                                    (Comment 2 (Just 1)),
                                                    (Comment 1 Nothing),
                                                    (Comment 3 (Just 1)),
                                                    (Comment 4 Nothing),
                                                    (Comment 5 (Just 4)),
                                                    (Comment 6 (Just 5)),
                                                    (Comment 7 Nothing),
                                                    (Comment 8 Nothing),
                                                    (Comment 9 (Just 6)),
                                                    (Comment 10 Nothing)
                                                    ]
