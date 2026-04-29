module Algorithms.Trees where

import Types (BTree (..), Step (..))

insertBST :: (Ord a) => a -> BTree a -> BTree a
insertBST x Empty = Node x Empty Empty
insertBST x (Node v l r)
  | x < v = Node v (insertBST x l) r
  | otherwise = Node v l (insertBST x r)

searchBST :: (Ord a) => a -> BTree a -> Bool
searchBST _ Empty = False
searchBST x (Node v l r)
  | x == v = True
  | x < v = searchBST x l
  | otherwise = searchBST x r -- use guard matching instead of if/else to avoid lazy || or

fromList :: (Ord a) => [a] -> BTree a
fromList = foldr insertBST Empty -- insertions fold right

height :: BTree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

inorder :: BTree a -> [a] -- inorder traversal (left, root, right)
inorder Empty = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r
