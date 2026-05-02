module Algorithms.BST where

import Algorithms.Trees
import qualified Data.Map.Strict as Map
import Types (BTree (..), GEdge (..), GNode (..), GraphResponse (..), GraphStep (..))

-- aproach: building a tree
-- 1. get all values inorder with `inorder tree`
-- 2. zip with indices to get [(id, value)]
-- 3. build nodes from that
-- 4. build edges by traversing tree and looking up each value's ID in a Map (see qualified import)

treeToGraph :: BTree Int -> ([GNode], [GEdge])
treeToGraph t =
  let values = inorder t
      idMap = Map.fromList (zip values [0 ..]) -- map : value -> ID
      t_nodes = zipWith (\val i -> GNode {nodeID = i, nodeValue = val}) values [0 ..]
      t_edges = buildEdges t idMap
   in (t_nodes, t_edges)
  where
    buildEdges :: BTree Int -> Map.Map Int Int -> [GEdge]
    buildEdges Empty _ = []
    buildEdges (Node x l r) idM =
      let parentID = idM Map.! x -- ! get id for node with value x
          leftEdges = case l of
            Empty -> []
            Node leftVal _ _ -> [GEdge {fromNode = parentID, toNode = idM Map.! leftVal, weight = Nothing}]
          rightEdges = case r of
            Empty -> []
            Node rightVal _ _ -> [GEdge {fromNode = parentID, toNode = idM Map.! rightVal, weight = Nothing}]
       in leftEdges ++ (buildEdges l idM) ++ rightEdges ++ (buildEdges r idM)
