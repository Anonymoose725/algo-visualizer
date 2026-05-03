module Algorithms.BST where

import Algorithms.Trees
import qualified Data.Map.Strict as Map
import Types (BTree (..), GEdge (..), GNode (..), GraphResponse (..), GraphStep (..))

-- aproach: building a tree
-- 1. get all values inorder with `insertion order, given from Map. caller must have a prebuilt map`
-- 2. zip with indices to get [(id, value)]
-- 3. build nodes from that
-- 4. build edges by traversing tree and looking up each value's ID in a Map (see qualified import)

treeToGraph :: BTree Int -> Map.Map Int Int -> ([GNode], [GEdge])
treeToGraph t idMap =
  let t_nodes = map (\(val, i) -> GNode {nodeID = i, nodeValue = val}) (Map.toAscList idMap) -- could be O(n)
      t_edges = buildEdges t idMap
   in (t_nodes, t_edges)
  where
    buildEdges :: BTree Int -> Map.Map Int Int -> [GEdge]
    buildEdges Empty _ = []
    buildEdges (Node x l r) m =
      let leftEdges = case (Map.lookup x m, l) of -- tuple case matching
            (Just parentID, Node leftVal _ _) ->
              case Map.lookup leftVal m of
                Just leftID -> [GEdge {fromNode = parentID, toNode = leftID, weight = Nothing}]
                Nothing -> []
            _ -> []
          rightEdges = case (Map.lookup x m, r) of
            (Just parentID, Node rightVal _ _) ->
              case Map.lookup rightVal m of
                Just rightID -> [GEdge {fromNode = parentID, toNode = rightID, weight = Nothing}]
                Nothing -> []
            _ -> []
       in leftEdges ++ buildEdges l m ++ rightEdges ++ buildEdges r m

-- at each comparison, we just highlight the current node
--                tree        target    idMap
bstSearchSteps :: BTree Int -> Int -> Map.Map Int Int -> [GraphStep]
bstSearchSteps Empty _ _ = [] -- target not found
bstSearchSteps (Node x l r) k idMap =
  case Map.lookup x idMap of
    Nothing -> [] -- node not in map, shouldnt happen
    Just xid ->
      let step =
            GraphStep
              { highlightedNodes = [xid],
                highlightedEdges = []
              }
       in if k == x
            then [step]
            else
              if k < x
                then step : bstSearchSteps l k idMap
                else step : bstSearchSteps r k idMap

-- at each node, record a step highlighting the current node (while searching for position)
-- when Empty is hit, new node is inserted: record a final step and highlight the new node
-- after each insertion, the tree changes, so idMap needs to be rebuilt
-- each insertion produces: a. a new GraphStep showing path taken, and b. the updated tree
--                vals
bstInsertSteps :: [Int] -> ([GraphStep], BTree Int, Map.Map Int Int)
bstInsertSteps vals = go vals Empty Map.empty 0
  where
    go [] t idMap _ = ([], t, idMap)
    go (x : xs) t idMap nextID =
      let stepsInPath = findPath x t idMap
          t' = insertBST x t
          idMap' = Map.insert x nextID idMap
          insertStep = GraphStep {highlightedNodes = [nextID], highlightedEdges = []}
          (restSteps, finalTree, idMapComplete) = go xs t' idMap' (nextID + 1) -- incremement id by insertion order
       in (stepsInPath ++ [insertStep] ++ restSteps, finalTree, idMapComplete)

findPath :: Int -> BTree Int -> Map.Map Int Int -> [GraphStep]
findPath _ Empty _ = []
findPath v (Node x l r) idMap =
  case Map.lookup x idMap of
    Nothing -> []
    Just xID ->
      let step = GraphStep {highlightedNodes = [xID], highlightedEdges = []}
       in if v <= x
            then step : findPath v l idMap
            else step : findPath v r idMap