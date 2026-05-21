module Algorithms.Graphs where

import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Set (Set)
import qualified Data.Set as Set
import Types (GEdge (..), GNode (..), GraphResponse (..), GraphStep (..))

-- type synonyms
type ID = String

type Dist = Int

type AdjMap = Map ID [(ID, Dist)] -- maps node id -> neighbours (their ids, weights to get to them)

type DistMap = Map ID (Maybe Dist)

type Visited = Set ID

type PQ = MinPQueue Dist ID

-- parsing to AdjMap
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  let (word, rest) = break (== delim) str -- ==delim predicate
   in word : case rest of
        [] -> []
        (_ : t) -> splitOn delim t

parseEdge :: [String] -> Maybe (ID, ID, Dist)
parseEdge [from, to, w] = Just (from, to, read w)
parseEdge _ = Nothing -- parse fail

-- edgeStrings seperates edges, parts seperates those into (node, node, weight) and tuples filters out the failures
parseEdges :: String -> AdjMap
parseEdges s =
  let edgeStrings = splitOn ';' s
      parts = map (splitOn ',') edgeStrings
      tuples = [t | Just t <- map parseEdge parts] -- filter Nothing out. see Data.List.catMaybes
   in foldl' (\m (from, to, w) -> Map.insertWith (++) from [(to, w)] m) Map.empty tuples

-- get all unique node labels from the adjacency map
allLabels :: AdjMap -> [ID]
allLabels adjMap = nub $ Map.keys adjMap ++ concatMap (map fst) (Map.elems adjMap)

-- assign each label a numeric ID
labelToID :: [ID] -> Map ID Int
labelToID labels = Map.fromList (zip labels [0 ..])

-- build GNode list
buildGNodes :: [ID] -> Map ID Int -> [GNode]
buildGNodes labels idMap =
  map
    ( \label ->
        GNode
          { nodeID = idMap Map.! label,
            nodeValue = 0,
            nodeLabel = Just label
          }
    )
    labels

-- build GEdge list
buildGEdges :: AdjMap -> Map ID Int -> [GEdge]
buildGEdges adjMap idMap =
  concatMap
    ( \(from, neighbours) ->
        map
          ( \(to, w) ->
              GEdge
                { fromNode = idMap Map.! from,
                  toNode = idMap Map.! to,
                  weight = Just w
                }
          )
          neighbours
    )
    (Map.toList adjMap)

--            AdjMap    Source
--        A,B,2;B,C,4;   A
runDijkstra :: String -> ID -> [GraphStep]
runDijkstra str src =
  let graph = parseEdges str
      listOfnodes = allLabels graph -- returns list containing just keys (ids)
      distMap = Map.fromList [(n, if n == src then Just 0 else Nothing) | n <- listOfnodes] -- set dst[src] = 0
      pq = PQ.singleton 0 src
      visited = Set.empty
   in dijkstra graph distMap visited pq

dijkstra :: AdjMap -> DistMap -> Visited -> PQ -> [GraphStep]
dijkstra graph dists visited q
  | PQ.null q = []
  | otherwise =
      case PQ.minViewWithKey q of
        Nothing -> [] -- queue is empty
        Just ((dist, u), q')
          | Set.member u visited -> dijkstra graph dists visited q' -- skip since u visited
          | otherwise ->
              let -- relax neighbours
                  visited' = Set.insert u visited
                  neighbours = Map.findWithDefault [] u graph -- instead of lookup
                  (dists', q'') = foldl' (relax dist) (dists, q') neighbours -- currying relax
               in let -- build steps
                      selectionStep =
                        GraphStep
                          { highlightedNodes = [u],
                            highlightedEdges = [],
                            distances = Just (Map.toList dists')
                          }
                      relaxSteps =
                        map
                          ( \(v, _) ->
                              -- ignore distance, this was handled logically by relaxing. just need ID for step
                              GraphStep
                                { highlightedNodes = [u, v],
                                  highlightedEdges = [(u, v)],
                                  distances = Just (Map.toList dists')
                                }
                          )
                          neighbours
                      restSteps = dijkstra graph dists' visited' q''
                   in selectionStep : relaxSteps ++ restSteps

relax :: Dist -> (DistMap, PQ) -> (ID, Dist) -> (DistMap, PQ)
relax uDist (dists, pq) (v, w) =
  let newDist = uDist + w
      oldDist = Map.findWithDefault Nothing v dists
   in case oldDist of
        Just d | d <= newDist -> (dists, pq) -- old path better
        _ -> (Map.insert v (Just newDist) dists, PQ.insert newDist v pq) -- improvement

-- full response builder
dijkstraResponse :: String -> ID -> GraphResponse
dijkstraResponse edgeStr source =
  let adjMap = parseEdges edgeStr
      labels = allLabels adjMap
      idMap = labelToID labels
      ns = buildGNodes labels idMap
      es = buildGEdges adjMap idMap
      fullsteps = runDijkstra edgeStr source
   in GraphResponse {nodes = ns, edges = es, steps = fullsteps}
