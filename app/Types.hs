{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Types where

-- for generic types
import Data.Aeson (ToJSON) -- for json generation
import GHC.Generics (Generic)

data Step = Step
  { stepNumber :: Int,
    currentState :: [Int],
    comparing :: (Int, Int),
    comparingIndices :: (Int, Int),
    partitionInfo :: Maybe PartitionInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance Show Step where -- custom Show typeclass instance
  show s =
    "Step "
      ++ show (stepNumber s)
      ++ " | comparing "
      ++ show (comparing s)
      ++ " at indices "
      ++ show (comparingIndices s)
      ++ " | state: "
      ++ show (currentState s)
      ++ " | partitions: "
      ++ show (partitionInfo s)

data PartitionInfo = PartitionInfo
  { leftRange :: (Int, Int),
    rightRange :: (Int, Int)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
  deriving (Show)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Foldable) -- leaf | node x l r

instance (Show a) => Show (BTree a) where
  show Empty = "nil"
  show (Node x l r) = "Node " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

data GNode = GNode
  { nodeID :: Int, -- need a way to calculatee efficiently and be unique: insertion order?
    nodeValue :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data GEdge = GEdge
  { fromNode :: Int, -- note: reference by ID
    toNode :: Int,
    weight :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data GraphStep = GraphStep
  { highlightedNodes :: [String],
    highlightedEdges :: [(String, String)]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance Show GraphStep where
  show s =
    "visiting nodes: "
      ++ show (highlightedNodes s)
      ++ " | edges: "
      ++ show (highlightedEdges s)

data GraphResponse = GraphResponse
  { nodes :: [GNode],
    edges :: [GEdge],
    steps :: [GraphStep]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
