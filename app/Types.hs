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
