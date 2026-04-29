{-# LANGUAGE DeriveFoldable #-}
-- enables explicitly with a language pragma
{-# LANGUAGE DerivingStrategies #-}

module Types where

-- for generic types
import Data.Aeson (ToJSON) -- for json generation
import GHC.Generics (Generic)

data Step = Step
  { stepNumber :: Int,
    currentState :: [Int],
    comparing :: (Int, Int)
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

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Foldable) -- leaf | node x l r

instance (Show a) => Show (BTree a) where
  show Empty = "nil"
  show (Node x l r) = "Node " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"
