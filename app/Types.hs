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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)