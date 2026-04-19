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