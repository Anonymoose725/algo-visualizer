module Algorithms.Sorting where

import Types (Step (..)) -- for now

{- pass -> the "outer for loop" of our bubblesort, with accumulator -}
pass :: [Int] -> Int -> [Int] -> ([Step], [Int])
pass [] _ acc = ([], reverse acc)
pass [x] _ acc = ([], reverse acc ++ [x]) -- not considered a "step" since no comparison is made
pass list@(x1 : x2 : xs) n acc =
  if x1 > x2
    then
      let (restSteps, restList) = pass (x1 : xs) (n + 1) (x2 : acc)
       in (step : restSteps, restList)
    else
      let (restSteps, restList) = pass (x2 : xs) (n + 1) (x1 : acc)
       in (step : restSteps, restList)
  where
    finalState = reverse acc ++ list
    step = Step {stepNumber = n, currentState = finalState, comparing = (x1, x2)}
