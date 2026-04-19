module Algorithms.Sorting where

import Types (Step (..)) -- for now

{- bubblesort -}

-- pass -> the "outer for loop" of our bubblesort, with accumulator
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

bubbleSort :: [Int] -> [Step]
bubbleSort [] = [] -- no work to be done
bubbleSort list = go list 1
  where
    go :: [Int] -> Int -> [Step] -- helper
    go l n =
      let (steps, l') = pass l n []
       in if l == l'
            then steps
            else steps ++ go l' (n + length steps)

{- mergesort -}

mergeSort :: [Int] -> Int -> ([Step], [Int])
mergeSort [] _ = ([], [])
mergeSort [x] _ = ([], [x])
mergeSort xs n =
  let (left, right) = splitAt (length xs `div` 2) xs
      (leftSteps, leftSorted) = mergeSort left n
      (rightSteps, rightSorted) = mergeSort right (n + length leftSteps)
      (mergeSteps, merged) = merge leftSorted rightSorted (n + length leftSteps + length rightSteps)
   in (leftSteps ++ rightSteps ++ mergeSteps, merged)

merge :: [Int] -> [Int] -> Int -> ([Step], [Int])
merge [] ys _ = ([], ys)
merge xs [] _ = ([], xs)
merge (x : xs) (y : ys) n =
  let state = (x : xs) ++ (y : ys)
      step = Step {stepNumber = n, currentState = state, comparing = (x, y)}
   in if x < y
        then
          let (restSteps, restList) = merge xs (y : ys) (n + 1)
           in (step : restSteps, x : restList)
        else
          let (restSteps, restList) = merge (x : xs) ys (n + 1)
           in (step : restSteps, y : restList)