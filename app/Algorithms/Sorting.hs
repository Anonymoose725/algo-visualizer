module Algorithms.Sorting where

import Types -- for now

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
    step =
      Step
        { stepNumber = n,
          currentState = finalState,
          comparing = (x1, x2),
          comparingIndices = (length acc, length acc + 1),
          partitionInfo = Nothing
        }

bubbleSort :: [Int] -> [Step]
bubbleSort [] = [] -- no work to be done:
bubbleSort list = go list 1
  where
    go :: [Int] -> Int -> [Step] -- helper
    go l n =
      let (steps, l') = pass l n []
       in if l == l'
            then steps
            else steps ++ go l' (n + length steps)

{- mergesort -}
--           full     lo     hi     n
mergeSort :: [Int] -> Int -> Int -> Int -> ([Step], [Int])
mergeSort full lo hi n
  | lo >= hi = ([], full) -- full coverage
  | otherwise =
      let mid = lo + (hi - lo) `div` 2
          (leftSteps, fullAfterL) = mergeSort full lo mid n
          (rightSteps, fullAfterR) = mergeSort fullAfterL (mid + 1) hi (n + length leftSteps)
          leftSorted = take (mid - lo + 1) (drop lo fullAfterL) -- left side
          rightSorted = take (hi - mid) (drop (mid + 1) fullAfterR) -- right side
          (mergeSteps, mergedPortion) = merge fullAfterL leftSorted rightSorted lo mid hi [] (n + length leftSteps + length rightSteps) 0 0
          fullAfterMerging = (take lo fullAfterL) ++ mergedPortion ++ (drop (hi + 1) fullAfterR)
       in (leftSteps ++ rightSteps ++ mergeSteps, fullAfterMerging)

--       full     left     right    lo     mid    hi     acc      n     Ltaken  Rtaken
merge :: [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> [Int] -> Int -> Int -> Int -> ([Step], [Int])
merge _ [] ys _ _ _ acc _ _ _ = ([], reverse acc ++ ys) -- left branch exhausted
merge _ xs [] _ _ _ acc _ _ _ = ([], reverse acc ++ xs) -- right branch exhausted
merge full (x : xs) (y : ys) lo mid hi acc n leftTaken rightTaken =
  let indexOfX = lo + leftTaken
      indexOfY = mid + 1 + rightTaken
   in if x <= y
        then
          let newAcc = x : acc -- x merged
              mergedSoFar = reverse newAcc -- reverse more efficient than acc ++ [x]
              remaining = xs ++ (y : ys)
              mergedSlice = mergedSoFar ++ remaining

              rightNow = take lo full ++ mergedSlice ++ drop (hi + 1) full

              step =
                Step
                  { stepNumber = n,
                    currentState = rightNow,
                    comparing = (x, y),
                    comparingIndices = (indexOfX, indexOfY),
                    partitionInfo = Just (PartitionInfo (lo, mid) (mid + 1, hi))
                  }

              (restSteps, restList) =
                merge full xs (y : ys) lo mid hi newAcc (n + 1) (leftTaken + 1) rightTaken
           in (step : restSteps, restList)
        else
          let newAcc = y : acc -- y merged
              mergedSoFar = reverse newAcc
              remaining = (x : xs) ++ ys
              mergedSlice = mergedSoFar ++ remaining

              rightNow = take lo full ++ mergedSlice ++ drop (hi + 1) full

              step =
                Step
                  { stepNumber = n,
                    currentState = rightNow,
                    comparing = (x, y),
                    comparingIndices = (indexOfX, indexOfY),
                    partitionInfo = Just (PartitionInfo (lo, mid) (mid + 1, hi))
                  }

              (restSteps, restList) =
                merge full (x : xs) ys lo mid hi newAcc (n + 1) leftTaken (rightTaken + 1)
           in (step : restSteps, restList)

{- insertion sort -}

insertionSort :: [Int] -> Int -> ([Step], [Int])
insertionSort xs = go xs 1 -- reduce n on both sides
  where
    --      arr    i      n
    go :: [Int] -> Int -> Int -> ([Step], [Int])
    go arr i stepNum -- (not called n to avoid shadow)
      | i >= length arr = ([], arr)
      | otherwise =
          let key = arr !! i
              (insertionSteps, arrAfterInsertion, n') = insertLeft arr key i (i - 1) stepNum
              (restSteps, finalArr) = go arrAfterInsertion (i + 1) n'
           in (insertionSteps ++ restSteps, finalArr)

    --             arr     key    i      j      n       steps   cur    stepNum
    insertLeft :: [Int] -> Int -> Int -> Int -> Int -> ([Step], [Int], Int)
    insertLeft arr key i j stepNum
      | j < 0 = ([], arr, stepNum)
      | otherwise =
          let x = arr !! j
              step =
                Step
                  { stepNumber = stepNum,
                    currentState = arr,
                    comparing = (x, key),
                    comparingIndices = (j, i),
                    partitionInfo = Nothing
                  }
           in if x > key
                then
                  let shiftSorted = swapPos i x $ swapPos j key arr -- swap
                      (restSteps, finalArr, n') = insertLeft shiftSorted key j (j - 1) (stepNum + 1)
                   in (step : restSteps, finalArr, n')
                else ([step], arr, stepNum + 1)

swapPos :: Int -> a -> [a] -> [a]
swapPos indexOfX v xs = take indexOfX xs ++ [v] ++ drop (indexOfX + 1) xs