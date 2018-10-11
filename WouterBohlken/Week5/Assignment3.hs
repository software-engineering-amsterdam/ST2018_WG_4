module Assignment3

where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5

-- Time: 1 hour
-- Generate a sudoku P, check if P has a unique solution
-- Then generate all P' sudoku's by removing one item and check if all new problems P' are ambiguous

main :: IO ()
main = do
          [r] <- rsolveNs [emptyN]
          s  <- genProblem r
          let fillPos = filledPositions (fst s)
          let solutions = map (eraseN s) fillPos
          quickCheck(uniqueSol s)
          quickCheck(all not $ map uniqueSol solutions)
