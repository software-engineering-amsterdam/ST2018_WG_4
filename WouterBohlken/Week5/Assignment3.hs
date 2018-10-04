module Assignment3

where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5

-- Time: 45 minutes
-- Remove all items and check if all new problems are ambiguous
test :: IO ()
test = do
          [r] <- rsolveNs [emptyN]
          s  <- genProblem r
          let fillPos = filledPositions (fst s)
          let solutions = map (eraseN s) fillPos
          quickCheck(uniqueSol s)
          quickCheck(all not $ map uniqueSol solutions)
