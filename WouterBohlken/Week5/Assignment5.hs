module Assignment5

where

import Data.List
import System.Random
import Assignment1

-- Time spent: 15 minutes
-- The problems generated by the code in Assignment1 inherently comply to the NRC constraint 

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s