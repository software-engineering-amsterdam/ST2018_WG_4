module Assignment5

where

import Data.List
import System.Random
import Assignment1

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
