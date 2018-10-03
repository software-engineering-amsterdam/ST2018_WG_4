module Main where

import Lecture5

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
          solveAndShow example1
          print blocks
