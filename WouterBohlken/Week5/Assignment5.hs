module Assignment5

where

import Data.List
import System.Random
import Assignment1

-- Time spent: 15 minutes
-- The problems generated by the code in Assignment 2 inherently comply to the NRC constraint

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

{-

Example:

+-------+-------+-------+
| 7 9 8 | 5 6 1 | 4 2 3 |
| 1 5 3 | 8 2 4 | 6 7 9 |
| 4 6 2 | 9 7 3 | 1 8 5 |
+-------+-------+-------+
| 8 7 1 | 4 3 5 | 2 9 6 |
| 5 4 6 | 2 8 9 | 7 3 1 |
| 3 2 9 | 6 1 7 | 5 4 8 |
+-------+-------+-------+
| 9 8 4 | 1 5 2 | 3 6 7 |
| 6 3 5 | 7 4 8 | 9 1 2 |
| 2 1 7 | 3 9 6 | 8 5 4 |
+-------+-------+-------+
+-------+-------+-------+
|       | 5   1 | 4     |
|   5   |       | 6     |
| 4     | 9 7   |       |
+-------+-------+-------+
|     1 |   3   |       |
|     6 |   8   |       |
|   2   |       |       |
+-------+-------+-------+
|       |   5   |     7 |
|       |     8 |   1   |
|       |       |       |
+-------+-------+-------+
-}