module Assignment4

where

import Data.List
import System.Random
import Data.Char
import Control.Arrow
import Data.Maybe
import Data.String
import Data.Array.IO
import Control.Monad
import Lecture5

-- Time spent: 2 hours

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

removeBlock :: Node -> [(Row,Column)]-> Node
removeBlock n [] = n
removeBlock n (x:xs) = eraseN (removeBlock n xs) x

removeBlocks :: Node -> [Int] -> Node
removeBlocks = foldl (\ n x -> removeBlock n (blockConstrnt !! x))

genProblemNBlocks :: Node -> [Int] -> IO Node
genProblemNBlocks n xs = genProblem (removeBlocks n xs)

-- From https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

generateWithBlocksMissing :: Int -> IO Node
generateWithBlocksMissing n = do
                                [r] <- rsolveNs [emptyN]
                                a <- shuffle [0..8]
                                p <- genProblemNBlocks r (take n a)
                                showNode p
                                return p


findN :: Int -> IO ()
findN n = do
          a <- generateWithBlocksMissing n
          if uniqueSol a then
            showNode a
            else findN n


{-


FindN 3 yielded:
+-------+-------+-------+
| 1 2 6 | 8 7 5 | 4 9 3 |
| 8 4 5 | 3 9 6 | 2 7 1 |
| 3 9 7 | 1 2 4 | 6 8 5 |
+-------+-------+-------+
| 5 3 4 | 9 6 7 | 1 2 8 |
| 7 6 8 | 5 1 2 | 3 4 9 |
| 9 1 2 | 4 3 8 | 7 5 6 |
+-------+-------+-------+
| 4 5 3 | 2 8 1 | 9 6 7 |
| 6 8 9 | 7 4 3 | 5 1 2 |
| 2 7 1 | 6 5 9 | 8 3 4 |
+-------+-------+-------+
+-------+-------+-------+
|       | 8   5 | 4     |
|       | 3 9   |   7   |
|       |   2   |     5 |
+-------+-------+-------+
|     4 |       | 1   8 |
|       |       | 3 4   |
| 9 1 2 |       |       |
+-------+-------+-------+
|     3 |       |   6   |
| 6 8 9 |       |     2 |
| 2     |       |   3   |
+-------+-------+-------+


FindN 4 yielded:
+-------+-------+-------+
| 5 8 7 | 9 1 6 | 3 4 2 |
| 9 2 1 | 4 3 5 | 8 6 7 |
| 6 3 4 | 2 7 8 | 5 1 9 |
+-------+-------+-------+
| 2 4 8 | 1 6 3 | 7 9 5 |
| 7 9 3 | 5 4 2 | 6 8 1 |
| 1 5 6 | 7 8 9 | 4 2 3 |
+-------+-------+-------+
| 4 1 2 | 6 5 7 | 9 3 8 |
| 3 7 9 | 8 2 4 | 1 5 6 |
| 8 6 5 | 3 9 1 | 2 7 4 |
+-------+-------+-------+
+-------+-------+-------+
| 5   7 |       | 3     |
| 9     |       | 8 6 7 |
| 6 3   |       |   1 9 |
+-------+-------+-------+
|       | 1   3 |       |
|       | 5   2 |       |
|       |   8 9 |       |
+-------+-------+-------+
|   1 2 |       | 9 3   |
|   7   |       |   5 6 |
| 8 6   |       |     4 |
+-------+-------+-------+


 FindN 5 didn't seem to generate a minimal problem after 100+ tries


 -}