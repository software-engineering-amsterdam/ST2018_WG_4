module Assignment4

where

import Data.List
import System.Random
import Lecture5

-- Time spent: 1 hour

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

removeBlock :: Node -> [(Row,Column)]-> Node
removeBlock n [] = n
removeBlock n (x:xs) = eraseN (removeBlock n xs) x

removeBlocks :: Node -> Int -> Node
removeBlocks n 0 = n
removeBlocks n i = removeBlocks (removeBlock n (blockConstrnt!!index)) index
                  where index = i-1

genProblemThreeEmptyBlocks :: Node -> IO Node
genProblemThreeEmptyBlocks n = genProblem (removeBlocks n 3)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblemThreeEmptyBlocks r
          showNode s
