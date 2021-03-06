-- NAME : Sander Meester
-- CKNUM : 11014822
-- STUDY : Master Software Engineering
-- COURSE : Software Specification, Verification and Testing
--
-- Lab5

module Ex1

where

import Lecture5
import System.Random
import Test.QuickCheck
import Data.List
import Data.Char
import Debug.Trace

import Control.Conditional
import Control.Monad

--Exercise 1
-- Sudoku extension

-- Solution to∷
--  +---------+---------+---------+
--  |         | 3       |         |
--  |   +-----|--+   +--|-----+   |
--  |   |     | 7|   |  | 3   |   |
--  | 2 |     |  |   |  |     | 8 |
--  +---------+---------+---------+
--  |   |   6 |  |   |5 |     |   |
--  |   +-----|--+   +--|-----+   |
--  |    9  1 | 6       |         |
--  |   +-----|--+   +--|-----+   |
--  | 3 |     |  | 7 |1 | 2   |   |
--  +---------+---------+---------+
--  |   |     |  |   |  |    3| 1 |
--  |   |8    |  | 4 |  |     |   |
--  |   +-----|--+   +--|-----+   |
--  |       2 |         |         |
--  +---------+---------+---------+
--
-- is:
--
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- | 6 1 9 | 7 5 8 | 3 2 4 |
-- | 2 3 5 | 4 1 6 | 9 7 8 |
-- +-------+-------+-------+
-- | 7 2 6 | 8 3 5 | 1 4 9 |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- | 3 5 4 | 9 7 1 | 2 8 6 |
-- +-------+-------+-------+
-- | 5 6 7 | 2 8 9 | 4 3 1 |
-- | 9 8 3 | 1 4 7 | 5 6 2 |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+

-- time spent: 75 mins


nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) nrcBlocks

subGridNrc :: Sudoku -> (Row,Column) -> [Value]
subGridNrc s (r,c) =
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where
   vs = filter (/= 0) (subGridNrc s (r,c))

freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) =
 (freeInRow s r)
  `intersect` (freeInColumn s c)
  `intersect` (freeInSubgrid s (r,c))
  `intersect` (freeInSubgridNrc s (r,c))

consistentNrc :: Sudoku -> Bool
consistentNrc s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) |
                    r <- [2,6], c <- [2,6]]

constraintsNrc :: Sudoku -> [Constraint]
constraintsNrc s = sortBy length3rd
    [(r,c, freeAtPosNrc s (r,c)) |
                       (r,c) <- openPositions s ]

initNodeNrc :: Grid -> [Node]
initNodeNrc gr = let s = grid2sud gr in
              if (not . consistentNrc) s then []
              else [(s, constraintsNrc s)]

succNodeNrc :: Node -> [Node]
succNodeNrc (s,[]) = []
succNodeNrc (s,p:ps) = extendNodeNrc (s,ps) p

extendNodeNrc :: Node -> Constraint -> [Node]
extendNodeNrc (s,constraintsNrc) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         pruneNrc (r,c,v) constraintsNrc) | v <- vs ]

pruneNrc :: (Row,Column,Value)
     -> [Constraint] -> [Constraint]
pruneNrc _ [] = []
pruneNrc (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblock (r,c) (x,y) =
       (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblockNrc (r,c) (x,y) =
       (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNrc (r,c,v) rest

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

solveShowNsNrc :: [Node] -> IO[()]
solveShowNsNrc = sequence . fmap showNode . solveNsNrc

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNsNrc (initNodeNrc gr)

solveNsNrc :: [Node] -> [Node]
solveNsNrc = search succNodeNrc solved

exampleNRC_1 :: Grid
exampleNRC_1 = [[0,0,0,3,0,0,0,0,0],
                [0,0,0,7,0,0,3,0,0],
                [2,0,0,0,0,0,0,0,8],
                [0,0,6,0,0,5,0,0,0],
                [0,9,1,6,0,0,0,0,0],
                [3,0,0,0,7,1,2,0,0],
                [0,0,0,0,0,0,0,3,1],
                [0,8,0,0,4,0,0,0,0],
                [0,0,2,0,0,0,0,0,0]]
