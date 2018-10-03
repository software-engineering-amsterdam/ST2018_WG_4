module Main where

import System.Random
import Test.QuickCheck
import Control.Monad
import Data.List
import Lecture5

-- Assignment 1
-- consistent :: Sudoku -> Bool
-- consistent s = and $
--                [ rowInjective s r |  r <- positions ]
--                 ++
--                [ colInjective s c |  c <- positions ]
--                 ++
--                [ subgridInjective s (r,c) |
--                     r <- [2,6], c <- [2,6]]



-- main :: IO ()
-- main = do
