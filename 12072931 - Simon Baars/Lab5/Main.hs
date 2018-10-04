module Main where

import Lecture5
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\x1b[36m== Assignment 1 (NRC Sudoku's) ==\x1b[0m"
     solveAndShow nrcExample

     putStrLn "\n\x1b[36m== Assignment 2 (Refactored NRC Sudoku) ==\x1b[0m"
     quickCheckResult $ forAll genSudokuPositions freePosTest

     putStrLn "\n\x1b[36m== Assignment 3 (Refactored NRC Sudoku) ==\x1b[0m"
     testHasUniqueSol 1 10
