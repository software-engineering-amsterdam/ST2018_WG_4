module Main where

import           Data.Maybe
import           Lecture5
import           Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\x1b[36m== Assignment 1 (NRC Sudoku's) ==\x1b[0m"
     solveAndShow nrcExample

     putStrLn "\n\x1b[36m== Assignment 2 (Refactored NRC Sudoku) ==\x1b[0m"
     quickCheckResult $ forAll genSudokuPositions freePosTest

     putStrLn "\n\x1b[36m== Assignment 3 (Test for unique solutions) ==\x1b[0m"
     testHasUniqueSol 1 3

     putStrLn "\n\x1b[36m== Assignment 4 (Empty block sudoku problems) ==\x1b[0m"
     putStrLn "Trying max 10 times to find a sudoku problem with at least 3 empty blocks."
     generateSudokuProblem 3 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "Trying max 10 times to find a sudoku problem with at least 4 empty blocks."
     generateSudokuProblem 4 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "Trying max 10 times to find a sudoku problem with at least 5 empty blocks."
     generateSudokuProblem 5 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "Trying max 10 times to find a sudoku problem with at least 6 empty blocks."
     generateSudokuProblem 6 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
