module Main where

import           Data.Maybe
import           Lecture5
import           Test.QuickCheck

-- Please note that all code for the assignments is in the Lecture5.hs file, seperated by comments denoting the specific assignments.
main :: IO ()
main =
  do putStrLn "\x1b[36m== Assignment 1 (NRC Sudoku's) ==\x1b[0m"
     solveAndShow nrcExample

     putStrLn "\n\x1b[36m== Assignment 2 (Refactored NRC Sudoku) ==\x1b[0m"
     quickCheckResult $ forAll genSudokuPositions freePosTest

     putStrLn "\n\x1b[36m== Assignment 3 (Test for minimal problems) ==\x1b[0m"
     testIsMinimal 0 10

     putStrLn "\n\x1b[36m== Assignment 4 (Empty block sudoku problems) ==\x1b[0m"
     putStrLn "Trying max 10 times to find a sudoku problem with at least 3 empty blocks."
     generateSudokuProblem 3 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "\nTrying max 10 times to find a sudoku problem with at least 4 empty blocks."
     generateSudokuProblem 4 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "\nTrying max 10 times to find a sudoku problem with at least 5 empty blocks."
     generateSudokuProblem 5 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"
     putStrLn "\nTrying max 10 times to find a sudoku problem with at least 6 empty blocks."
     generateSudokuProblem 6 10 >>= \x -> if isJust x then fromJust x >>= \y -> showNode y else putStrLn "Not found"

     putStrLn "\n\x1b[36m== Assignment 5 (NRC Problem generator) ==\x1b[0m"
     showNode =<< (genRandomSudoku >>= genProblem)

     putStrLn "\n\x1b[36m== BONUS Assignment 7 (Hints comparison NRC vs normal sudokus) ==\x1b[0m"
     nums <- genNumHints
     putStrLn "Average tips from ten generated sudokus:"
     print (fromIntegral(sum nums) / fromIntegral(length nums))
     nums <- genNumHintsNRC
     putStrLn "\nAverage tips from ten generated NRC sudokus:"
     print (fromIntegral(sum nums) / fromIntegral(length nums))
