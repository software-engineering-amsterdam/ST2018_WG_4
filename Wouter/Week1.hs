module Excercises where

import Lab1

import Data.List
import Test.QuickCheck

faculty :: Int -> Int
faculty 0 = 0
faculty n = faculty (n - 1) * n

squareFaculty :: Int -> Int
squareFaculty 0 = 0
squareFaculty x = squareFaculty (x - 1) + x ^ 2

tripleFaculty :: Int -> Int
tripleFaculty 0 = 0
tripleFaculty x = tripleFaculty (x - 1) + x ^ 3

-- Excercise 1

formula2 :: Int -> Int
formula2 n = (n * (n + 1) * (2 * n + 1)) `div` 6

formula3 :: Int -> Int
formula3 n = (n * (n + 1) `div` 2) ^ 2


test2 = quickCheckResult (\n -> n >= 0 --> squareFaculty n == formula2 n)

test3 = quickCheckResult (\n -> n >= 0 --> tripleFaculty n == formula3 n)

-- Excercise 2

formula4 :: Int -> Int
formula4 n = length (subsequences [1..n])

test4 = quickCheckResult (\n -> squareFaculty n == formula3 n) 

-- Exercise 3

permLength :: Int -> Int
permLength n = length (permutations [1..n])

test5 = quickCheckResult (\n -> n >= 0 --> faculty n == permLength n) 

-- Excercise 4

primeRange :: Integer -> [Integer]
primeRange x = 2 : filter prime [3..x]

isPrimeReversal:: Integer -> Bool
isPrimeReversal x = prime x && prime (reversal x)

primeReversalRange:: Integer -> [Integer]
primeReversalRange x = filter (isPrimeReversal) (primeRange x)

-- Excercise 5

