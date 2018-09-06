module Excercises where

import Lab1

import Data.List
import Test.QuickCheck

faculty :: Int -> Int
faculty 1 = 1
faculty n = faculty (n - 1) * n

squareFaculty :: Int -> Int
squareFaculty 0 = 0
squareFaculty x = squareFaculty (x - 1) + x ^ 2

tripleFaculty :: Int -> Int
tripleFaculty 0 = 0
tripleFaculty x = tripleFaculty (x - 1) + x ^ 3

listProduct :: [Integer] -> Integer
listProduct [] = 1
listProduct (x:xs) = x * listProduct xs

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

test6 = quickCheckResult (\n -> n >= 0 --> (isPrimeReversal n) == (elem n (primeReversalRange n)))
-- HORRIBLE IMPLEMENTATION !!!! PLEASE DO NOT USE THIS

-- Excercise 5

takePrimes :: Int -> Int -> [Integer]
takePrimes x y = drop x (take (y+1) primes)

consecutive :: Int -> Int -> Integer
consecutive x y     | prime (sum (takePrimes x y)) = sum (takePrimes x y)
                    | otherwise = consecutive (x+1) (y+1)


-- Excercise 6

test7 = quickCheckResult (\n -> n >= 2 --> prime ((product [2..n]) + 1))
-- First fail is 4, because (2 * 3 * 4) + 1 = 25. 25 can be devided by 5 and thus is not a prime


-- Excercise 7

-- numDigits :: Integer -> Integer
-- numDigits x = length show x

-- parity :: Integer -> Integer
-- parity x = x - 1 % 2


-- Excercise 8

