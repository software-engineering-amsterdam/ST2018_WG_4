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

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

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
consecutive101 :: Integer
consecutive101 = consecutive 0 100


-- Excercise 6

productPlusOneIsPrime :: Int -> Bool
productPlusOneIsPrime n = prime ((product (take n primes)) + 1)

refuteConsecutivePrimes :: [Int]
refuteConsecutivePrimes = filter (productPlusOneIsPrime (primes)) primes

-- Excercise 7

numDigits :: Int -> Int
numDigits x = length (show x)

parity :: Integer -> Integer
parity x = rem (x - 1) 2

luhn :: [Integer] -> Integer -> Bool
luhn [] sum = rem sum 10 == 0
-- luhn (d:ds) sum = luhn ds

-- isAmericanExpress, isMaster, isVisa :: Integer -> Bool

-- Excercise 8
