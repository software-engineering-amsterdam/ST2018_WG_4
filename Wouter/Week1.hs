module Excercises where

import Lab1

import Data.Char
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

primeList :: Int -> [Integer]
primeList x = take x primes

productPlusOneIsNotPrime :: [Integer] -> Bool
productPlusOneIsNotPrime n = not (prime ((product n) + 1))

refuteConsecutivePrimes :: [[Integer]]
refuteConsecutivePrimes = filter productPlusOneIsNotPrime (map (primeList) [1..])
-- 'take 1 refuteConsecutivePrimes' yields '[[2,3,5,7,11,13]]'



-- Excercise 7

isEven :: Int -> Bool
isEven n = mod n 2 == 0

isOdd :: Int -> Bool
isOdd n = not (isEven n)

processDigit :: Int -> Int
processDigit d | d <= 9     = d
               | otherwise  = d - 9

checkLuhn :: [Int] -> Bool
checkLuhn ds = rem (sum (filter (isEven) ds) + sum (map (processDigit) (filter (isOdd) (map (*2) ds)))) 10 == 0

luhn :: Integer -> Bool
luhn ds = checkLuhn (map (digitToInt) (reverse (show ds)))

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = length (show n) == 15 && ( ("34" `isPrefixOf` (show n) || "37" `isPrefixOf` (show n)))

isMaster n = length (show n) == 16 && (
                                (let first2 = take 2 (show n)
                                  in (first2 >= "50" && first2 <= "55") ) ||
                                (let first6 = take 6 (show n)
                                  in (first6 >= "222100" && first6 <= "272099") ) )

isVisa n = take 1 (show n) == "4" && (let numberLength = length (show n)
                                    in (numberLength == 13 || numberLength == 16 || numberLength == 19) )

-- Excercise 8

-- accuses :: Boy -> Boy -> Bool
--
-- accusers :: Boy -> [Boy]
--
-- guilty, honest :: [Boy]


-- BONUS

-- 9


sumPythagorean :: Int -> Int -> Int
sumPythagorean a b = sum [a, b, (floor (sqrt(fromIntegral(a^2) + fromIntegral(b^2))))]

isSpecial :: Int -> Int -> Bool
isSpecial a b = ((a^2) + (b^2)) == floor ((sqrt(fromIntegral(a)^2 + fromIntegral(b)^2)))^2

pythagoreanTriplets :: Int -> Int -> [Int]
pythagoreanTriplets a b | a >= b = pythagoreanTriplets a (a+1)
                        | sumPythagorean a b < 1000 = pythagoreanTriplets a (b+1)
                        | sumPythagorean a b == 1000 && isSpecial a b = [a, b, floor (sqrt(fromIntegral(a^2) + fromIntegral(b^2)))]
                        | sumPythagorean a b > 1000 && (a == (b-1)) = []
                        | otherwise = pythagoreanTriplets (a+1) (a+2)

specialPythagoreanTriplet :: [Int]
specialPythagoreanTriplet = pythagoreanTriplets 1 2

-- 10
