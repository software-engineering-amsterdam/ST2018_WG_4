module Excercises where

import Lecture2
import Lab2

import Data.List
import Data.Maybe
import Test.QuickCheck

-- Useful logic notation

devideQuartiles :: IO ()
devideQuartiles = do
                    xs <- probs 10000
                    let
                      l1 = filter (\x -> x >= 0 && x <= 0.25) xs
                      l2 = filter (\x -> x > 0.25 && x <= 0.5) xs
                      l3 = filter (\x -> x > 0.5 && x <= 0.75) xs
                      l4 = filter (\x -> x > 0.75 && x <= 1) xs
                      in print [length l1, length l2, length l3, length l4]

-- test1 = quickCheck (sum (read $ devideQuartiles) == 10000)
-- Test that the sum of all lists == 10000

-- Recognizing triangles

isPythagorean :: Integer -> Integer -> Integer -> Bool
isPythagorean a b c = (a^2 + b^2) == c^2

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (a + b) <= c || (a + c) <= b || (b + c) <= a = NoTriangle
                | a == b && b == c = Equilateral
                | a == b || b == c || a == c = Isosceles
                | isPythagorean a b c || isPythagorean b c a || isPythagorean c a b = Rectangular
                | otherwise = Other

-- Testing properties strength

property1, property2, property3, property4 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x
property4 x = (even x && x > 3) || even x

-- strengthList :: IO ()
-- strengthList = do



-- Recognizing Permutations


-- isPermutation :: Eq a => [a] -> [a] -> Bool
-- isPermutation a b = quicksort a == quicksort b


-- Recognizing and generating derangements


isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = quicksort xs == quicksort ys && (filter (\x -> fst x == snd x) (zip xs ys) == [])

deran :: [Int] -> [[Int]]
deran xs = filter (isDerangement xs) (permutations xs)


-- Implementing and testing ROT13 encoding

alphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']

indexInAlphabet :: Char -> Int
indexInAlphabet c = fromJust $ elemIndex c alphabet

transRot13 :: Char -> Char
transRot13 c  | not (elem c alphabet) = c
              | (indexInAlphabet c) >= 26 = alphabet!!((((indexInAlphabet c) + 13) `mod` 26) + 26)
              | otherwise = alphabet!!(((indexInAlphabet c) + 13) `mod` 26)
-- TODO: Refactor this...


rot13 :: String -> String
rot13 s = concat (map (\y -> [transRot13 y]) s)

-- Implementing and testing IBAN validation

-- rotate function found on stack overflow https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

iban :: String -> Bool
iban s = (read $ (concat (map (\x -> if elem x alphabet then (show ((indexInAlphabet x `mod` 26) + 10)) else [x]) (rotate 4 (filter (/=' ') s)))) :: Integer) `mod` 97 == 1

-- TODO: Generate tests using random integers and a check digit which is random, except the correct one
