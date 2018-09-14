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

property1, property2, property3, property4, property5 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x
property4 x = (even x && x > 3) || even x
property5 x = even x

properties :: [Int -> Bool]
properties = [property1, property2, property3, property4, property5]

testProperties :: (Int -> Bool) -> [(Int -> Bool)] -> [Bool]
testProperties property [] = []
testProperties property (p:properties) = [(stronger [-10..10] property p)] ++ testProperties property properties

countPropertyStrength :: (Int -> Bool) -> Int
countPropertyStrength p = length (filter (\x -> x == True) (testProperties p properties) )

propertyStrength :: (Int -> Bool) -> Int
propertyStrength p = countPropertyStrength p

-- strengthList :: [Int]
-- strengthList = sortBy (compare `on` (length . snd)) (map (\l -> (propertyStrength l, head l))) properties



-- Recognizing Permutations

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] ys = False
isPermutation (x:xs) ys | elem x ys = isPermutation xs (removeItem x ys)
                        | otherwise = False

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

letterByIndex :: Int -> Char
letterByIndex n = alphabet!!n

transRot13 :: Char -> Char
transRot13 c  | not (elem c alphabet) = c
              | (indexInAlphabet c) >= 26 = letterByIndex ((((indexInAlphabet c) + 13) `mod` 26) + 26)
              | otherwise = letterByIndex (((indexInAlphabet c) + 13) `mod` 26)
-- TODO: Refactor this...

rot13 :: String -> String
rot13 s = concat (map (\y -> [transRot13 y]) s)

-- Test that the inverse of Rot13 is the same
testRot13String = quickCheck ((\s -> rot13 (rot13 s) == s) :: [Char] -> Bool)
-- Test that Rot13 does not change integer values
testRot13Int = quickCheck ((\n -> rot13 (show n) == (show n)) :: [Int] -> Bool)


-- Implementing and testing IBAN validation

-- rotate function found on stack overflow https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

iban :: String -> Bool
iban s = (read $ (concat (map (\x -> if elem x alphabet then (show ((indexInAlphabet x `mod` 26) + 10)) else [x]) (rotate 4 (filter (/=' ') s)))) :: Integer) `mod` 97 == 1

validIbans = ["AL35202111090000000001234567",
              "AD1400080001001234567890",
              "AT483200000012345864",
              "AZ96AZEJ00000000001234567890",
              "BH02CITI00001077181611",
              "BY86AKBB10100000002966000000"]
testIbans = quickCheckResult(all (\i -> iban i) validIbans)

-- TODO: Generate tests using random integers and a check digit which is random, except the correct one
