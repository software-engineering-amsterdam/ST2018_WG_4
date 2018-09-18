module Excercises where

import Lecture2

import Data.Char
import Data.List
import Data.Maybe
import Data.String
import System.Random
import Test.QuickCheck

-- probs
-- Time: 1:30 minutes

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

devideQuartiles :: Int -> IO [Int]
devideQuartiles n = do
                    xs <- probs n
                    let
                      l1 = filter (\x -> x >= 0 && x <= 0.25) xs
                      l2 = filter (\x -> x > 0.25 && x <= 0.5) xs
                      l3 = filter (\x -> x > 0.5 && x <= 0.75) xs
                      l4 = filter (\x -> x > 0.75 && x <= 1) xs
                    return [length l1, length l2, length l3, length l4]

-- First we test if all numbers are contained in quartiles and thus there is no x where x < 0 or x > 1
devideQuartilesAmountTest :: Int -> IO ()
devideQuartilesAmountTest n = do
                        quartiles <- devideQuartiles n;
                        quickCheck((sum quartiles) == n)



-- test1 = quickCheck (sum (read $ devideQuartiles) == 10000)
-- TODO: Test that the sum of all lists == 10000


-- Recognizing triangles
-- Time: 2 hours

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

isPythagorean :: Integer -> Integer -> Integer -> Bool
isPythagorean a b c = (a^2 + b^2) == c^2

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (a + b) <= c || (a + c) <= b || (b + c) <= a = NoTriangle
                | a == b && b == c = Equilateral
                | a == b || b == c || a == c = Isosceles
                | isPythagorean a b c || isPythagorean b c a || isPythagorean c a b = Rectangular
                | otherwise = Other

triangleText :: Integer -> Integer -> Integer -> String
triangleText a b c  | triangle a b c == NoTriangle = "Not a triangle"
                    | triangle a b c == Equilateral = "Equilateral"
                    | triangle a b c == Isosceles = "Isosceles"
                    | triangle a b c == Rectangular = "Rectangular"
                    | otherwise = "Other"


-- We verify that the order of parameters doesn't has no effect on the output of the function
testTriangleInput = quickCheckResult(\a b c -> (triangle a b c) == (triangle b a c) && (triangle a b c) == (triangle c a b))

-- To verify that when the sum of 2 sides is smaller than one other side, we generate 2 variales
-- Then we generate a third parameter by adding these variables + 1, and then verify that the output is "NoTriangle"

testNoTriangle = quickCheckResult(\a b -> a >= 1 && b >= 1 --> triangle a b (a+b+1) == NoTriangle)
-- We verify that when we use a single random number as all 3 parameters, the outcome will always be "Equilateral"

testEqualateral = quickCheckResult(\a -> a >= 1 --> triangle a a a == Equilateral)
-- When 2 parameters are the same, the outcome should be "Isosceles", as an extra check, we verify that a /= b, otherwise the outcome would be "Equilateral"

testIsosceles = quickCheckResult(\a b -> a >= 1 && b >= 1 && a /= b && (a+a) > b && (b+b) > a --> triangle a a b == Isosceles)

-- testRectangular = quickCheckResult(\a b -> a >= 2 && b >= 3 && a /= b --> triangle a b (sqrt (a^2 + b^2)) == Rectangular)

-- We test other cases by generating a number strating from 4, and adding 1 and 2 to the other parameters, respectively
-- We start from 4, as the outcome a=1 would be "NoTriangle", and a=3 would be "Rectangular"
testOther = quickCheckResult(\a -> a >= 4 --> triangle a (a+1) (a+2) == Other)

-- Testing properties strength
-- Time: 1:30 hours

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

propertyMapping :: Int -> [Int] -> [(String, Int)]
propertyMapping n [] = []
propertyMapping n (x:xs) = [("property" ++ show n, x)] ++ propertyMapping (n+1) xs

strengthList :: [(String, Int)]
strengthList = sortBy (\ x y -> compare (snd y) (snd x)) (propertyMapping 1 ((map (propertyStrength)) properties))




-- Recognizing Permutations
-- Time: 1:30 hours
-- "You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure":
-- This means that we need to include a precondition that filters list containing duplicates

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

permutationTests = [(([1,2,3], [2,3,1]), True),
                      (([1,2,3], [2,3,1,5]), False),
                      (([1,2,3,5], [2,3,1]), False),
                      (([], []), True)]

testPermutations = quickCheckResult(all (\(x, t) -> isPermutation (fst x) (snd x) == t) permutationTests)
-- TODO: Ordered list of stronger and weaker
-- TODO: Automate using new techniques




-- Recognizing and generating derangements
-- Time: 1 hour

isDerangement :: Ord a => [a] -> [a] -> Bool
isDerangement xs ys = quicksort xs == quicksort ys && (filter (\x -> fst x == snd x) (zip xs ys) == [])

deran :: [Int] -> [[Int]]
deran xs = filter (isDerangement xs) (permutations xs)

derangementTests = [(([1,2,3], [2,3,1]), True),
                      (([1,2,3], [2,3,1,5]), False),
                      (([1,2,3,5], [2,3,1]), False),
                      (([1,2,3,4], [2,3,1,4]), False),
                      (([], []), True)]

testDerangements = quickCheckResult(all (\(x, b) -> isDerangement (fst x) (snd x) == b) derangementTests)
-- TODO: Ordered list of stronger and weaker
-- TODO: Automate using new techniques



-- Implementing and testing ROT13 encoding
-- Time: 45 minutes
-- TODO: Specification

-- Specification:

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

rot13 :: String -> String
rot13 s = concat (map (\c -> [transRot13 c]) s)

-- Test that the inverse of Rot13 is the same
testRot13StringInverse = quickCheck ((\s -> ((rot13 . rot13) s) == s) :: [Char] -> Bool)
-- Test that Rot13 does not change integer values
testRot13IntEqual = quickCheck ((\n -> rot13 (show n) == (show n)) :: [Int] -> Bool)
-- TODO: QuickChecks by Specification


-- Implementing and testing IBAN validation
-- Time: 1:30 hours


-- rotate function found on stack overflow https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

iban :: String -> Bool
iban s = (read $ (concat (map (\x -> if elem x alphabet then (show ((indexInAlphabet x `mod` 26) + 10)) else [x]) (rotate 4 (filter (/=' ') s)))) :: Integer) `mod` 97 == 1

validIbans = ["NL37INGB0008825966",
              "AL35202111090000000001234567",
              "AD1400080001001234567890",
              "AT483200000012345864",
              "AZ96AZEJ00000000001234567890",
              "BH02CITI00001077181611",
              "BY86AKBB10100000002966000000"]

testValidIbans = quickCheckResult(all (\i -> iban i) validIbans)

-- Found on http://bluebones.net/2007/01/replace-in-haskell/
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

intToCheckDigits :: Int -> String
intToCheckDigits n  | length (show n) == 1 = "0" ++ show n
                    | otherwise = show n


ibanBase = "NLXXINGB0008825966"

createTestableIban :: Int -> String
createTestableIban n = replace ibanBase "XX" (intToCheckDigits n)

ibanTestFunction :: Int -> Bool
ibanTestFunction n = iban (createTestableIban n)

-- In this test, we use an IBAN base, where the check digits are replaced by "XX"
-- Then we generate IBAN's for all digits 1-97 and we verify that exactly one of the generated IBAN's is valid
testGeneratedIbans = quickCheck(length (filter (\x -> x == True) (map (ibanTestFunction) [1..97])) == 1)

-- Euler project 1
-- Time: 20 seconds
multiples :: [Int]
multiples = filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [0..1000]


main = do
  putStrLn "\nProbs"
  devideQuartilesAmountTest 1000000

  putStrLn "\nRecognizing triangles"
  testTriangleInput
  testNoTriangle
  testEqualateral
  testIsosceles
  -- testRectangular
  testOther

  putStrLn "\nRecognizing Permutations"
  testPermutations

  putStrLn "\nRecognizing and generating derangements"
  testDerangements

  putStrLn "\nImplementing and testing ROT13 encoding"
  testRot13StringInverse
  testRot13IntEqual

  putStrLn "\nImplementing and testing IBAN validation"
  testValidIbans
  testGeneratedIbans
