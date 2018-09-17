
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Exersize 1
--  Time: 120 minutes

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

fillQuartiles :: Int -> IO [Int]
fillQuartiles n = do tmp <- probs n
                     return [length [x | x <- tmp, x < 0.25], length [x | x <- tmp, x >= 0.25, x < 0.5],length [x | x <- tmp, x >= 0.5, x < 0.75], length [x | x <- tmp, x >= 0.75]]

calcFractions :: IO [Int] -> IO [Int]
calcFractions xs = do tmp <- xs
                      return (map (\x -> abs(x - (sum tmp `div` 4))) tmp)

testProbs :: Int -> IO [Float]
testProbs n = do tmp <- calcFractions (fillQuartiles n)
                 return (map (\x -> fromIntegral x / fromIntegral n) tmp)

-- Test report:

-- Exersize 2
--  Time: 45 minutes

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
     | a <= 0 || b <= 0 || c <= 0 = NoTriangle
     | a + b <= c || b + c <= a || a + c <= b = NoTriangle
     | a == b && b == c = Equilateral
     | a == b || a == c || b == c = Isosceles
     | a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2 = Rectangular
     | otherwise = Other

-- Test report:

-- Exersize 3
--  Time: 30 minutes
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

prop1, prop2, prop3, prop4 :: Integer -> Bool
prop1 x = even x && x > 3
prop2 x = even x || x > 3
prop3 x = (even x && x > 3) || even x
prop4 = even

domain = [(-10)..10]

-- Exersize 4
--  Time: 15 minutes

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = forall xs (`elem` ys)

-- Exersize 5:
--  Time: 30 minutes

-- Some help from:
-- https://stackoverflow.com/questions/24233238/comparing-elements-at-the-same-index-in-two-lists
compareIndexes :: Eq a => [(a, a)] -> [a]
compareIndexes = map fst . filter (uncurry (==))

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = forall xs (\x -> elem x ys && null(compareIndexes (zip xs ys)))

-- Exersize 6:
--  Time
-- Specification of ROT13: f(x) -> x + 13 mod 26

getElemIndex :: Char -> Int
getElemIndex c = let index = elemIndex c ['a'..'z']
                 in case index of
                      Just x -> x
                      Nothing -> -1

performROT13 :: String -> String
performROT13 (c:cs)
     | getElemIndex c /= -1 = (['a'..'z'] !! ((getElemIndex c + 13) `mod` 26)) : performROT13 cs
     | getElemIndex c == -1 = c : performROT13 cs
     | otherwise = []
performROT13 c = []

-- Exersize 7:
--  Time

iban :: String -> Bool
