import Test.QuickCheck
import Data.List

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Lab Assignment 1:

assignmentTwoLeftSide :: Integer -> Integer
assignmentTwoLeftSide n = sum(map (^2) [1..n])

assignmentTwoRightSide :: Integer -> Integer
assignmentTwoRightSide n = (n*(n+1)*(2*n+1)) `div` 6

assignmentTwoTest :: Integer -> Bool
assignmentTwoTest n = (n > 0) --> assignmentTwoLeftSide n == assignmentTwoRightSide n

assignmentThreeLeftSide :: Integer -> Integer
assignmentThreeLeftSide n = sum(map (^3) [1..n])

assignmentThreeRightSide :: Integer -> Integer
assignmentThreeRightSide n = ((n*(n+1)) `div` 2)^2

assignmentThreeTest :: Integer -> Bool
assignmentThreeTest n = (n > 0) --> assignmentThreeLeftSide n == assignmentThreeRightSide n

-- Lab Assignment 2:

getListOfSizeN :: Integer -> [Integer]
getListOfSizeN n
 | n <= 0 = [] -- Base case
 | otherwise = [0..n]

getPowerListOfSizeN :: Integer -> [[Integer]]
getPowerListOfSizeN  n
 | n <= 0 = [[]] -- Base case
 | otherwise = subsequences [0..n]

powerListInductionTest :: Integer -> Bool
powerListInductionTest n = (n >= 0) --> length(getPowerListOfSizeN(n)) == 2 ^ (length(getListOfSizeN(n)))

genPos :: Gen Integer
genPos = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (< 25)

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- It is, because of the complexity of the resulting list of subsequences for large numbers (>25). Due to this, QuickCheck gets blocked during the checking. To solve this, a custom QuickCheck generator had to be written to instruct QuickCheck to only pick numbers below 25.

-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?
-- Answer: Actually I am testing both. If either the `subsequences` method or the mathematical fact `|A| = n then |P(A)| = 2^n` was incorrect, the test would fail. Another aspect of the haskell language that would be tested by this code would be the sequence generator (`[0..n]`).

-- Lab Assignment 3



main :: IO ()
main = do
  quickCheckResult assignmentTwoTest
  quickCheckResult assignmentThreeTest
  quickCheckResult $ forAll genPos $ powerListInductionTest
  print "Done!"
