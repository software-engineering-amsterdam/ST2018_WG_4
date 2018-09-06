-- NAME : Sander Meester
-- CKNUM : 11014822
-- STUDY : Master Software Engineering
-- COURSE : Software Specification, Verification and Testing
--
-- Lab1
--

module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


factorial :: Int -> Int
factorial n = product [1..n]

sumOfPwrs :: Int -> Int -> Int
sumOfPwrs n pwr = sum ( map (^pwr) [1..n])

-- Exercise 1:
-- time: 1.5 hour
exTwoTest :: Int -> Bool
exTwoTest n = (n > 0) --> (sumOfPwrs n 2 == ( ((n * (n+1)) * (2*n+1)) `div` 6))

exThreeTest :: Int -> Bool
exThreeTest n = (n > 0) --> (sumOfPwrs n 3 == ( (n * (n+1) `div` 2) ^2))

--Exercise 2
-- time: 30 mins
exFourTest :: [Int] -> Bool
exFourTest xs = (length (xs) < 20) --> (2 ^ length (xs)) == length( subsequences xs)
-- Is the property hard to test?
--  - yes, since the complexity rises very fast with larger lists,
--  - the testing becomes very slow. (max length of 20 was set to prevent long running)

-- We are checking the mathematical fact, proven by induction.

--Exercise 3
-- time: 30 mins
exFiveTest :: [Int] -> Bool
exFiveTest xs = (length (xs) < 10) --> length (permutations xs) == factorial (length (xs))
-- Is the property hard to test?
--  - yes, since the complexity rises very fast with larger lists,
--  - the testing becomes very slow. (max length of 20 was set to prevent long running)

-- We are checking the mathematical fact, proven by induction.

--Exercise 4
-- time:
exFiveTest :: [Int] -> Bool
exFiveTest xs = (length (xs) < 10) --> length (permutations xs) == factorial (length (xs))
