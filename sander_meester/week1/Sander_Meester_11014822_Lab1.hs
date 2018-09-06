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

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Int -> Int
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
-- time: 30 mins

findPrimes :: Int -> [Int]
findPrimes n = filter prime [0..n]

isReversablePrime :: Int -> Bool
isReversablePrime n = prime n && prime (reversal n)

reversablePrimes :: [Int]
reversablePrimes = filter (isReversablePrime) (findPrimes 10000)

-- We could test this function by iterating of all primes and checking if every reversable prime is in the list.

--Exercise 5
-- Source of slice:
-- https://www.google.nl/search?q=haskell+slice+list&oq=haskell+slice&aqs=chrome.0.69i59j69i60j0j69i57j0l2.1217j0j7&sourceid=chrome&ie=UTF-8

-- time:
slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from + 1) (drop from xs)

getSumOfPrimes :: Int -> Int -> Int
getSumOfPrimes i j
    | prime(sum (slice i j primes )) = sum (slice i j primes )
    | otherwise = getSumOfPrimes (i+1) (j+1)

get101Prime = getSumOfPrimes 0 100

-- We can test this by going over all primes starting at 100, and checking whether it is a sum of the 100 primes before it
