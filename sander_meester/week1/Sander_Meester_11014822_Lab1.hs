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
import Data.Char

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

-- time: 45 mins
slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from + 1) (drop from xs)

getSumOfPrimes :: Int -> Int -> Int
getSumOfPrimes i j
    | prime(sum (slice i j primes )) = sum (slice i j primes )
    | otherwise = getSumOfPrimes (i+1) (j+1)

get101Prime = getSumOfPrimes 0 100

-- We can test this by going down over all primes starting at the prime we had as an answer, and checking whether it is a sum of any of sets of 100 consecutive primes found before this prime.

--Exercise 6
-- time 30 mins
getProductOfPrimes :: Int -> Int
getProductOfPrimes i
    | not (prime(product (slice 0 i primes ) +1)) = primes !! i
    | otherwise = getProductOfPrimes (i+1)

getCounterExample = getProductOfPrimes 2
-- The smallest counterexample is [2*3*5*7*11*13]+1 = 30031 (Pn=13)

--Exercise 7
-- time 1 hour (- tests)
-- https://stackoverflow.com/questions/2838727/how-do-i-get-the-sums-of-the-digits-of-a-large-number-in-haskell
sumDigit :: Int -> Int
sumDigit 0 = 0
sumDigit x = (x `mod` 10) + sumDigit (x `div` 10)

doubleSecondDigits :: [Int] -> [Int]
doubleSecondDigits (n:nt:ns) = n : sumDigit(nt*2) : doubleSecondDigits(ns)
doubleSecondDigits n = n

luhn :: Integer -> Bool
luhn n = sum (doubleSecondDigits (map digitToInt (show n))) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = luhn n
isMaster n = luhn n
isVisa n = luhn n

-- Test functions:
-- TODO: these

-- Exercise 8
--  time

accuses :: Boy -> Boy -> Bool
accuses b1 b2
    | b1 == Peter && (b2 == Matthew || b2 == Jack) = True
    | b1 == Jack && (b2 == Matthew || b2 == Peter) = True
    | b1 == Carl && b2 == Arnold = True
    -- | b1 == Arnold && (b2 == Matthew || b2 == Peter) = True
    | otherwise = False

accusers :: Boy -> [Boy]
accusers b
    | b == Matthew = [Peter, Jack]
    | b == Peter = [Jack]
    | b == Jack = [Peter]
    | b == Arnold = [Carl]
    | b == Carl = []
    | otherwise = []


-- guilty :: [Boy]
-- honest :: [Boy]
