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

sieve :: [Int] -> [Int]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

eprimes = sieve [2..]

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
-- time 1.25 hour
-- https://stackoverflow.com/questions/2838727/how-do-i-get-the-sums-of-the-digits-of-a-large-number-in-haskell
sumDigit :: Int -> Int
sumDigit 0 = 0
sumDigit x = (x `mod` 10) + sumDigit (x `div` 10)

doubleSecondDigits :: [Int] -> [Int]
doubleSecondDigits (n:nt:ns) = n : sumDigit(nt*2) : doubleSecondDigits(ns)
doubleSecondDigits n = n

digitToList :: Integer -> [Int]
digitToList n = map digitToInt (show n)

listToDigit :: [Int] -> Integer
listToDigit ns = read (map intToDigit ns)

luhn :: Integer -> Bool
luhn n = sum (doubleSecondDigits (reverse (digitToList n))) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = (luhn n) && (length (show n) == 15) && (listToDigit ( slice 0 2 (digitToList n)) > 33) && (listToDigit ( slice 0 1 (digitToList n)) < 38)
isMaster n = luhn n && (length (show n) == 16) && (
              ((listToDigit ( slice 0 1 (digitToList n)) > 50) &&
               (listToDigit ( slice 0 1 (digitToList n)) < 56)) ||
              ((listToDigit ( slice 0 3 (digitToList n)) > 2221) &&
               (listToDigit ( slice 0 3 (digitToList n)) < 2720))
             )
isVisa n = luhn n && (length (show n) == 16) && (slice 0 0 (digitToList n) !! 0 == 4)

-- Tests:
visaCards = [4916036260934004,
             4539301335926626,
             4916848251695919,
             4539738376397136,
             4485933520215466,
             6277500350364257,
             550612643282991,
             4323950234189624,
             375543148983147,
             53252569781795000]

masterCards = [5277500350364257,
               5506126432829910,
               5323950234189624,
               5407673640547685,
               5325256978179500,
               6277500350364257,
               550612643282991,
               4323950234189624,
               375543148983147,
               53252569781795000]

americanExpressCards = [375543148983147,
                        379773178506528,
                        372003346961034,
                        340656764193148,
                        344174328134547,
                        6277500350364257,
                        550612643282991,
                        4323950234189624,
                        075543148983147,
                        53252569781795000]

checkAmericanExpress= map isAmericanExpress americanExpressCards
checkMasterCards = map isMaster masterCards
checkVisas = map isVisa visaCards

-- Exercise 8
--  time
-- Three boys speak the truth, therefore is someone has three accusers someone is guilty?

accuses :: Boy -> Boy -> Bool
accuses b1 b2
    | b1 == Matthew = (b1 /= b2 && b2 /= Carl)
    | b1 == Peter = (b2 == Matthew || b2 == Jack)
    | b1 == Jack = not (accuses Matthew b2 || accuses Peter b2)
    | b1 == Arnold = (/=) (accuses Matthew b2) (accuses Peter b2)
    | b1 == Carl = not (accuses Arnold b2)
    | otherwise = False

accusers :: Boy -> [Boy]
accusers b1 = [b2 | b2 <- boys, accuses b2 b1]

honest :: [Boy]
honest = filter (\x -> accuses x (head guilty)) boys

guilty :: [Boy]
guilty = filter (\x -> length (accusers x) == 3) boys

-- Guilty: [Jack], Honest: [Matthew,Peter,Carl]


-- Euler problem 9
--  time 30 mins
pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)
   [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y  ]

getRightPtriple :: [(Integer,Integer,Integer)] -> (Integer, Integer, Integer)
getRightPtriple xs
       | map (\(x,y,z) -> x+y+z ) ([head xs]) !! 0 == 1000 = head xs
       | otherwise = getRightPtriple (tail xs)

-- Product of abc: (200,375,425) = 1000

-- Euler problem 10
--  time

-- Finds the sum of the first 2 million primes
-- Answer: 142913828922
sumManyPrimes :: Int
sumManyPrimes = sum (takeWhile (< 2000000) primes)
-- To Slow right now

-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this sequence?

-- Euler problem 49
--  time
fourDigPrimes :: [Int]
fourDigPrimes = filter (> 999) primes

getPrimePermutations :: Int -> [Integer]
getPrimePermutations n = sort (map listToDigit (nub (filter (\x -> x !! 0 /= 0) (permutations (map digitToInt (show(n)))))))

getPossiblePrimes :: [[Integer]]
getPossiblePrimes = map getPrimePermutations fourDigPrimes

checkPrimes :: [Integer] -> [Bool]
checkPrimes ps = map prime (map fromIntegral ps)

findSeqPrimes :: [[Integer]] -> [Integer]
findSeqPrimes (p:ps)
       | length (filter (==True) (checkPrimes(p))) > 2 = p
       | otherwise = findSeqPrimes ps

-- euler49 :: [[Integer]]
-- euler49 =
