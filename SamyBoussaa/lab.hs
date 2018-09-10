import Test.QuickCheck
import Data.List
import Lab1
-- import Data.Numbers

-- Assignment 1 formula 2
-- started 9:15
-- finished 10:17
leftHandSide:: Int -> Int
leftHandSide n = sum(map (^2) [1..n])


rightHandSide:: Int -> Int
rightHandSide n  =  quot (n * (n + 1) * (2*n + 1)) (6)


funcTwoTest :: Int -> Bool
funcTwoTest n
  | n > 0 = leftHandSide n == rightHandSide n
  | otherwise = True

-- Assignment 1 formula 3
-- started 10:22
-- finished 10:25

leftHandSideThree:: Int -> Int
leftHandSideThree n = sum(map (^3) [1..n])


rightHandSideThree:: Int -> Int
rightHandSideThree n = (quot (n * (n + 1)) (2))^2


funcThreeTest:: Int -> Bool
funcThreeTest n
  | n > 0 = leftHandSideThree n == rightHandSideThree n
  | otherwise = True



-- Assignment 2
-- started 10:37
-- finished 10:53


fourSet :: Int -> Bool
fourSet n
  | n <  10 && n > 0 = length (subsequences [1..n]) == 2^n
  | otherwise = True

-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: The property is hard to test because it when using a set with high cardinality, the
-- complexity of the problem becomes too high. It takes a lot of time too compute.

-- Q: Give your thoughts on the following issue: when you perform the test for exercise 4,
--    what are you testing actually? Are you checking a mathematical fact? Or are you
--    testing whether subsequences satisfies a part of its specification? Or are you
--    testing something else still?
-- A: I am testing a mathematical hypothese (can become a fact if proven by induction)
--    for a finite set with a cardinality of 10 max. (Refine?)

factorial :: Int -> Int
factorial n = product([1..n])

-- Assignment 3
-- started 11:35
-- finished 11:45
assignThree :: Int -> Bool
assignThree n = length (permutations [1..n]) == factorial n

-- Is the property hard to test? If you find that it is, can you given a reason why?


-- Assignment 4
-- started 12.12
-- finished ...
-- estimated time (30 mins)
primeListReversal :: [Integer]
primeListReversal = filter (\x -> elem (reversal x) primelist) primelist where
                    primelist = takeWhile (< 10000) primes

-- Reverse each prime and compare it to the list, sort the list. check for each
-- Element if prime
testPrimeListReversal :: Bool
testPrimeListReversal = ((sort [ reversal x | x<-primeListReversal]) == primeListReversal) ==  not (elem False (map prime primeListReversal))

-- Assignment 5
-- started 14.23
-- finished 15:35

-- 37447

slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to - from) (drop from xs)

primelist :: Int -> [Integer]
primelist n = take 101 (drop n primes) --slice n (101 + n) primes

-- Feed n = 0
smallestPrime :: Int -> Integer
smallestPrime n = if (prime (sum(primelist n))) then sum(primelist n) else smallestPrime (n + 1)

-- Assignment 6
-- started 16:11
-- finished ~17:10 (estimated)

conjuctureSix :: Int -> [Integer] -> [Integer]
conjuctureSix n total
 | not (prime (product(take n primes) + 1)) = conjuctureSix (n + 1) (total ++ [product(take n primes) + 1])
 | otherwise = conjuctureSix (n +1)  total

conjuctureSixMin :: [Integer]
conjuctureSixMin = take 1 (conjuctureSix 0 [])



-- Old implementation
-- if not (prime (product(slice 0 n primes) + 1))
--                 then  "When n is " ++ show n ++ " then the number that is not prime is " ++ show (product(slice 0 n primes) + 1) ++
--                       ""
--                 else conjuctureSix (n + 1)



 -- Assignment 7 - read, string == list of chars
 -- started 17:37
 -- finished

-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

oneList :: [a] -> [b] -> [(a, b)]
oneList []     _      = []
oneList (x:xs) (y:ys) = (x, y) : oneList xs ys

luhn :: Integer -> [(Int, Integer)]
luhn creditcard_number = oneList digits creditcard_number combination
                         where
                        combination = genericTake (genericLength (digits creditcard_number)) (cycle [2, 1])
