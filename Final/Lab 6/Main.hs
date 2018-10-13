module Main where

import System.Random
import Test.QuickCheck
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import Data.List
import Control.Exception
import System.Clock
import Test.QuickCheck.Monadic
import Lecture6
import Data.Maybe

-- Assignment 1
-- Time: 220 minutes
-- Please see the Lecture6.hs file for the code of the exM function.
-- Result:
--                    +++ OK, passed 100 tests.
testExM :: (Integer,Integer,Integer) -> Bool
testExM (x,y,n) = exM x y n == expM x y n

-- Assignment 2
-- Time: 180 minutes
-- Result:
--                    exM 5 7 2 took -1748 nanoseconds less than 5^7 `mod` 2.
--                    exM 3 2 3 took -1778 nanoseconds less than 3^2 `mod` 3.
--                    exM 2 1 4 took -1483 nanoseconds less than 2^1 `mod` 4.
--                    exM 4 4 2 took -2233 nanoseconds less than 4^4 `mod` 2.
--                    exM 5 1 2 took -1220 nanoseconds less than 5^1 `mod` 2.
--                    exM 3 7 7 took -1840 nanoseconds less than 3^7 `mod` 7.
--                    exM 5 6 3 took -1917 nanoseconds less than 5^6 `mod` 3.
--                    exM 4 10 9 took -2909 nanoseconds less than 4^10 `mod` 9.
--                    exM 3 4 8 took -2074 nanoseconds less than 3^4 `mod` 8.
--                    exM 8 3 3 took -1728 nanoseconds less than 8^3 `mod` 3.
--                    exM 12 12 11 took -1901 nanoseconds less than 12^12 `mod` 11.
--                    exM 16 7 3 took -1506 nanoseconds less than 16^7 `mod` 3.
--                    exM 5 18 6 took -2508 nanoseconds less than 5^18 `mod` 6.
--                    exM 7 3 8 took -1597 nanoseconds less than 7^3 `mod` 8.
--                    exM 6 11 8 took -1964 nanoseconds less than 6^11 `mod` 8.
--                    exM 7 20 5 took -1646 nanoseconds less than 7^20 `mod` 5.
--                    exM 23 23 20 took -1439 nanoseconds less than 23^23 `mod` 20.
--                    exM 6 21 19 took -2590 nanoseconds less than 6^21 `mod` 19.
--                    exM 7 6 10 took -1718 nanoseconds less than 7^6 `mod` 10.
--                    exM 3 16 5 took -2117 nanoseconds less than 3^16 `mod` 5.
--                    exM 20 6 4 took -1700 nanoseconds less than 20^6 `mod` 4.
--                    exM 24 14 20 took -1448 nanoseconds less than 24^14 `mod` 20.
--                    exM 28 12 12 took -1898 nanoseconds less than 28^12 `mod` 12.
--                    exM 26 28 16 took -1126 nanoseconds less than 26^28 `mod` 16.
--                    exM 24 38 43 took -2317 nanoseconds less than 24^38 `mod` 43.
--                    exM 2 20 1 took -1657 nanoseconds less than 2^20 `mod` 1.
--                    exM 8 24 1 took -1508 nanoseconds less than 8^24 `mod` 1.
--                    exM 32 11 32 took -2099 nanoseconds less than 32^11 `mod` 32.
--                    exM 28 23 7 took -2163 nanoseconds less than 28^23 `mod` 7.
--                    exM 8 33 9 took -1937 nanoseconds less than 8^33 `mod` 9.
--                    exM 29 5 6 took -1490 nanoseconds less than 29^5 `mod` 6.
--                    exM 2 36 8 took -1829 nanoseconds less than 2^36 `mod` 8.
--                    exM 11 27 16 took -1587 nanoseconds less than 11^27 `mod` 16.
--                    exM 34 17 32 took -1632 nanoseconds less than 34^17 `mod` 32.
--                    exM 29 13 3 took -1435 nanoseconds less than 29^13 `mod` 3.
--                    exM 3 29 5 took -1972 nanoseconds less than 3^29 `mod` 5.
--                    exM 6 3 23 took -1235 nanoseconds less than 6^3 `mod` 23.
--                    exM 8 33 12 took -1999 nanoseconds less than 8^33 `mod` 12.
--                    exM 25 11 1 took -1759 nanoseconds less than 25^11 `mod` 1.
--                    exM 3 17 28 took -1409 nanoseconds less than 3^17 `mod` 28.
--                    exM 34 11 36 took -1894 nanoseconds less than 34^11 `mod` 36.
--                    exM 30 15 7 took -1186 nanoseconds less than 30^15 `mod` 7.
--                    exM 38 27 2 took -1658 nanoseconds less than 38^27 `mod` 2.
--                    exM 8 9 34 took -1925 nanoseconds less than 8^9 `mod` 34.
--                    exM 23 14 41 took -982 nanoseconds less than 23^14 `mod` 41.
--                    exM 2 21 12 took -2095 nanoseconds less than 2^21 `mod` 12.
--                    exM 10 24 60 took -1210 nanoseconds less than 10^24 `mod` 60.
--                    exM 28 3 48 took -740 nanoseconds less than 28^3 `mod` 48.
--                    exM 45 22 22 took -1445 nanoseconds less than 45^22 `mod` 22.
--                    exM 50 20 17 took -1573 nanoseconds less than 50^20 `mod` 17.
--                    exM 61 24 31 took -1610 nanoseconds less than 61^24 `mod` 31.
--                    exM 40 33 1 took -1939 nanoseconds less than 40^33 `mod` 1.
--                    exM 52 9 28 took -2116 nanoseconds less than 52^9 `mod` 28.
--                    exM 50 18 39 took -2009 nanoseconds less than 50^18 `mod` 39.
--                    exM 25 48 19 took -2099 nanoseconds less than 25^48 `mod` 19.
--                    exM 2 68 25 took -1638 nanoseconds less than 2^68 `mod` 25.
--                    exM 30 22 35 took -2019 nanoseconds less than 30^22 `mod` 35.
--                    exM 21 53 37 took -2628 nanoseconds less than 21^53 `mod` 37.
--                    exM 41 62 13 took -2478 nanoseconds less than 41^62 `mod` 13.
--                    exM 29 23 34 took -1840 nanoseconds less than 29^23 `mod` 34.
--                    exM 52 4 25 took -1409 nanoseconds less than 52^4 `mod` 25.
--                    exM 36 57 36 took -1568 nanoseconds less than 36^57 `mod` 36.
--                    exM 62 51 29 took -3134 nanoseconds less than 62^51 `mod` 29.
--                    exM 9 31 64 took -2149 nanoseconds less than 9^31 `mod` 64.
--                    exM 23 29 11 took -1942 nanoseconds less than 23^29 `mod` 11.
--                    exM 15 70 50 took -2331 nanoseconds less than 15^70 `mod` 50.
--                    exM 11 6 27 took -1406 nanoseconds less than 11^6 `mod` 27.
--                    exM 14 18 6 took -1205 nanoseconds less than 14^18 `mod` 6.
--                    exM 62 60 51 took -2967 nanoseconds less than 62^60 `mod` 51.
--                    exM 53 74 78 took -1931 nanoseconds less than 53^74 `mod` 78.
--                    exM 81 31 33 took -1230 nanoseconds less than 81^31 `mod` 33.
--                    exM 55 51 33 took -3300 nanoseconds less than 55^51 `mod` 33.
--                    exM 14 18 62 took -1635 nanoseconds less than 14^18 `mod` 62.
--                    exM 72 9 42 took -1844 nanoseconds less than 72^9 `mod` 42.
--                    exM 62 38 65 took -1958 nanoseconds less than 62^38 `mod` 65.
--                    exM 5 7 66 took -1643 nanoseconds less than 5^7 `mod` 66.
--                    exM 50 45 52 took -3160 nanoseconds less than 50^45 `mod` 52.
--                    exM 15 10 57 took -1442 nanoseconds less than 15^10 `mod` 57.
--                    exM 57 79 21 took -2520 nanoseconds less than 57^79 `mod` 21.
--                    exM 83 27 14 took -1614 nanoseconds less than 83^27 `mod` 14.
--                    exM 75 7 23 took -1322 nanoseconds less than 75^7 `mod` 23.
--                    exM 36 16 13 took -1907 nanoseconds less than 36^16 `mod` 13.
--                    exM 20 80 61 took -3524 nanoseconds less than 20^80 `mod` 61.
--                    exM 82 53 71 took -2569 nanoseconds less than 82^53 `mod` 71.
--                    exM 60 15 90 took -1934 nanoseconds less than 60^15 `mod` 90.
--                    exM 13 38 22 took -2314 nanoseconds less than 13^38 `mod` 22.
--                    exM 47 31 92 took -1065 nanoseconds less than 47^31 `mod` 92.
--                    exM 72 59 29 took -2325 nanoseconds less than 72^59 `mod` 29.
--                    exM 39 13 8 took -1506 nanoseconds less than 39^13 `mod` 8.
--                    exM 49 41 64 took -1822 nanoseconds less than 49^41 `mod` 64.
--                    exM 88 91 91 took -3327 nanoseconds less than 88^91 `mod` 91.
--                    exM 44 1 23 took -1066 nanoseconds less than 44^1 `mod` 23.
--                    exM 56 16 15 took -2271 nanoseconds less than 56^16 `mod` 15.
--                    exM 57 39 42 took -2310 nanoseconds less than 57^39 `mod` 42.
--                    exM 52 12 45 took -1416 nanoseconds less than 52^12 `mod` 45.
--                    exM 87 80 101 took -2535 nanoseconds less than 87^80 `mod` 101.
--                    exM 41 34 95 took -1597 nanoseconds less than 41^34 `mod` 95.
--                    exM 71 27 93 took -1868 nanoseconds less than 71^27 `mod` 93.
--                    exM 36 102 52 took -2709 nanoseconds less than 36^102 `mod` 52.
--                    exM 21 99 87 took -2236 nanoseconds less than 21^99 `mod` 87.
--                    +++ OK, passed 100 tests.
genPos :: Gen (Integer,Integer,Integer)
genPos = (arbitrary :: Gen (Integer,Integer,Integer)) `suchThat` (\(x,y,z) -> x > 0 && y>0 && z>0)

genListOfPos :: Gen [(Integer,Integer,Integer)]
genListOfPos = listOf genPos

testPerformance :: (Integer,Integer,Integer) -> Property
testPerformance (x,y,n) = monadicIO $ do
  currTime <- run(getTime Monotonic)
  run(evaluate $ exM x y n)
  currTime2 <- run(getTime Monotonic)
  run(evaluate $ x ^ y `mod` n)
  currTime3 <- run(getTime Monotonic)
  run(putStrLn $ "\x1b[01m\x1b[96mexM "++ show x ++" "++show y++" "++show n++"\x1b[0m took "++ show ((toNanoSecs currTime3-toNanoSecs currTime2)-(toNanoSecs currTime2-toNanoSecs currTime)) ++" nanoseconds less than \x1b[01m\x1b[96m"++ show x ++"^"++show y++" `mod` "++show n++"\x1b[00m.")
  return True

printRan :: Integer -> Integer -> Integer -> Bool
printRan x y z = trace ("x = " ++ show x ++ ", y = " ++ show y ++ ", z = " ++ show z) True

-- Assignment 3
-- Time: 30 minutes
-- Result:
--                    These are the composite natural numbers between 1 and 100: [8,12,16,18,20,24,27,28,30,32,36,40,42,44,45,48,50,52,54,56,60,63,64,66,68,70,72,75,76,78,80,81,84,88,90,92,96,98,99]
composites :: [Integer]
composites = filter (\x -> length (factors x) > 2) [0..]

-- Assignment 4
-- Time: 60 minutes
-- Result:
--                    100 times doing `findTestFool` till fermat is fooled (3 checks max per number):
--                    [1105,1105,6601,1729,1105,561,2465,2821,2465,1105,1105,1105,1729,1729,1105,1729,2821,2465,1729,561,561,2465,1729,1105,2465,1729,1105,2465,1729,561,561,2465,1105,1105,6601,2465,1105,8911,1729,1729,1105,435,1729,1105,561,561,5565,2821,1105,2465,8911,1105,1035,1729,4123,1105,2821,1105,2821,28,1729,561,1105,2465,561,2465,561,1729,637,6601,1105,2821,561,1105,1105,165,41041,2465,29341,1105,8911,561,45,1729,2465,561,561,2465,1105,1105,1105,1105,2821,1729,561,1105,1105,561,8911,1105]
--
--                    100 times doing `findTestFool` till fermat is fooled (1 check max per number):
--                    [231,28,27,715,275,28,105,63,153,45,273,105,52,105,190,105,561,28,225,76,45,153,27,388,231,81,105,105,175,195,171,45,345,276,645,363,99,75,561,285,165,175,52,45,70,165,105,117,66,45,189,292,45,153,325,125,45,286,45,28,190,45,27,28,28,45,105,75,124,231,561,165,190,153,28,63,125,231,45,232,231,28,435,165,76,207,28,286,286,45,52,66,76,165,105,190,130,231,175,148]
--                    == Assignment 5 (Amount of attempts till carmichael number is hit) ==
--
-- Remark: When checking 3 times, almost always carmicheal numbers are hit. When checking just one time, a lot of lower non-carmicheal numbers are hit.
findTestFool :: Int -> IO Integer
findTestFool k = firstM (\x -> primeTestsF k x >>= \y -> return(prime x /= y)) composites >>= \x -> return (fromJust x)

-- Assignment 5
-- Time: 90 minutes
-- Read the entry on Carmichael numbers on Wikipedia to explain what you find. If necessary, consult other sources.
-- The following summarized what is described about the occurring phenomenon:
-- "The Fermat probable primality test will fail to show a Carmichael number is composite until we run across one of its factors!"
-- Because of that, I use the function `countTillHit` which will show the mount of attempts till the fermat test succeeds in
-- showing primality.
-- Result:
--                    100 times showing primality for carmicheal numbers (amount of tries using fermat is shown), using the first carmicheal number:
--                    [1,1,2,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,3,1,2,2,1,1,1,2,2,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,2,1,1,3,1,1,4,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,3,1,1,1,2,1]
--
--                    100 times showing primality for carmicheal numbers (amount of tries using fermat is shown), using the second carmicheal number:
--                    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1,2,2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1]
--
-- Remark: As visible, how higher the carmicheal number gets, the more often the test doesn't fail. The fermat test fails quite often for low carmicheal numbers, but way less for higher ones.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
     k <- [1..],
     prime (6*k+1),
     prime (12*k+1),
     prime (18*k+1) ]

countTillHit :: (Int -> Integer -> IO Bool) -> Int -> IO Integer
countTillHit f index = let carM = drop index carmichael in firstM (\x -> f 1 (head carM)) [1..] >>= \x -> return (fromJust x)

-- Assignment 6A
-- Time: 30 minutes
-- Use the list from the previous exercise to test the Miller-Rabin primality check. What do you find?
-- Result:
--                    100 times showing primality for carmicheal numbers (amount of tries using Miller-Rabin is shown), using the first carmicheal number:
--                    [3,13,22,9,14,10,4,3,4,45,4,10,7,27,8,3,4,12,4,21,22,12,10,28,15,7,32,19,6,4,4,9,7,7,24,9,10,36,18,3,2,17,15,5,9,52,2,13,7,6,5,14,9,20,13,11,12,27,4,21,1,10,5,9,1,13,14,2,4,11,2,20,15,5,6,2,5,15,19,6,6,29,2,2,42,13,1,5,6,1,20,7,8,22,5,8,9,5,2,5]
--
--                    100 times showing primality for carmicheal numbers (amount of tries using Miller-Rabin is shown), using the second carmicheal number:
--                    [11,59,7,1,14,4,10,14,18,25,11,1,3,3,8,21,8,11,44,12,3,3,5,8,7,9,3,3,4,23,8,13,11,16,8,12,22,24,1,9,10,5,5,7,18,13,28,31,25,7,11,11,39,11,8,17,13,26,7,7,8,10,6,68,1,1,10,33,14,3,10,44,5,10,2,11,6,12,5,1,75,2,19,1,8,10,4,9,3,2,9,3,29,1,11,7,9,12,19,43]
--
-- Remark: This result is very interesting. It seems that Miller-Rabin shows the opposite effect from Fermat. Whereas fermat's tests get fooled more often for low carmicheal numbers, the Miller-Rabin test gets fooled (way) more often for high carmicheal numbers.

-- Assignment 6B
-- Time: 120 minutes
-- You can use the Miller-Rabin primality check to discover some large Mersenne primes. The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2p−1 is also prime. Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.
-- Result:
--                    Using the inefficient but reliable `prime` function:
--                    [2,3,5,7,13,17,19,31]
--
--                    Using fermat:
--                    [2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281]
--
--                    Using Miller-Rabin:
--                    [2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281]
--
-- All these found numbers are actually mersenne primes according to WikiPedia.
--
-- Using Miller-Rabin and Fermat I can get way more Mersenne primes than when using the normal `prime` function. After about 16 Mersenne primes the process gets very slow, and I have a feeling my laptop currently dislikes me for putting all these heavy computations on it. The reliability of Fermat in particular is not always very good.
findMersennePrimesSimple :: Int -> [Integer]
findMersennePrimesSimple amount = take amount (filter (\x -> prime (2^x-1)) primes)

findMersennePrimes :: (Int -> Integer -> IO Bool) -> Int -> Integer -> IO [Integer]
findMersennePrimes algorithm checks maxNum = filterM (\x -> algorithm checks (2^x-1)) (takeWhile (<maxNum) primes)

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 1 (Modular Exponentiation) ==\x1b[0m"
  quickCheck $ forAll genPos testExM

  putStrLn "\x1b[36m== Assignment 2 (Performance Testing) ==\x1b[0m"
  quickCheck $ forAll genPos testPerformance

  putStrLn "\x1b[36m== Assignment 3 (Composite natural numbers) ==\x1b[0m"
  putStrLn $ "These are the composite natural numbers between 1 and 100: " ++ show (takeWhile (<100) composites)

  putStrLn "\x1b[36m== Assignment 4 (Fermat primality check) ==\x1b[0m"
  putStrLn "100 times doing `findTestFool` till fermat is fooled (3 checks max per number):"
  print =<< replicateM 100 (findTestFool 3)
  putStrLn "\n100 times doing `findTestFool` till fermat is fooled (1 check max per number):"
  print =<< replicateM 100 (findTestFool 1)

  putStrLn "\x1b[36m== Assignment 5 (Amount of attempts till carmichael number is hit) ==\x1b[0m"
  putStrLn "100 times showing primality for carmicheal numbers (amount of tries using fermat is shown), using the first carmicheal number:"
  print =<< replicateM 100 (countTillHit primeTestsF 0)
  putStrLn "\n100 times showing primality for carmicheal numbers (amount of tries using fermat is shown), using the second carmicheal number:"
  print =<< replicateM 100 (countTillHit primeTestsF 1)

  putStrLn "\x1b[36m== Assignment 6A (Amount of attempts till carmichael number is hit using Miller-Rabin) ==\x1b[0m"
  putStrLn "100 times showing primality for carmicheal numbers (amount of tries using Miller-Rabin is shown), using the first carmicheal number:"
  print =<< replicateM 100 (countTillHit primeMR 0)
  putStrLn "\n100 times showing primality for carmicheal numbers (amount of tries using Miller-Rabin is shown), using the second carmicheal number:"
  print =<< replicateM 100 (countTillHit primeMR 1)

  putStrLn "\x1b[36m== Assignment 6B (Finding Mersenne primes) ==\x1b[0m"
  putStrLn "Using the inefficient but reliable `prime` function:"
  print $ findMersennePrimesSimple 8
  putStrLn "\nUsing Miller-Rabin:"
  print =<< findMersennePrimes primeMR 1 2500
  putStrLn "\nUsing fermat:"
  print =<< findMersennePrimes primeTestsF 1 2500
