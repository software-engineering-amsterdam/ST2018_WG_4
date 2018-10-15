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
--                    exM 6 6 2 took -417 nanoseconds less than 6^6 `mod` 2.
--                    exM 7 3 8 took -455 nanoseconds less than 7^3 `mod` 8.
--                    exM 3 5 4 took -522 nanoseconds less than 3^5 `mod` 4.
--                    exM 3 8 5 took -387 nanoseconds less than 3^8 `mod` 5.
--                    exM 1 1 4 took -289 nanoseconds less than 1^1 `mod` 4.
--                    exM 1 1 3 took -407 nanoseconds less than 1^1 `mod` 3.
--                    exM 5 5 1 took 855 nanoseconds less than 5^5 `mod` 1.
--                    exM 7 6 7 took -517 nanoseconds less than 7^6 `mod` 7.
--                    exM 2 6 6 took -586 nanoseconds less than 2^6 `mod` 6.
--                    exM 3 13 9 took -619 nanoseconds less than 3^13 `mod` 9.
--                    exM 11 16 6 took -598 nanoseconds less than 11^16 `mod` 6.
--                    exM 6 9 8 took -631 nanoseconds less than 6^9 `mod` 8.
--                    exM 13 12 1 took 665 nanoseconds less than 13^12 `mod` 1.
--                    exM 2 14 4 took -551 nanoseconds less than 2^14 `mod` 4.
--                    exM 9 8 8 took -512 nanoseconds less than 9^8 `mod` 8.
--                    exM 6 11 12 took -362 nanoseconds less than 6^11 `mod` 12.
--                    exM 9 14 7 took -682 nanoseconds less than 9^14 `mod` 7.
--                    exM 11 5 9 took -453 nanoseconds less than 11^5 `mod` 9.
--                    exM 10 20 11 took -119 nanoseconds less than 10^20 `mod` 11.
--                    exM 7 4 14 took -433 nanoseconds less than 7^4 `mod` 14.
--                    exM 4 14 11 took -425 nanoseconds less than 4^14 `mod` 11.
--                    exM 26 28 13 took 398 nanoseconds less than 26^28 `mod` 13.
--                    exM 23 25 26 took -106 nanoseconds less than 23^25 `mod` 26.
--                    exM 19 23 26 took -268 nanoseconds less than 19^23 `mod` 26.
--                    exM 39 14 30 took -356 nanoseconds less than 39^14 `mod` 30.
--                    exM 11 18 6 took -809 nanoseconds less than 11^18 `mod` 6.
--                    exM 37 17 40 took 29 nanoseconds less than 37^17 `mod` 40.
--                    exM 35 9 21 took -556 nanoseconds less than 35^9 `mod` 21.
--                    exM 22 31 6 took 213 nanoseconds less than 22^31 `mod` 6.
--                    exM 23 18 31 took -288 nanoseconds less than 23^18 `mod` 31.
--                    exM 40 14 20 took -313 nanoseconds less than 40^14 `mod` 20.
--                    exM 2 32 25 took -607 nanoseconds less than 2^32 `mod` 25.
--                    exM 27 30 17 took 27 nanoseconds less than 27^30 `mod` 17.
--                    exM 31 31 7 took -31 nanoseconds less than 31^31 `mod` 7.
--                    exM 37 27 14 took -62 nanoseconds less than 37^27 `mod` 14.
--                    exM 8 9 5 took -467 nanoseconds less than 8^9 `mod` 5.
--                    exM 36 39 18 took 365 nanoseconds less than 36^39 `mod` 18.
--                    exM 5 7 2 took -470 nanoseconds less than 5^7 `mod` 2.
--                    exM 45 17 19 took -134 nanoseconds less than 45^17 `mod` 19.
--                    exM 8 35 37 took -352 nanoseconds less than 8^35 `mod` 37.
--                    exM 38 15 7 took -566 nanoseconds less than 38^15 `mod` 7.
--                    exM 8 28 43 took -436 nanoseconds less than 8^28 `mod` 43.
--                    exM 18 24 14 took -278 nanoseconds less than 18^24 `mod` 14.
--                    exM 33 6 22 took -666 nanoseconds less than 33^6 `mod` 22.
--                    exM 31 25 16 took -273 nanoseconds less than 31^25 `mod` 16.
--                    exM 43 28 21 took 250 nanoseconds less than 43^28 `mod` 21.
--                    exM 43 1 9 took -143 nanoseconds less than 43^1 `mod` 9.
--                    exM 30 17 23 took 75 nanoseconds less than 30^17 `mod` 23.
--                    exM 11 53 7 took 56 nanoseconds less than 11^53 `mod` 7.
--                    exM 15 11 18 took -269 nanoseconds less than 15^11 `mod` 18.
--                    exM 44 32 61 took -86 nanoseconds less than 44^32 `mod` 61.
--                    exM 13 41 30 took -424 nanoseconds less than 13^41 `mod` 30.
--                    exM 28 44 36 took -248 nanoseconds less than 28^44 `mod` 36.
--                    exM 39 36 8 took -262 nanoseconds less than 39^36 `mod` 8.
--                    exM 52 42 2 took 120 nanoseconds less than 52^42 `mod` 2.
--                    exM 30 23 19 took -436 nanoseconds less than 30^23 `mod` 19.
--                    exM 37 8 46 took -501 nanoseconds less than 37^8 `mod` 46.
--                    exM 34 8 24 took -511 nanoseconds less than 34^8 `mod` 24.
--                    exM 39 22 76 took -149 nanoseconds less than 39^22 `mod` 76.
--                    exM 27 26 36 took -319 nanoseconds less than 27^26 `mod` 36.
--                    exM 58 18 20 took -215 nanoseconds less than 58^18 `mod` 20.
--                    exM 9 20 59 took -112 nanoseconds less than 9^20 `mod` 59.
--                    exM 18 21 45 took -321 nanoseconds less than 18^21 `mod` 45.
--                    exM 76 23 19 took -37 nanoseconds less than 76^23 `mod` 19.
--                    exM 59 33 42 took 189 nanoseconds less than 59^33 `mod` 42.
--                    exM 63 61 27 took 437 nanoseconds less than 63^61 `mod` 27.
--                    exM 20 24 60 took -266 nanoseconds less than 20^24 `mod` 60.
--                    exM 30 104 20 took 681 nanoseconds less than 30^104 `mod` 20.
--                    exM 23 64 9 took -2 nanoseconds less than 23^64 `mod` 9.
--                    exM 9 1 43 took -423 nanoseconds less than 9^1 `mod` 43.
--                    exM 34 35 22 took -36 nanoseconds less than 34^35 `mod` 22.
--                    exM 14 61 6 took -200 nanoseconds less than 14^61 `mod` 6.
--                    exM 53 19 24 took -288 nanoseconds less than 53^19 `mod` 24.
--                    exM 24 39 4 took 177 nanoseconds less than 24^39 `mod` 4.
--                    exM 65 53 75 took -13 nanoseconds less than 65^53 `mod` 75.
--                    exM 38 32 17 took 57 nanoseconds less than 38^32 `mod` 17.
--                    exM 9 77 34 took -199 nanoseconds less than 9^77 `mod` 34.
--                    exM 35 1 71 took -276 nanoseconds less than 35^1 `mod` 71.
--                    exM 35 72 52 took 155 nanoseconds less than 35^72 `mod` 52.
--                    exM 64 26 76 took 25 nanoseconds less than 64^26 `mod` 76.
--                    exM 6 55 23 took -269 nanoseconds less than 6^55 `mod` 23.
--                    exM 58 45 29 took 151 nanoseconds less than 58^45 `mod` 29.
--                    exM 23 64 44 took 113 nanoseconds less than 23^64 `mod` 44.
--                    exM 41 10 66 took -14731 nanoseconds less than 41^10 `mod` 66.
--                    exM 32 27 39 took -194 nanoseconds less than 32^27 `mod` 39.
--                    exM 56 16 40 took -368 nanoseconds less than 56^16 `mod` 40.
--                    exM 75 15 50 took -316 nanoseconds less than 75^15 `mod` 50.
--                    exM 1 75 16 took -555 nanoseconds less than 1^75 `mod` 16.
--                    exM 67 23 27 took -70 nanoseconds less than 67^23 `mod` 27.
--                    exM 65 1 2 took -397 nanoseconds less than 65^1 `mod` 2.
--                    exM 17 52 58 took 369 nanoseconds less than 17^52 `mod` 58.
--                    exM 92 87 92 took 748 nanoseconds less than 92^87 `mod` 92.
--                    exM 8 38 94 took -272 nanoseconds less than 8^38 `mod` 94.
--                    exM 1 44 90 took -590 nanoseconds less than 1^44 `mod` 90.
--                    exM 61 63 70 took 447 nanoseconds less than 61^63 `mod` 70.
--                    exM 50 51 95 took 114 nanoseconds less than 50^51 `mod` 95.
--                    exM 100 48 70 took -5 nanoseconds less than 100^48 `mod` 70.
--                    exM 67 87 46 took -12 nanoseconds less than 67^87 `mod` 46.
--                    exM 79 81 23 took 172 nanoseconds less than 79^81 `mod` 23.
--                    exM 2 37 4 took -538 nanoseconds less than 2^37 `mod` 4.
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
