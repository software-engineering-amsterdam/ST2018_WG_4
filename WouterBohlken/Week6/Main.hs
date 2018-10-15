module Main where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.String
import Data.Time
import Debug.Trace
import Data.List
import System.Random
import Test.QuickCheck
import Lecture6
import Control.Monad

-- Exercise 1
-- See Lecture6.hs

-- Exercise 2
-- compareExM :: IO Float
-- compareExM = do
--               let aStart = getCurrentTime
--               expM 56 46472344 6
--               let aTime = getCurrentTime - aStart
--               let bStart = getCurrentTime
--               Main.exM 56 46472344 6
--               let bTime = getCurrentTime - bStart
--               return bTime / aTime


-- Exercise 3

composites :: [Integer]
composites = filter (\x -> factors x /= [x]) [2..]


-- Exercise 4

testPrimeF :: Integer -> IO Bool
testPrimeF n = do
                fPrime <- primeTestF n
                let actualPrime = prime n
                return (fPrime == actualPrime)

testPrimesF :: [Integer] ->IO ()
testPrimesF [] = putStrLn ""
testPrimesF (x:xs) = do
                      isPrime <- testPrimeF x
                      unless isPrime $ putStrLn ("Fooled primeTestF with: " ++ show x)
                      testPrimesF xs

testCompositePrimes :: Int -> IO ()
testCompositePrimes n = testPrimesF (take n composites)


-- Exercise 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
       k <- [2..],
       prime (6*k+1),
       prime (12*k+1),
       prime (18*k+1) ]


testCarmichaelF :: Int -> IO ()
testCarmichaelF n = testPrimesF (take n carmichael)


-- Exercise 6

testPrimeMR :: Integer -> Integer -> IO Bool
testPrimeMR k n = do
                fPrime <- primeMR k n
                let actualPrime = prime n
                return (fPrime == actualPrime)

testPrimesMR :: Integer -> [Integer] ->IO ()
testPrimesMR k [] = putStrLn ""
testPrimesMR k (x:xs) = do
                      isPrime <- testPrimeMR k x
                      unless isPrime $ putStrLn ("Fooled primeTestMR with: " ++ show x)
                      testPrimesMR k xs


testCarmichaelMR :: Integer -> Int -> IO ()
testCarmichaelMR k n = testPrimesMR k (take n carmichael)


-- Exercise 6 (2)

isMersennePrime :: Integer -> IO Bool
isMersennePrime n = primeMR 2 ((2 ^ n) - 1)

mersennePrimes :: [Integer] -> IO ()
mersennePrimes (x:xs) = do
                            isMers <- isMersennePrime x
                            when isMers $ print x
                            mersennePrimes xs

{-
Running this function for 1 minute yieldes the following output:
3
5
7
13
17
19
31
61
89
107
127
521
607
1279
2203
2281
3217
4253

I checked the numbers on https://www.mersenne.org/primes/ and they all exist
-}

main :: IO ()
main = do
    putStrLn "+++ Assignment 1 +++"
