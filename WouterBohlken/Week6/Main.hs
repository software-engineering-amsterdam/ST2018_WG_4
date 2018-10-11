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
-- See Lecture6

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

testPrimeMR :: Integer -> IO Bool
testPrimeMR n = do
                fPrime <- primeMR 2 n
                let actualPrime = prime n
                return (fPrime == actualPrime)

testPrimesMR :: [Integer] ->IO ()
testPrimesMR [] = putStrLn ""
testPrimesMR (x:xs) = do
                      isPrime <- testPrimeMR x
                      unless isPrime $ putStrLn ("Fooled primeTestMR with: " ++ show x)
                      testPrimesMR xs


testCarmichaelMR :: Int -> IO ()
testCarmichaelMR n = testPrimesMR (take n carmichael)

main :: IO ()
main = do
    putStrLn "+++ Assignment 1 +++"
