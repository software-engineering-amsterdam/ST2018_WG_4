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

-- Assignment 1

exM' :: Integer -> Integer -> Integer -> Integer -> Integer
exM' base 0     modulus result = result
exM' base expon modulus result | expon `mod` 2 == 1 = exM' newBase newExpon modulus ((result * base) `mod` modulus)
                               | otherwise = exM' newBase newExpon modulus result
                              where newBase = (base ^ 2) `mod` modulus
                                    newExpon = shiftR expon 1

-- Base on the 'Right-to-left binary method' pseudocode in https://en.wikipedia.org/wiki/Modular_exponentiation
exM :: Integer -> Integer -> Integer -> Integer
exM base expon 1 = 0
exM base expon modulus = exM' (base `mod` modulus) expon modulus 1

-- Assignment 2
-- compareExM :: IO Float
-- compareExM = do
--               let aStart = getCurrentTime
--               expM 56 46472344 6
--               let aTime = getCurrentTime - aStart
--               let bStart = getCurrentTime
--               Main.exM 56 46472344 6
--               let bTime = getCurrentTime - bStart
--               return bTime / aTime

-- Assignment 3

composites :: [Integer]
composites = filter (\x -> factors x /= [x]) [2..]



main :: IO ()
main = do
    putStrLn "+++ Assignment 1 +++"
