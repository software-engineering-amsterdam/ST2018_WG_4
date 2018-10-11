module Main where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.String
import Debug.Trace
import Data.List
import Numeric
import System.Random
import Test.QuickCheck
import Lecture6


-- int2bin :: Integer -> String
-- int2bin i = showIntAtBase 2 intToDigit i ""

exM' :: Integer -> Integer -> Integer -> Integer -> Integer
exM' base 0     modulus result = result
exM' base expon modulus result | expon `mod` 2 == 1 = exM' ((base ^ 2) `mod` modulus) (shiftR expon 1) modulus ((result * base) `mod` modulus)
                               | otherwise = exM' ((base ^ 2) `mod` modulus) (shiftR expon 1) modulus result

exM :: Integer -> Integer -> Integer -> Integer
exM base expon 1 = 0
exM base expon modulus = exM' (base `mod` modulus) expon modulus 1

main :: IO ()
main = do
    putStrLn "+++ Assignment 1 +++"
