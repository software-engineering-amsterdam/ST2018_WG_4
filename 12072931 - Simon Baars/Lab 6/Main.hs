module Main where

import System.Random
import Test.QuickCheck
import Control.Monad
import Debug.Trace
import Data.List

-- Assignment 1
toBin :: Integer -> Integer
toBin 0 = 0
toBin x = 10 * toBin (x `div` 2) + x `mod` 2

listFoldl :: ([a]->a) -> [a] -> [a]
listFoldl f acc = let newAcc = f acc in acc ++ listFoldl f [newAcc]

exM :: Integer -> Integer -> Integer -> Integer
exM x y n = foldr (\i acc -> acc * snd (snd i) `mod` n) 1 (filter (\i -> fst i == '1')
            (zip (reverse . show $ toBin y) (listFoldl (\acc -> (fst (head acc) * 2, snd (head acc)^2)) [(1,x)]))) `mod` n

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 1 (Modular Exponentiation) ==\x1b[0m"
  print $ exM 3 200 50
