module Main where

import System.Random
import Test.QuickCheck
import Control.Monad
import Debug.Trace
import Data.List
import Control.Exception
import System.Clock
import Test.QuickCheck.Monadic

-- Assignment 1
toBin :: Integer -> Integer
toBin 0 = 0
toBin x = 10 * toBin (x `div` 2) + x `mod` 2

listFoldl :: ([a]->a) -> [a] -> [a]
listFoldl f acc = let newAcc = f acc in acc ++ listFoldl f [newAcc]

exM :: Integer -> Integer -> Integer -> Integer
exM x y n = foldr (\i acc -> acc * snd (snd i) `mod` n) 1 (filter (\i -> fst i == '1')
            (zip (reverse . show $ toBin y) (listFoldl (\acc -> (fst (head acc) * 2, snd (head acc)^2)) [(1,x)]))) `mod` n

testExM :: (Integer,Integer,Integer) -> Bool
testExM (x,y,n) = exM x y n == x^y `mod` n

-- Assignment 2
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
factors :: Integer -> [Integer]
factors y = [ x | x <- [1..y], y `mod` x == 0]

composites :: [Integer]
composites = filter (\x -> length (factors x) > 2) [0..]

-- Assignment 4

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 1 (Modular Exponentiation) ==\x1b[0m"
  quickCheck $ forAll genPos testExM

  putStrLn "\x1b[36m== Assignment 2 (Performance Testing) ==\x1b[0m"
  quickCheck $ forAll genPos testPerformance

  putStrLn "\x1b[36m== Assignment 3 (Composite natural numbers) ==\x1b[0m"
  putStrLn $ "These are the composite natural numbers between 1 and 100: " ++ show (takeWhile (<100) composites)

  putStrLn "\x1b[36m== Assignment 4 (Fermat primality check) ==\x1b[0m"
