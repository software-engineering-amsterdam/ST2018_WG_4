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
testExM :: (Integer,Integer,Integer) -> Bool
testExM (x,y,n) = exM x y n == expM x y n

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
composites :: [Integer]
composites = filter (\x -> length (factors x) > 2) [0..]

-- Assignment 4
findTestFool :: Int -> IO Integer
findTestFool k = firstM (\x -> primeTestsF k x >>= \y -> return(prime x /= y)) composites >>= \x -> return (fromJust x)

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 1 (Modular Exponentiation) ==\x1b[0m"
  quickCheck $ forAll genPos testExM

  putStrLn "\x1b[36m== Assignment 2 (Performance Testing) ==\x1b[0m"
  quickCheck $ forAll genPos testPerformance

  putStrLn "\x1b[36m== Assignment 3 (Composite natural numbers) ==\x1b[0m"
  putStrLn $ "These are the composite natural numbers between 1 and 100: " ++ show (takeWhile (<100) composites)

  putStrLn "\x1b[36m== Assignment 4 (Fermat primality check) ==\x1b[0m"
  print =<< findTestFool 3
