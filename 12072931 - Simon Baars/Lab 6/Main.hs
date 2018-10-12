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
-- Please see the Lecture6.hs file for the code of the exM function.
-- Result:
--                    +++ OK, passed 100 tests.
testExM :: (Integer,Integer,Integer) -> Bool
testExM (x,y,n) = exM x y n == expM x y n

-- Assignment 2
-- Time: 180 minutes
-- Result:
--
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
--                    100 times doing till fermat is fooled (3 checks max per number):
--                    [1105,1729,1105,1729,1105,1729,1729,561,561,2465,561,15841,2465,1729,561,2465,2821,1105,1105,2821,1105,6601,561,2821,2465,561,1729,1729,1105,2821,6601,1105,2465,1105,1729,6601,66
--                    01,1105,1105,561,1729,1105,561,1045,561,6601,6601,1729,1729,6601,6601,29341,2465,1729,2821,1105,1105,561,1105,561,561,1105,1105,1105,561,1105,561,1105,561,561,561,1105,2821,561,1
--                    105,1729,1105,561,10585,2465,8911,2821,5565,1729,1729,561,1105,561,1729,1105,2465,1729,2821,2821,1105,1729,2465,1729,2465,2465]
-- Remark: These are all carmicheal numbers
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
--                    200 times showing primality for carmicheal numbers (amount of tries using fermat is shown):
--                    [2,1,2,1,2,1,1,1,2,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,2,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,3,1,4,5,2,2,2,1,1,2,1,1,1,1,2,1,1,1,2,1,1,1,1,1,1,1,2,2,1,2,1,1,2,1,1,1,2,2
--                    ,1,1,1,1,2,1,2,1,2,2,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,2,2,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,1,3,1,2,1,1,1,1,1
--                    ,1,1,1,1,2,1,2,1,1,1,1,3,2,1,2,1,1,1,1,1,1,1]
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
     k <- [1..],
     prime (6*k+1),
     prime (12*k+1),
     prime (18*k+1) ]

countTillHit :: Int -> IO Integer
countTillHit index = let carM = drop index carmichael in firstM (\x -> primeTestF (head carM)) [1..] >>= \x -> return (fromJust x)

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

  putStrLn "\x1b[36m== Assignment 5 (Amount of attempts till carmichael number is hit) ==\x1b[0m"
  putStrLn "100 times showing primality for carmicheal numbers (amount of tries using fermat is shown):"
  print =<< replicateM 100 (countTillHit 0)
