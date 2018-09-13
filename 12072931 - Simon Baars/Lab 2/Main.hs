module Main where
import Lab2
import System.Random
import Test.QuickCheck

-- Assignment 1
-- Time: 120 minutes
calcQuartile :: [Float] -> [Float] -> [Int]
calcQuartile randomList (x1:x2:rest) = length(filter (\x -> x>=x1 && x<x2) randomList):calcQuartile randomList (x2:rest)
calcQuartile x y = []

getAmountInQuartile :: Int -> IO [Int]
getAmountInQuartile x = probs x >>= \y -> return (calcQuartile y [0, 0.25..1])

withinPercentage :: Int -> Int -> Int -> Bool
withinPercentage number percentage ofNumber = number>=ofNumber-p && number<=ofNumber+p
    where p = ofNumber `div` 100 * percentage

testQuartiles :: Int -> IO Bool
testQuartiles x = getAmountInQuartile x >>= \y -> return ((x>0) --> all (\z -> withinPercentage z 10 (x `div` 4)) y)

-- Assignment 2

allTriple :: (Integer, Integer, Integer) -> (Integer -> Bool) -> Bool
allTriple (a1, a2, a3) f = f a1 && f a2 && f a3

triangleType :: (Integer, Integer, Integer) -> String
triangleType (x,y,z)
  | x+y+z /= 180 = "Not a triangle"
  | allTriple (x,y,z) (==60) = "Equilateral"
  | allTriple (x,y,z) (\x -> x == 90 || x == 45) = "Rectangular"
  | (x == y) || (y==z) || (z==x) = "Isosceles"
  | otherwise = "Other"

checkTestResult :: Bool -> String
checkTestResult True  = "Test succeeded!"
checkTestResult False = "Test failed!"

main :: IO ()
main = do
  putStrLn "== Assignment 1 =="
  testQuartiles 10000 >>= \y -> putStrLn(checkTestResult y)

  putStrLn "\n== Assignment 1 =="
  putStrLn $ triangleType (100,41,39)

  putStrLn "Done"
