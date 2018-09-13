module Main where
import Lab2
import System.Random
import Test.QuickCheck
import System.IO.Unsafe

-- Assignment 1
-- Time: 180 minutes
-- Result: getAmountInQuartile 10000 --> [2454,2486,2523,2537]
-- Result: testQuartilesSafe 10000 >>= \y -> checkTestResult y --> Test succeeded!
-- Result: quickCheckResult $ forAll genBigNumbers testQuartiles --> +++ OK, passed 100 tests.
calcQuartile :: [Float] -> [Float] -> [Int]
calcQuartile randomList (x1:x2:rest) = length(filter (\x -> x>=x1 && x<x2) randomList):calcQuartile randomList (x2:rest)
calcQuartile x y = []

getAmountInQuartile :: Int -> IO [Int]
getAmountInQuartile x = probs x >>= \y -> return (calcQuartile y [0, 0.25..1])

withinPercentage :: Int -> Int -> Int -> Bool
withinPercentage number percentage ofNumber = number>=ofNumber-p-1 && number<=ofNumber+p+1
    where p = ofNumber `div` 100 * percentage

testQuartiles :: Int -> Bool
testQuartiles x =  (x>100) --> all (\z -> withinPercentage z 25 (x `div` 4)) (unsafePerformIO(getAmountInQuartile x))

testQuartilesSafe :: Int -> IO Bool
testQuartilesSafe x = getAmountInQuartile x >>= \y -> return ((x>0) --> all (\z -> withinPercentage z 10 (x `div` 4)) y)

genBigNumbers :: Gen Int -- Generator for QuickCheck because it would otherwise pick such big numbers that the program would hang for a long time.
genBigNumbers = abs `fmap` (arbitrary :: Gen Int) `suchThat` (>1000)


-- Assignment 2
-- Time: 50 minutes.
-- To test assignment 2, I tested the following cases:
--      Testing NoTriangle by checking if different types of invalid triangles result in NoTriangle (making sure I hit all parts of the NoTriangle pattern's condition):
  --            triangle 90 10 10 --> NoTriangle
--            triangle 10 90 10 --> NoTriangle
--            triangle 10 10 90 --> NoTriangle
--
--      Testing Equilateral by trying different kinds of cases that should return in Equilateral:
--            triangle 10 10 10 --> Equilateral
--            triangle 100 100 100 --> Equilateral
--            triangle 1000 1000 1000 --> Equilateral
--
--      Testing Rectangular by trying different kinds of cases that should return in Rectangular (making sure I hit all parts of the Rectangular pattern's condition):
--            triangle 3 4 5 --> Rectangular
--            triangle 5 3 4 --> Rectangular
--            triangle 4 5 3 --> Rectangular

isPythagorean :: Integer -> Integer -> Integer -> Bool
isPythagorean a b c = a^2 + b^2 == c^2

triangleType :: (Integer, Integer, Integer) -> String
triangleType (x,y,z)
  | (x+y)<z || (y+z)<x || (z+x)<y = "Not a triangle"
  | x==y && y==z && z==x = "Equilateral"
  | isPythagorean x y z || isPythagorean y z x || isPythagorean z x y = "Rectangular"
  | x==y || y==z || z==x = "Isosceles"
  | otherwise = "Other"

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = case triangleType (x,y,z) of "Not a triangle" -> NoTriangle
                                              "Equilateral" -> Equilateral
                                              "Rectangular" -> Rectangular
                                              "Isosceles" -> Isosceles
                                              _ -> Other

checkTestResult :: Bool -> String
checkTestResult True  = "Test succeeded!"
checkTestResult False = "Test failed!"

main :: IO ()
main = do
  putStrLn "== Assignment 1 =="
  testQuartilesSafe 10000 >>= \y -> putStrLn("Safe, but without QuickCheck: " ++ checkTestResult y)
  quickCheckResult $ forAll genBigNumbers testQuartiles

  putStrLn "\n== Assignment 1 =="
  putStrLn $ triangleType (100,41,39)

  putStrLn "Done"
