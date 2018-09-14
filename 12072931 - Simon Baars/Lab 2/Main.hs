module Main where
import Lab2
import Lecture2
import System.Random
import Test.QuickCheck
import System.IO.Unsafe

-- Assignment 1 (Random floating point numbers)
-- Time: 190 minutes
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


-- Assignment 2 (Recognizing triangles)
-- Time: 50 minutes.
-- To test assignment 2, I tested the following cases:
--      Testing NoTriangle by checking if different types of invalid triangles result in NoTriangle (making sure I hit all parts of the NoTriangle pattern's condition):
  --            triangle 90 10 10 --> NoTriangle
--            triangle 10 90 10 --> NoTriangle
--            triangle 10 10 90 --> NoTriangle
--
--      Testing Equilateral by trying different kinds of cases that should return in Equilateral:
--            triangle 10 10 10 --> Equilateral
--
--      Testing Rectangular by trying different kinds of cases that should return in Rectangular (making sure I hit all parts of the Rectangular pattern's condition):
--            triangle 3 4 5 --> Rectangular
--            triangle 5 3 4 --> Rectangular
--            triangle 4 5 3 --> Rectangular
--
--      Testing Isosceles by trying different kinds of cases that should return in Isosceles (making sure I hit all parts of the Isosceles pattern's condition):
--            triangle 20 15 15 --> Isosceles
--            triangle 15 20 15 --> Isosceles
--            triangle 15 15 20 --> Isosceles
--
--      Testing Other by trying different kinds of cases that should return in Isosceles (making sure I hit all parts of the Isosceles pattern's condition):
--            triangle 10 11 12 --> Other
--            triangle 12 10 11 --> Other
--            triangle 11 12 10 --> Other
--
-- All these properties are being tested for by the function `testTriangle`. This is the result of running that function:
--      Tests for shape NoTriangle succeeded for input variables 90, 10, 10
--      Tests for shape Equilateral succeeded for input variables 10, 10, 10
--      Tests for shape Rectangular succeeded for input variables 3, 4, 5
--      Tests for shape Isosceles succeeded for input variables 20, 15, 15
--      Tests for shape Other succeeded for input variables 10, 11, 12

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

checkTriangleFunction :: Integer -> Integer -> Integer -> Shape -> IO ()
checkTriangleFunction x y z expectedShape = if (triangle x y z == expectedShape) && (triangle z x y == expectedShape) && (triangle y z x == expectedShape)
  then putStrLn("Tests for shape " ++ show expectedShape ++ " succeeded for input variables "++show x++", "++show y++", "++show z)
  else error("Tests for shape " ++ show expectedShape ++ " FAILED for input variables "++show x++", "++show y++", "++show z)

testTriangle :: IO ()
testTriangle = do
  checkTriangleFunction 90 10 10 NoTriangle
  checkTriangleFunction 10 10 10 Equilateral
  checkTriangleFunction 3 4 5 Rectangular
  checkTriangleFunction 20 15 15 Isosceles
  checkTriangleFunction 10 11 12 Other

-- Assignment 3 (Testing properties strength)
-- Time: 60 minutes
-- Result: [p1,p3,p4,p5,p2]

evenAndMoreThanThree, evenOrMoreThanThree, evenAndMoreThanThreeOrEvenRight, evenAndMoreThanThreeOrEvenLeft :: Int -> Bool
evenAndMoreThanThree y = stronger [-10..10] (\ x -> even x && x > 3) even
evenOrMoreThanThree y = stronger [-10..10] (\ x -> even x || x > 3) even
evenAndMoreThanThreeOrEvenRight y = stronger [-10..10] (\ x -> (even x && x > 3) || even x) even
evenAndMoreThanThreeOrEvenLeft y = stronger [-10..10] even (\ x -> (even x && x > 3) || even x)

p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x
p4 x = (even x && x > 3) || even x

domain = [-10..10]

data Prop a = Prop { name :: String, propertyFunction :: Int -> Bool}
instance Show (Prop a) where show = name
instance Eq (Prop a) where y == z = name y == name z
instance Ord (Prop a) where
  compare Prop { propertyFunction = x } Prop { propertyFunction = y }
    | stronger domain y x = GT
    | weaker domain y x = LT
    | otherwise = EQ
properties = [Prop "p1" p1, Prop "p2" p2, Prop "p3" p3, Prop "p4" p4, Prop "p5" even]

-- Assignment 4 (Recognizing Permutations)

checkTestResult :: Bool -> String
checkTestResult True  = "Test succeeded!"
checkTestResult False = "Test failed!"

main :: IO ()
main = do
  putStrLn "== Assignment 1 (Random floating point numbers) =="
  testQuartilesSafe 10000 >>= \y -> putStrLn("Safe, but without QuickCheck: " ++ checkTestResult y)
  quickCheckResult $ forAll genBigNumbers testQuartiles

  putStrLn "\n== Assignment 2 (Recognizing triangles) =="
  testTriangle

  putStrLn "\n== Assignment 3 (Testing properties strength) =="
  print $ quicksort properties

  putStrLn "\n== Assignment 4 (Recognizing Permutations) =="


  putStrLn "Done"
