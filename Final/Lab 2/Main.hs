module Main where
import Lab2
import Lecture2
import System.Random
import Test.QuickCheck
import System.IO.Unsafe
import Data.List
import Data.Char

-- Assignment 1 (Random floating point numbers)
-- Time: 190 minutes
-- About my solution: A lot of people have been struggling with the fact that the `probs` function return an IO datatype, including me. After a while I found that the `>>=` operator could solve this problem. However, using this operator the return value is always an IO datatype. This is fine, except for QuickCheck, as it doesn't accept IO datatype output (a function with `IO Bool` output instead of `Bool`). Because of this, I created 2 tests: a "safe" test and an "unsafe" QuickCheck. The safe test uses a fixed input number (10.000) to test this function. For QuickCheck I have used `unsafePerformIO` from the `System.IO.Unsafe` module to be able to feed this function to QuickCheck. This way I have proven both the application of a safe way to test this function and a way to automate the test process of this function. For the automated way I only test on big (>1000) numbers to get statistically relevant results.
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
-- Time: 90 minutes
-- Result: print . reverse $ quicksort permutationProperties --> [p2,p3,p4,p1]

p1, p2, p3 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x

domain = [-10..10]

data Prop a = Prop { name :: String, propertyFunction :: Int -> Bool}
instance Show (Prop a) where show = name
instance Eq (Prop a) where y == z = name y == name z
instance Ord (Prop a) where
  compare Prop { propertyFunction = x } Prop { propertyFunction = y }
    | stronger domain x y = GT
    | weaker domain x y = LT
    | otherwise = EQ
properties = [Prop "p1" p1, Prop "p2" p2, Prop "p3" p3, Prop "p4" even]

-- Assignment 4 (Recognizing Permutations)
-- Time: 120 minutes
-- Result: [Bigger,Increased,Smaller,Swapped,Rotated,Reversed]
-- Executed tests:
--        Testing same list: Tests `isPermutation [1,2,3] [1,2,3]`, which is expected to return True.
--        Testing all rotated lists: Tests `isPermutation [1,2,3] [2,3,1]` and `isPermutation [1,2,3] [3,1,2]`, which are both expected to return True.
--        Testing reversed list: Tests `isPermutation [1,2,3] [3,2,1]`, which is expected to return True.
--        Testing all swapped lists: Tests `isPermutation [1,2,3] [2,1,3]` and `isPermutation [1,2,3] [1,3,2]`, which are both expected to return True.
--        Testing all smaller lists: Tests `isPermutation [1,2,3] [1,2]` and `isPermutation [1,2,3] [1]`, which are both expected to return False.
--        Testing all bigger lists: Tests `isPermutation [1,2,3] [1,2,3,0]`, which is expected to return False.
--        Testing increased list: Tests `isPermutation [1,2,3] [2,3,4]`, which is expected to return False.
--
-- With these tests all different combinations are tested of cases that should return True and cases that return False for input [1,2,3] and all it's variations.
--
-- Test results (which can be retrieved by running the "testPermutations" function):
--        Testing same list: Test succeeded!
--        Testing all rotated lists: Test succeeded!
--        Testing reversed list: Test succeeded!
--        Testing all swapped lists: Test succeeded!
--        Testing all smaller lists: Test succeeded!
--        Testing all bigger lists: Test succeeded!
--        Testing increased list: Test succeeded!

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = length x == length y && all (\z -> length(filter (==z) x) == length(filter (==z) y)) x

-- Property: Being rotated. Rotated lists will always be a permutation.
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

propRotated :: Eq a => Int -> [a] -> Bool
propRotated rotation origList = isPermutation origList (rotate rotation origList)

-- All rotations. For example [1,2,3] has [3,1,2] and [2,3,1] as rotations.
propAllRotations :: Eq a => [a] -> Bool
propAllRotations origList = not(null origList) --> all (`propRotated` origList) [1..length origList-1]

-- Property: Being reversed. Reversed lists will always be a permutation. For example, [1,2,3] has [3,2,1] as reverse.
propReversed :: Eq a => [a] -> Bool
propReversed origList = isPermutation origList (reverse origList)

-- Property: Having 2 elements swapped. Swapped lists will always be a permutation.
swapAt :: Int -> [a] -> [a]
swapAt index list = take index list ++ head swapWithPos : head swapPos : tail swapWithPos
  where swapPos = drop index list
        swapWithPos = drop 1 swapPos

propSwapped :: Eq a => Int -> [a] -> Bool
propSwapped index origList = isPermutation origList (swapAt index origList)

-- All swapped elements. For example [1,2,3] has [2,1,3] and [1,3,2] as swapped.
propAllSwapped :: Eq a => [a] -> Bool
propAllSwapped origList = (length origList > 1) --> all (`propSwapped` origList) [0..length origList-2]

-- Property: Being smaller. Smaller lists will never be a permutation.
propSmaller :: Eq a => Int -> [a] -> Bool
propSmaller amount origList = isPermutation origList (take amount origList)

-- All smaller. For example [1,2,3] has [1,2] and [1] as smaller lists.
propAllSmaller :: Eq a => [a] -> Bool
propAllSmaller origList = not(null origList) --> all (`propSmaller` origList) [1..length origList-1]

-- Property: Being bigger. Bigger lists will never be a permutation.
propBigger :: [Int] -> Bool
propBigger origList = isPermutation origList (0:origList)

-- Property: Being increased. Increased lists will never be a permutation.
propIncreased :: Int -> [Int] -> Bool
propIncreased amount origList = isPermutation origList (map (+amount) origList)

-- Because the stronger and weaker functions require an (a -> Bool) function to be able to operate upon a domain, this function converts the list operating properties into (Int -> Bool) functions by creating a list of the given size.
propListOfSize :: ([Int] -> Bool) -> Int -> Bool
propListOfSize propFunction size = propFunction [1..size]

permutationProperties = [Prop "Reversed" (propListOfSize propReversed), Prop "Rotated" (propListOfSize propAllRotations), Prop "Swapped" (propListOfSize propAllSwapped), Prop "Smaller" (propListOfSize propAllSmaller), Prop "Bigger" (propListOfSize propBigger), Prop "Increased" (propListOfSize (propIncreased 1))]

permutationTestList = [1,2,3]

testPermutations :: IO ()
testPermutations = do
  putStrLn $ "Testing same list: "++checkTestResult (isPermutation permutationTestList permutationTestList)
  putStrLn $ "Testing all rotated lists: "++checkTestResult (propAllRotations permutationTestList)
  putStrLn $ "Testing reversed list: "++checkTestResult (propReversed permutationTestList)
  putStrLn $ "Testing all swapped lists: "++checkTestResult (propAllSwapped permutationTestList)
  putStrLn $ "Testing all smaller lists: "++checkTestResult (not (propAllSmaller permutationTestList))
  putStrLn $ "Testing all bigger lists: "++checkTestResult (not (propBigger permutationTestList))
  putStrLn $ "Testing increased list: "++checkTestResult (not (propIncreased 1 permutationTestList))

-- Assignment 5 (Recognizing and generating derangements)
-- Time: 90 minutes
-- Result: [Rotated,Smaller,Reversed,Swapped,Increased,Bigger]
-- Executed tests (both with automated and manual input). Down here, the manual tests are documented. The automated tests do kinda the same, but then with automated input (down here the input [1,2,3] is used):
--        Testing same list: Tests `isDerangement [1,2,3] [1,2,3]`, which is expected to return False.
--        Testing all rotated lists: Tests `isDerangement [1,2,3] [2,3,1]` and `isDerangement [1,2,3] [3,1,2]`, which are both expected to return True.
--        Testing reversed list on even lists: Tests `isDerangement [1,2,3,4] [4,3,2,1]`, which is expected to return True.
--        Testing all swapped lists on even lists: Tests `isDerangement [1,2,3,4] [2,1,4,3]`, which is expected to return True.
--        Testing reversed list on uneven lists: Tests `isDerangement [1,2,3] [3,2,1]`, which is expected to return False.
--        Testing all swapped lists on uneven lists: Tests `isDerangement [1,2,3] [2,1,3]`, which is expected to return False.
--        Testing all smaller lists: Tests `isDerangement [1,2,3] [1,2]` and `isDerangement [1,2,3] [1]`, which are both expected to return False.
--        Testing all bigger lists: Tests `isDerangement [1,2,3] [1,2,3,0]`, which is expected to return False.
--        Testing increased list: Tests `isDerangement [1,2,3] [2,3,4]`, which is expected to return False.
--
-- Result additional tests (which can be retrieved by running the "testDerangements" function):
--        Running automated testcases.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--
--        Running manual testcases.
--        Testing same list: Test succeeded!
--        Testing all rotated lists: Test succeeded!
--        Testing reversed list on even lists: Test succeeded!
--        Testing all swapped lists on even lists: Test succeeded!
--        Testing reversed list on uneven lists: Test succeeded!
--        Testing all swapped lists on uneven lists: Test succeeded!
--        Testing all smaller lists: Test succeeded!
--        Testing all bigger lists: Test succeeded!
--        Testing increased list: Test succeeded!

isDerangement :: [Int] -> [Int] -> Bool
isDerangement x y = length x == length y && isPermutation x y && all (uncurry (/=)) (zip x y)

deran :: [Int] -> [[Int]]
deran x = filter (`isDerangement` x) (permutations x)

-- Property: Being rotated. Rotated lists will always be a derangement.
propDeranRotated :: Int -> [Int] -> Bool
propDeranRotated rotation origList = isDerangement origList (rotate rotation origList)

-- All derangement rotations. For example [1,2,3] has [3,1,2] and [2,3,1] as rotations, which are all derangements.
propAllDeranRotations :: [Int] -> Bool
propAllDeranRotations origList = not(null origList) && allDifferent origList --> all (`propDeranRotated` origList) [1..length origList-1]

-- Property: Being reversed. Reversed lists will be a derangement half of the time. For example, [1,2,3] has [3,2,1] as reverse, which is not a derangement. However, [1,2,3,4] has [4,3,2,1] as a reverse, which is actually a derangement.
propDeranReversed :: [Int] -> Bool
propDeranReversed origList = allDifferent origList --> isDerangement origList (reverse origList)

-- Property: Having every 2 elements swapped. Swapped lists will be a derangement half of the time. For example, [1,2,3] has [2,1,3] as swapped list, which is not a derangement. However, [1,2,3,4] has [2,1,4,3] as swapped list, which is actually a derangement.
propDeranSwapped :: [Int] -> Bool
propDeranSwapped origList = allDifferent origList --> isDerangement origList (foldr swapAt origList [0,2..length origList-2])

-- Property: Being smaller. Smaller lists will never be a derangement.
propDeranSmaller :: Int -> [Int] -> Bool
propDeranSmaller amount origList = isDerangement origList (take amount origList)

-- All smaller. For example [1,2,3] has [1,2] and [1] as smaller lists.
propAllDeranSmaller :: [Int] -> Bool
propAllDeranSmaller origList = not(null origList) --> all (`propDeranSmaller` origList) [1..length origList-1]

-- Property: Being bigger. Bigger lists will never be a derangement.
propDeranBigger :: [Int] -> Bool
propDeranBigger origList = isDerangement origList (0:origList)

-- Property: Being increased. Increased lists will never be a derangement.
propDeranIncreased :: Int -> [Int] -> Bool
propDeranIncreased amount origList = (amount /= 0) --> isDerangement origList (map (+amount) origList)

derangementProperties = [Prop "Reversed" (propListOfSize propDeranReversed), Prop "Rotated" (propListOfSize propAllDeranRotations), Prop "Swapped" (propListOfSize propDeranSwapped), Prop "Smaller" (propListOfSize propAllDeranSmaller), Prop "Bigger" (propListOfSize propDeranBigger), Prop "Increased" (propListOfSize (propDeranIncreased 1))]

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

derangementTestList = [1,2,3]
derangementTestListTwo = [1,2,3,4]

evenListSizeReverse :: [Int] -> Bool
evenListSizeReverse list = even (length list) --> propDeranReversed list

evenListSizeSwapped :: [Int] -> Bool
evenListSizeSwapped list = even (length list) --> propDeranSwapped list

oddListSizeReverse :: [Int] -> Bool
oddListSizeReverse list = odd (length list) && allDifferent list --> not(propDeranReversed list)

oddListSizeSwapped :: [Int] -> Bool
oddListSizeSwapped list = odd (length list) && allDifferent list --> not(propDeranSwapped list)

testPropAllDeranSmaller :: [Int] -> Bool
testPropAllDeranSmaller list = length list > 1--> not(propAllDeranSmaller list)

testPropDeranBigger :: [Int] -> Bool
testPropDeranBigger list = not(propDeranBigger list)

testPropDeranIncreased :: Int -> [Int] -> Bool
testPropDeranIncreased x list = x /= 0 && not(null list) --> not(propDeranIncreased x list)

testDerangements :: IO ()
testDerangements = do
  putStrLn "\nRunning automated testcases."
  quickCheck propAllDeranRotations
  quickCheck evenListSizeReverse
  quickCheck evenListSizeSwapped
  quickCheck oddListSizeReverse
  quickCheck oddListSizeSwapped
  quickCheck testPropAllDeranSmaller
  quickCheck testPropDeranBigger
  quickCheck testPropDeranIncreased

  putStrLn "\nRunning manual testcases."
  putStrLn $ "Testing same list: "++checkTestResult (not(isDerangement derangementTestList derangementTestList))
  putStrLn $ "Testing all rotated lists: "++checkTestResult (propAllDeranRotations derangementTestList)
  putStrLn $ "Testing reversed list on even lists: "++checkTestResult (propDeranReversed derangementTestListTwo)
  putStrLn $ "Testing all swapped lists on even lists: "++checkTestResult (propDeranSwapped derangementTestListTwo)
  putStrLn $ "Testing reversed list on uneven lists: "++checkTestResult (not(propDeranReversed derangementTestList))
  putStrLn $ "Testing all swapped lists on uneven lists: "++checkTestResult (not(propDeranSwapped derangementTestList))
  putStrLn $ "Testing all smaller lists: "++checkTestResult (not (propAllDeranSmaller derangementTestList))
  putStrLn $ "Testing all bigger lists: "++checkTestResult (not (propDeranBigger derangementTestList))
  putStrLn $ "Testing increased list: "++checkTestResult (not (propDeranIncreased 1 derangementTestList))

-- Assigment 6 (Implementing and testing ROT13 encoding)
-- ROT13 specification: For every character `x` in the list ['a'..'z'] or ['A'..'Z'], it has to be replaced by the character that is in the same list at index `index(x)+13`. If the result is more than the length of the list, the length of the list is to be subtracted from this index, and the entry at the resulting index will be used for the substitution.
-- Time: 60 minutes
-- Result of running QuickCheck on all 4 properties:
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.
--        +++ OK, passed 100 tests.

doRotate :: String -> Char -> Char
doRotate domain rotatingCharacter = domain!!(((ord rotatingCharacter + 13) - ord (head domain)) `mod` length domain)

rot13 :: String -> String
rot13 = foldr (\c acc -> if c `elem` lower then doRotate lower c : acc else if c `elem` upper then doRotate upper c : acc else c:acc) []
  where upper = ['A'..'Z']
        lower = ['a'..'z']

-- Property: The length must be the same after rotation.
lengthSame :: String -> Bool
lengthSame decoded = length decoded == length (rot13 decoded)

-- Property: All letters must still be letters after rotation.
isCharacterLetter :: String -> Bool
isCharacterLetter decoded = all (\(x,y) -> y `elem` upper || y `elem` lower) (filter (\(x,y) -> x `elem` upper || x `elem` lower) (zip decoded (rot13 decoded)))
  where upper = ['A'..'Z']
        lower = ['a'..'z']

-- Property: All non-letter characters must be the same after rotation (rot13 only modifies letters).
equalNonLetterCharacters :: String -> Bool
equalNonLetterCharacters decoded = all (uncurry (==)) (filter (\(x,y) -> not(x `elem` upper || x `elem` lower)) (zip decoded (rot13 decoded)))
  where upper = ['A'..'Z']
        lower = ['a'..'z']

-- Property: The case of the letters stays the same (an uppercase letter should not become a lowercase letter and vice versa).
equalLetterCase :: String -> Bool
equalLetterCase decoded = all (\(x,y) -> ((x `elem` upper) --> y `elem` upper) && ((x `elem` lower) --> y `elem` lower)) (zip decoded (rot13 decoded))
  where upper = ['A'..'Z']
        lower = ['a'..'z']

-- Property: When rotated two times rot13 will yield the beginning result.
doubleRotYieldsBeginResult :: String -> Bool
doubleRotYieldsBeginResult decoded = decoded == rot13 (rot13 decoded)

-- Assignment 7 (Implementing and testing IBAN validation)
-- Time: 100 minutes
-- Result:
--     Testing valid IBANs: Test succeeded!
--     Testing invalid IBANs: Test succeeded!
--
-- Examplanation of the result:
-- To test this function, I included an example IBAN for every country, as available on the official IBAN site (https://www.iban.com/structure). This was absolutely necessary, as this was the only way to hit every case of the `ibanLengthTable`. Also, during testing it seemed that I had forgotten an entry in my `ibanLengthTable`, which was found and could thus be fixed because of the extensive tests. To test if invalid would return False for the IBAN checker, I turned all valid emails into invalid ones by incrementing each by one. This was done by incrementing the fourth number, which would become the final number of the `mod` number and would thus always result in an invalid iban. This even tests the final case of the `ibanLengthTable` (the invalid one that returns -1, thus never resulting in a valid IBAN code) as sometimes the fourth number would be '9' which would, incremented by one, become a number of a greater length (10) and thus resulting in a longer IBAN (which would hit the final case of the `ibanLengthTable`).
-- Automated testing was not done for this case because there are not many definable properties for the `iban` method that would actually proof much (which the manual testing does). For instance, the property `startsWithValidLetters` could have been written, which would basically have to check for everything the `ibanLengthTable` checks for and would thus not proof anything. The testing which is currently done (manual) is optimal to test the validity of the `iban` function.

ibanLengthTable :: String -> Int
ibanLengthTable "AD" = 24
ibanLengthTable "AE" = 23
ibanLengthTable "AL" = 28
ibanLengthTable "AO" = 25
ibanLengthTable "AT" = 20
ibanLengthTable "AZ" = 28
ibanLengthTable "BA" = 20
ibanLengthTable "BE" = 16
ibanLengthTable "BF" = 28
ibanLengthTable "BG" = 22
ibanLengthTable "BH" = 22
ibanLengthTable "BI" = 16
ibanLengthTable "BJ" = 28
ibanLengthTable "BR" = 29
ibanLengthTable "BY" = 28
ibanLengthTable "CF" = 27
ibanLengthTable "CG" = 27
ibanLengthTable "CH" = 21
ibanLengthTable "CI" = 28
ibanLengthTable "CM" = 27
ibanLengthTable "CR" = 22
ibanLengthTable "CV" = 25
ibanLengthTable "CY" = 28
ibanLengthTable "CZ" = 24
ibanLengthTable "DE" = 22
ibanLengthTable "DJ" = 27
ibanLengthTable "DK" = 18
ibanLengthTable "DO" = 28
ibanLengthTable "DZ" = 26
ibanLengthTable "EE" = 20
ibanLengthTable "EG" = 27
ibanLengthTable "ES" = 24
ibanLengthTable "FI" = 18
ibanLengthTable "FO" = 18
ibanLengthTable "FR" = 27
ibanLengthTable "GA" = 27
ibanLengthTable "GB" = 22
ibanLengthTable "GE" = 22
ibanLengthTable "GI" = 23
ibanLengthTable "GL" = 18
ibanLengthTable "GQ" = 27
ibanLengthTable "GR" = 27
ibanLengthTable "GT" = 28
ibanLengthTable "GW" = 25
ibanLengthTable "HN" = 28
ibanLengthTable "HR" = 21
ibanLengthTable "HU" = 28
ibanLengthTable "IE" = 22
ibanLengthTable "IL" = 23
ibanLengthTable "IQ" = 23
ibanLengthTable "IR" = 26
ibanLengthTable "IS" = 26
ibanLengthTable "IT" = 27
ibanLengthTable "JO" = 30
ibanLengthTable "KM" = 27
ibanLengthTable "KW" = 30
ibanLengthTable "KZ" = 20
ibanLengthTable "LB" = 28
ibanLengthTable "LC" = 32
ibanLengthTable "LI" = 21
ibanLengthTable "LT" = 20
ibanLengthTable "LU" = 20
ibanLengthTable "LV" = 21
ibanLengthTable "MA" = 28
ibanLengthTable "MC" = 27
ibanLengthTable "MD" = 24
ibanLengthTable "ME" = 22
ibanLengthTable "MG" = 27
ibanLengthTable "MK" = 19
ibanLengthTable "ML" = 28
ibanLengthTable "MR" = 27
ibanLengthTable "MT" = 31
ibanLengthTable "MU" = 30
ibanLengthTable "MZ" = 25
ibanLengthTable "NE" = 28
ibanLengthTable "NI" = 32
ibanLengthTable "NO" = 15
ibanLengthTable "NL" = 18
ibanLengthTable "PK" = 24
ibanLengthTable "PL" = 28
ibanLengthTable "PS" = 29
ibanLengthTable "PT" = 25
ibanLengthTable "QA" = 29
ibanLengthTable "RO" = 24
ibanLengthTable "RS" = 22
ibanLengthTable "SA" = 24
ibanLengthTable "SC" = 31
ibanLengthTable "SE" = 24
ibanLengthTable "SI" = 19
ibanLengthTable "SK" = 24
ibanLengthTable "SM" = 27
ibanLengthTable "SN" = 28
ibanLengthTable "ST" = 25
ibanLengthTable "SV" = 28
ibanLengthTable "TD" = 27
ibanLengthTable "TG" = 28
ibanLengthTable "TL" = 23
ibanLengthTable "TN" = 24
ibanLengthTable "TR" = 26
ibanLengthTable "UA" = 29
ibanLengthTable "VG" = 24
ibanLengthTable "XK" = 20
ibanLengthTable x = -1

iban :: String -> Bool
iban inputIBAN
   | ibanLengthTable (take 2 inputIBAN) == length inputIBAN = read (foldr (\x acc -> if x `elem` letters then show (ord x - ord (head letters) + 10)++acc else x:acc) [] (drop 4 inputIBAN ++ take 4 inputIBAN)) `mod` 97 == 1
   | otherwise = False
      where letters = ['A'..'Z']

replace :: Int -> Char -> String -> String
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list


-- This list of test ibans contains one valid iban per country.
testIBANs = ["AL35202111090000000001234567","AD1400080001001234567890","AT483200000012345864","AZ96AZEJ00000000001234567890","BH02CITI00001077181611","BY86AKBB10100000002966000000","BE71096123456769","BA393385804800211234","BR1500000000000010932840814P2","BG18RZBB91550123456789","CR23015108410026012345","HR1723600001101234565","CY21002001950000357001234567","CZ5508000000001234567899","DK9520000123456789","DO22ACAU00000000000123456789","SV43ACAT00000000000000123123","EE471000001020145685","FO9264600123456789","FI1410093000123458","FR7630006000011234567890189","GE60NB0000000123456789","DE91100000000123456789","GI04BARC000001234567890","GR9608100010000001234567890","GL8964710123456789","GT20AGRO00000000001234567890","HU93116000060000000012345676","IS030001121234561234567890","IQ20CBIQ861800101010500","IE64IRCE92050112345678","IL170108000000012612345","IT60X0542811101000000123456","JO71CBJO0000000000001234567890","KZ563190000012344567","XK051212012345678906","KW81CBKU0000000000001234560101","LV97HABA0012345678910","LB92000700000000123123456123","LI7408806123456789012","LT601010012345678901","LU120010001234567891","MK07200002785123453","MT31MALT01100000000000000000123","MR1300020001010000123456753","MU43BOMM0101123456789101000MUR","MD21EX000000000001234567","MC5810096180790123456789085","ME25505000012345678951","NL02ABNA0123456789","NO8330001234567","PK36SCBL0000001123456702","PS92PALS000000000400123456702","PL10105000997603123456789123","PT50002700000001234567833","QA54QNBA000000000000693123456","RO09BCYP0000001234567890","LC14BOSL123456789012345678901234","SM76P0854009812123456789123","ST23000200000289355710148","SA4420000001234567891234","RS35105008123123123173","SC52BAHL01031234567890123456USD","SK8975000000000012345671","SI56192001234567892","ES7921000813610123456789","SE7280000810340009783242","CH5604835012345678009","TL380010012345678910106","TN5904018104004942712345","TR320010009999901234567890","UA903052992990004149123456789","AE460090000000123456789","GB98MIDL07009312345678","VG21PACG0000000123456789"]

ibanCheckTest :: IO ()
ibanCheckTest = do
  putStrLn $ "Testing valid IBANs: " ++ checkTestResult (all iban testIBANs)
  putStrLn $ "Testing invalid IBANs: " ++ checkTestResult (all (not . iban) (map (\x -> replace 3 (head (show (digitToInt (x !! 3) + 1))) x) testIBANs))

-- Project Euler Bonus

-- Euler project 1
-- Time: 20 seconds
multiples :: [Int]
multiples = filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [0..1000]

-- Euler 25:
-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
--   - index = 4782
--  Time 45 minutes

-- Fibs function with help from https://wiki.haskell.org/The_Fibonacci_sequence
fibs :: [Integer]
fibs = map fst (iterate (\(a,b) -> (b,a+b)) (0,1))

solveEuler25 :: Int
solveEuler25 = length (takeWhile (< 10^999) fibs)

-- Euler 35:
-- How many circular primes are there below one million?
--  - 55
--  Time 60 minutes
prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

digitToList :: Int -> [Int]
digitToList n = map digitToInt (show n)

listToDigit :: [Int] -> Int
listToDigit ns = read (map intToDigit ns)

rotateOneSpot :: [Int] -> [Int]
rotateOneSpot (x:xs) = xs ++ [x]

rotations :: Int -> [Int] -> [[Int]]
rotations n xs = take n (iterate rotateOneSpot xs)

getRotOfDigit :: Int -> [Int]
getRotOfDigit n = map listToDigit (rotations (length(digitToList n)) (digitToList n))

isCircPrime :: Int -> Bool
isCircPrime n = all prime (getRotOfDigit n)

solveEuler35 :: Int
solveEuler35 = length (filter isCircPrime (takeWhile (<10^6) primes))

-- Euler project 38
-- Time: 30 minutes
pandigital :: Int -> Int -> String -> (Int, String)
pandigital i n p  | length p == 9 && n > 1 = (i, p)
                  | length p > 9 = (i, "")
                  | otherwise = pandigital i (n+1) (p ++ show (i * n))

largestPandigital :: (Int, String)
largestPandigital = maximumBy (\ x y -> compare (snd x) (snd y)) (map (\x -> pandigital x 1 "") [1..50000])
-- We take 50000 as a maximum since (1 * 50000) + (2 * 50000) = 50000100000 which exceeds the 9 digit limit
-- Outcome is (9999,"999919998"), 9999 is the largest 1 to 9 pandigital 9-digit number

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

  let testingList = [1,2,3]
  putStrLn "\n== Assignment 4 (Recognizing Permutations) =="
  print . reverse $ quicksort permutationProperties -- Reversed so it's descending
  testPermutations

  putStrLn "\n== Assignment 5 (Recognizing and generating derangements) =="
  print $ quicksort derangementProperties
  testDerangements

  putStrLn "\n== Assignment 6 (Implementing and testing ROT13 encoding) =="
  quickCheck lengthSame
  quickCheck isCharacterLetter
  quickCheck equalNonLetterCharacters
  quickCheck equalLetterCase
  quickCheck doubleRotYieldsBeginResult

  putStrLn "\n== Assignment 7 (Implementing and testing IBAN validation) =="
  ibanCheckTest

  putStrLn "\nEuler 1: Multiples of 3 and 5:"
  print multiples

  putStrLn "\nEuler 25: Index of the first term in the Fibonacci sequence to contain 1000 digits:"
  print solveEuler25

  putStrLn "\nEuler 35: Circular primes below one million:"
  print solveEuler35

  putStrLn "\nEuler 38: Pandigital multiples:"
  print largestPandigital

  putStrLn "Done"
