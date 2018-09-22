import Lab2
import Data.List
import Data.Char
import Test.QuickCheck
import Text.Show.Functions
import System.Random
import Data.Function (on)


-- Assignment 1
-- time 1 hours

testCurry :: Int -> IO ()
testCurry n = do
                listNumbs <- probs n
                let
                  first =  filter (\x -> x >= 0 && x <= 0.25)  listNumbs
                  second = filter (\x -> x > 0.25 && x <= 0.5)  listNumbs
                  third = filter (\x -> x > 0.5 && x <= 0.75)  listNumbs
                  fourth = filter (\x -> x > 0.75 && x <= 1)  listNumbs
                  in print (length first, length second, length third, length fourth)


-- Assignment 2
-- time 20 min

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (a + b) <= c || (a + c) <= b || (b + c) <= a = NoTriangle
                | a == b && b == c = Equilateral
                | a == b || b == c || a == c = Isosceles
                | isPyth a b c || isPyth b c a || isPyth c a b = Rectangular
                | otherwise = Other


isPyth :: Integer -> Integer -> Integer -> Bool
isPyth a b c = (a^2 + b^2) == c^2

-- Assignment 3: Testing properties strength
-- Started 13.00
-- estimated 4 hours

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x
p4 x = (even x && x > 3) || even x
p5 = even

-- Counts amount of True's
convertBoolToInt :: [Bool] -> Int-> Int
convertBoolToInt [] count = count
convertBoolToInt (headPerm:tailPerm) count
  | headPerm = convertBoolToInt tailPerm (count + 1)
  | otherwise = convertBoolToInt tailPerm count

-- Mapping of Property :t [Char] to Property :t (Int-> Bool)
mapProp :: [Char] -> (Int -> Bool)
mapProp propertyString
  | propertyString == "p1" = p1
  | propertyString == "p2" = p2
  | propertyString == "p3" = p3
  | propertyString == "p4" = p4
  | propertyString == "p5" = p5


-- For each tuple being the property as a char and the rest of the properties,
-- check how much is stronger and use the convertBoolToInt func to count them
checkPerm :: [Char] -> [(Int -> Bool)] -> Int
checkPerm propChar allProp = convertBoolToInt strongerList 0
                             where
                               property = (mapProp propChar)
                               strongerList = map (\x -> stronger [-10..10] property x) allProp

--
strengthList :: [([Char], Int)]
strengthList =  map (\ (x, y)  -> (x, (checkPerm x y)) ) (zip allPropChar allProp)
                where
                  allProp =  [[p2, p3, p4, p5],
                             [p1, p3, p4, p5],
                             [p1, p2, p4, p5],
                             [p1, p2, p3, p5],
                             [p1, p2, p3, p4]]
                  allPropChar =  ["p1", "p2", "p3", "p4", "p5"]



-- Assignment 3: Recognizing Permutations


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation listOne listTwo
  | elem listTwo (permutations listOne) = True
  | otherwise = False


-- Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation.
-- You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- It means that the input that only has unique variables has less elements in the output
-- compared to input that does not.
-- Furthermore, it is the case that if you strengthen the requirements on your inputs, your
-- testing procedure gets weaker. This is because the set of relevant tests get smaller
-- Prelude Data.List> permutations [1,1]
-- [[1,1],[1,1]]

testPermOne, testPermTwo, testPermThree,testPermFour :: Bool
testPermOne = isPermutation [1,2,3,4] [4,3,1,2] -- True
testPermTwo = isPermutation  [1,2,3,4] [4,1,2] -- False
testPermThree = isPermutation  [1,2,3,4] [4,4,1,2] -- False
testPermFour = isPermutation  [1,2,3,4] [4,3,1,20] -- False



-- Provide an ordered list of properties by strength using the weaker and stronger definitions.
testStrongerProp = [["a", "b", "c", "d"], ["b", "c", "d"], ["b", "c"]]
testStrongerPropPerm = [[ "b", "a", "d", "c"], ["d", "b", "c"], ["c", "b"]]

-- Sorting perms, stronger are infront of the lists
-- https://stackoverflow.com/questions/2307893/sorting-lists-of-lists-in-haskell
sortPerms perms = reverse (groupBy ((==) `on` length) $ sortBy (compare `on` length) perms)

-- Assignment 4: Recognizing and generating derangements
-- Started
-- ended

checkMapping :: Eq a => [(a, a)] -> Bool
checkMapping zippedList = (filter (\ (x, y) -> x == y) zippedList) == []

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement listOne listTwo = isPermutation listOne listTwo && checkMapping (zip listOne listTwo)

deran :: Eq a => [a] -> [[a]]
deran origlist = filter (\ x -> (checkMapping (zip origlist x)) == True) (permutations origlist)

-- Assignment 5: Implementing and testing ROT13 encoding
-- Started
-- ended

-- Specification:
-- Precondition = postcondition + 13
-- Postcondtion = precondtion + 13
-- cardinality of the alfabet == 26
-- if postcondition =  rot (x) then Precondition = rot(rot(x)) which is x
add13 :: [Char] -> [Char] -> [Char]
add13 [] aftStr = aftStr
add13 (x:xs) aftStr
  | x >=  'a' && x <= 'z' = add13 xs (aftStr ++ [(chr (( mod (ord x - ord 'a' + 13) 26) + ord 'a'))])
  | x >= 'A' && x <= 'Z' = add13 xs (aftStr ++ [(chr (( mod (ord x - ord 'A' + 13) 26) + ord 'A'))])
  | otherwise = aftStr ++ (x:xs)

rot13 :: [Char] -> [Char]
rot13 precond = add13 precond []

-- Test Property: Call with quickCheck
testRot13 :: [Char] -> Bool
testRot13 = \x -> let postcondition = rot13 x in rot13 postcondition == x
             where types x = x::[Char]


-- Assignment 7: Implementing and testing IBAN validation
-- Estimated time 4h

-- Mapping of char to int, starting with a = 10 etc.
charToInt :: Char -> [Char]
charToInt letter =  show ((ord letter - ord 'A'  + 10))

-- For each element in the first list check if not a digit and if
-- it is not then call charToInt, afterwards just add it to the
-- after string (output)
makeStr :: [Char] -> [Char] -> [Char]
makeStr [] aftStr = aftStr
makeStr (x:xs) aftStr
  | not (x >=  '0' && x <= '9') = makeStr xs (aftStr ++ (charToInt x))
  | otherwise =  makeStr xs (aftStr ++ [x])

-- Converts total string that is obtained from mkstr to integer
mkInt ::  [Char] -> Integer
mkInt str = read (makeStr ((drop 4 str) ++ (take 4 str)) [])::Integer

-- Determines if the IBAN number is valid or not
iban :: [Char] -> Bool
iban str
 | (mod (mkInt str) 97) == 1 = True
 | otherwise = False

-- Using a generator: http://www.generateiban.com/test-iban/
ibanTrue = ["NL90ABNA0834679672", "NL47ABNA0252605756", "NL49AEGO0729267978",
            "NL47RABO0317797108", "NL57ABNA0542052412", "NL54RABO0131367056"]

-- Changing the generated iban's
ibanFalse = ["NL90ABNA1834679672", "NX47ABNA0252605756", "NL494EGO0729267978",
             "NL47RABO0217797108", "NL57ABNA0442052442", "NL54RADO0133367056"]

-- Tests all the defined iban's above
ibanTestManual = (all (\x -> iban x) ibanTrue) && (all (\x -> not(iban x)) ibanFalse)

-- No you cannot automate this
