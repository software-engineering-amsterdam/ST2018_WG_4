import           Lab1
import           Data.Char
import           Data.List
import           Test.QuickCheck

-- Lab Assignment 1:
-- Time: 30 minutes

assignmentTwoLeftSide :: Integer -> Integer
assignmentTwoLeftSide n = sum(map (^2) [1..n])

assignmentTwoRightSide :: Integer -> Integer
assignmentTwoRightSide n = (n*(n+1)*(2*n+1)) `div` 6

assignmentTwoTest :: Integer -> Bool
assignmentTwoTest n = (n > 0) --> assignmentTwoLeftSide n == assignmentTwoRightSide n

assignmentThreeLeftSide :: Integer -> Integer
assignmentThreeLeftSide n = sum(map (^3) [1..n])

assignmentThreeRightSide :: Integer -> Integer
assignmentThreeRightSide n = ((n*(n+1)) `div` 2)^2

assignmentThreeTest :: Integer -> Bool
assignmentThreeTest n = (n > 0) --> assignmentThreeLeftSide n == assignmentThreeRightSide n

-- Lab Assignment 2:
-- Time: 20 minutes

getListOfSizeN :: Integer -> [Integer]
getListOfSizeN n
 | n <= 0 = [] -- Base case
 | otherwise = [0..n]

getPowerListOfSizeN :: Integer -> [[Integer]]
getPowerListOfSizeN  n
 | n <= 0 = [[]] -- Base case
 | otherwise = subsequences [0..n]

powerListInductionTest :: Integer -> Bool
powerListInductionTest n = (n >= 0) --> length(getPowerListOfSizeN n) == 2 ^ length(getListOfSizeN n)

genAssignment2 :: Gen Integer
genAssignment2 = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (< 25)

-- Question: Is the property hard to test? If you find that it is, can you given a reason why?
-- Answer: It is, because of the size of the resulting list of subsequences for large numbers (>25) increases exponentially. Due to this, QuickCheck gets blocked during the checking. To solve this, a custom QuickCheck generator had to be written to instruct QuickCheck to only pick numbers below 25. However, this reduces the representativeness of the test itself.

-- Question: Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?
-- Answer: Actually I am testing both. If either the `subsequences` method or the mathematical fact `|A| = n then |P(A)| = 2^n` was incorrect, the test would fail. Another aspect of the haskell language that would be tested by this code would be the sequence generator (`[0..n]`).

-- Lab Assignment 3
-- Time: 25 minutes

factorial :: Integer -> Integer
factorial n = product [1..n]

permutationsList :: Integer -> [[Integer]]
permutationsList n
 | n <= 0 = [[]] -- Base case
 | otherwise = permutations [1..n]

permutationTest :: Integer -> Bool
permutationTest n = (n >= 0) --> length(permutationsList n) == fromIntegral(factorial n)

genAssignment3 :: Gen Integer
genAssignment3 = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (< 10)

-- Question: Is the property hard to test? If you find that it is, can you given a reason why?
-- Answer: It is, for the same reason as for assignment 2 (the size of the permutations lists becomes higher exponentially). Sadly, for assignment 3 it is even worse than for assignment 2. This can be solved by only allowing numbers up to 10, reducing the representativeness of the test itself largely.

-- Question: Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification? Or are you testing something else still?
-- Answer: Again, I'd have to conclude that both facts are checked. If either the `permutations` method or the mathematical fact `(n + 1)! is the number of permutations for a list with size n` was incorrect, the test would fail. Additionally we are testing the `factorial` function.

-- Assignment 4
-- Time: 25 minutes

findAllReversedPrimesTillTenThousand :: [Integer]
findAllReversedPrimesTillTenThousand = takeWhile (<10000) (filter (prime . reversal) primes)

-- How would you test this function, by the way?
-- By checking for every prime below 10.000 that if it is inversible that it is in the resulting list, and if it's not inversible that it is not in the resulting list:

reversedPrimesTest :: Bool
reversedPrimesTest = all (\x -> prime(reversal x) --> x `elem` findAllReversedPrimesTillTenThousand) (takeWhile (<10000) primes)

-- Assignment 5
-- Time: 20 minutes

findFirst101ConsecutivePrimeSum :: Integer
findFirst101ConsecutivePrimeSum = head (filter prime (map (\x -> sum(take 101 (drop x primes))) [0..]))

-- Do you have to test that your answer is correct? How could this be checked?
-- Yes, you do have to test the answer. However, writing a test in haskell using QuickCheck would result in writing exactly the same code (to check whether the test succeeded), which doesn't prove much. In this case, unit tests would be more applicable.

-- Assignment 6
-- Time: 20 minutes

generateConjunctureCounterexamples :: [[Integer]]
generateConjunctureCounterexamples = map (`take` primes) (filter (\x -> not(prime(product(take x primes)+1))) [1..])

-- Assignment 7
-- Time: 155 minutes

doEncrypt :: String -> String -> String
doEncrypt acc (evenEl:oddEl:restOfList) = doEncrypt (evenEl : head (show((uncurry (+) . (`divMod` 10) . (*2)) (digitToInt oddEl))) : acc) restOfList
doEncrypt acc (lastEl:emptyList) = lastEl : acc
doEncrypt acc [] = acc

luhn :: Integer -> Bool
luhn x = tail (show (foldl (\acc x -> acc + digitToInt x) 0 (doEncrypt [] $ reverse $ show x))) == "0"

inRanges :: Integer -> [(Integer, Integer)] -> Bool
inRanges checking [] = False
inRanges checking ((x,y):pairs)
  | checking>=x && checking<y = True
  | otherwise = inRanges checking pairs

totalLength :: Int -> Int -> Integer
totalLength number size =  read(integerString ++ replicate (size-length integerString) '0')
  where integerString = show number

masterRanges = [(totalLength 51 16, totalLength 56 16), (totalLength 2221 16, totalLength 2721 16)]
americanExpressRanges = [(totalLength 34 15, totalLength 35 15), (totalLength 37 15, totalLength 38 15)]
visaRanges = [(totalLength 4 16, totalLength 5 16)]

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x = luhn x && inRanges x americanExpressRanges
isVisa x = luhn x && inRanges x visaRanges
isMaster x = luhn x && inRanges x masterRanges

-- Tests:
visaCards = [(4916036260934004, True),
             (4539301335926626, True),
             (4916848251695919, True),
             (4539738376397136, True),
             (4485933520215466, True),
             (6277500350364257, False),
             (550612643282991, False),
             (4323950234189624, False),
             (375543148983147, False),
             (53252569781795000, False)]

masterCards = [(5277500350364257, True),
               (5506126432829910, True),
               (5323950234189624, True),
               (5407673640547685, True),
               (5325256978179500, True),
               (6277500350364257, False),
               (550612643282991, False),
               (4323950234189624, False),
               (375543148983147, False),
               (53252569781795000, False)]

americanExpressCards = [(375543148983147, True),
                        (379773178506528, True),
                        (372003346961034, True),
                        (340656764193148, True),
                        (344174328134547, True),
                        (6277500350364257, False),
                        (550612643282991, False),
                        (4323950234189624, False),
                        (075543148983147, False),
                        (53252569781795000, False)]

testCreditCards :: (Integer -> Bool) -> [(Integer, Bool)] -> Bool
testCreditCards testingFunction = all (\(x, y) -> testingFunction x == y)

-- Assignment 8
-- Time: 95 minutes

accuses :: Boy -> Boy -> Bool
accuses Matthew other = (other /= Matthew) && (other /= Carl)
accuses Peter Matthew = True
accuses Peter Jack    = True
accuses Jack other    = not $ accuses Matthew other || accuses Peter other
accuses Arnold other  =  accuses Matthew other /= accuses Peter other
accuses Carl other    = not $ accuses Arnold other
accuses x y           = False

accusers :: Boy -> [Boy]
accusers b1 = filter (`accuses` b1) boys

guilty, honest :: [Boy]
guilty = filter (\x -> length (accusers x) == 3) boys
honest = filter (\x -> accuses x (head guilty)) boys

checkTestResult :: Bool -> String
checkTestResult True = "Test succeeded!"
checkTestResult False = "Test failed!"

main :: IO ()
main = do
  putStrLn "== Assignment 1 =="
  quickCheckResult assignmentTwoTest
  quickCheckResult assignmentThreeTest

  putStrLn "\n== Assignment 2 =="
  --quickCheckResult $ forAll genAssignment2 powerListInductionTest

  putStrLn "\n== Assignment 3 =="
  quickCheckResult $ forAll genAssignment3 permutationTest

  putStrLn "\n== Assignment 4 =="
  putStrLn $ checkTestResult reversedPrimesTest

  putStrLn "\n== Assignment 5 =="
  putStrLn $ "The first prime that can be constructed of the sum of 101 consecutive primes is " ++ show findFirst101ConsecutivePrimeSum

  putStrLn "\n== Assignment 6 =="
  putStrLn $ "The smallest counter example is " ++ show(head generateConjunctureCounterexamples)

  putStrLn "\n== Assignment 7 =="
  putStrLn $ "Visa test: " ++ checkTestResult(testCreditCards isVisa visaCards)
  putStrLn $ "Mastercard test: " ++ checkTestResult(testCreditCards isMaster masterCards)
  putStrLn $ "American Express test: " ++ checkTestResult(testCreditCards isAmericanExpress americanExpressCards)

  putStrLn "\n== Assignment 8 =="
  putStrLn $ "Guilty person: " ++ show guilty
  putStrLn $ "Honest persons: " ++ show honest
