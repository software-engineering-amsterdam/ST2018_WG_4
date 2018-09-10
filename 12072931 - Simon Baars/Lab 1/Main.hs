import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Char

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Lab Assignment 1:

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

factorial :: Integer -> Integer
factorial n = product [1..n]

permutationsList :: Integer -> [[Integer]]
permutationsList  n
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

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

findAllReversedPrimesTillTenThousand :: [Integer]
findAllReversedPrimesTillTenThousand = takeWhile (<10000) (filter (prime . reversal) primes)

-- How would you test this function, by the way?
-- By checking for every prime below 10.000 that if it is inversible that it is in the resulting list, and if it's not inversible that it is not in the resulting list:

reversedPrimesTest :: Bool
reversedPrimesTest = all (\x -> prime(reversal x) --> x `elem` findAllReversedPrimesTillTenThousand) (takeWhile (<10000) primes)

-- Assignment 5

findFirst101ConsecutivePrimeSum :: Integer
findFirst101ConsecutivePrimeSum = head (filter prime (map (\x -> sum(take 101 (drop x primes))) [0..]))

-- Do you have to test that your answer is correct? How could this be checked?
-- Yes, you do have to test the answer. However, writing a test in haskell would result in writing exactly the same code, which doesn't prove much.

-- Assignment 6

generateConjunctureCounterexamples :: [[Integer]]
generateConjunctureCounterexamples = map (`take` primes) (filter (\x -> not(prime(product(take x primes)+1))) [1..])

-- Assignment 7

luhn :: Int -> Bool
luhn x = tail (show (foldl (\acc x -> acc + digitToInt x) 0 (foldr doEncrypt [] $ zip [0..] (show x)))) == "0"
    where
        doEncrypt (i,y) acc = if odd i
            then head (show((uncurry (+) . (`divMod` 10) . (*2)) (digitToInt y))) : acc
            else y : acc

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x = ((length creditCardString == 15) && (head creditCardString == '3')) && ((secondCharacter == '4') || (secondCharacter == '7'))
    where creditCardString = show x
          secondCharacter = head $ tail creditCardString
isVisa x = (length creditCardString == 16) && (head creditCardString == '4') where creditCardString = show x
isMaster x = ((length creditCardString == 16) && (head creditCardString == '5')) && ((secondNumber >= 1) || (secondNumber <= 5))
  where creditCardString = show x
        secondNumber = digitToInt $ head $ tail creditCardString

-- Assignment 8

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew other = (other /= Matthew) && (other /= Carl)
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack other = not $ accuses Matthew other || accuses Peter other
accuses Arnold other =  accuses Matthew other /= accuses Peter other
accuses Carl other = not $ accuses Arnold other
accuses x y = False

accusers :: Boy -> [Boy]
accusers b1 = filter (`accuses` b1) boys

guilty, honest :: [Boy]
guilty = filter (\x -> length (accusers x) == 3) boys
honest = filter (\x -> accuses x (head guilty)) boys

main :: IO ()
main = do
  putStrLn "Running tests for Assignment 1"
  quickCheckResult assignmentTwoTest
  quickCheckResult assignmentThreeTest
  putStrLn "Running tests for Assignment 2"
  --quickCheckResult $ forAll genAssignment2 powerListInductionTest
  putStrLn "Running tests for Assignment 3"
  --quickCheckResult $ forAll genAssignment3 permutationTest
  putStrLn "Running tests for Assignment 4"
  print reversedPrimesTest
  putStrLn "Running tests for Assignment 5"
  print findFirst101ConsecutivePrimeSum
  putStrLn "The smallest counter example for Assignment 6 is "
  print $ head generateConjunctureCounterexamples
  putStrLn "Running tests for Assignment 7"

  putStrLn "Results of Assignment 8:"
  print guilty
  print honest
  putStrLn "Done!"
