
import Test.QuickCheck
import Data.List
import Lab1
import Data.Char
-- import Data.Numbers

-- Assignment 1 formula 2
-- started 9:15
-- finished 10:17

leftHandSide:: Int -> Int
leftHandSide n = sum(map (^2) [1..n])


rightHandSide:: Int -> Int
rightHandSide n  =  quot (n * (n + 1) * (2*n + 1)) (6)


funcTwoTest :: Int -> Bool
funcTwoTest n
  | n > 0 = leftHandSide n == rightHandSide n
  | otherwise = True

-- Assignment 1 formula 3
-- started 10:22
-- finished 10:25

leftHandSideThree:: Int -> Int
leftHandSideThree n = sum(map (^3) [1..n])


rightHandSideThree:: Int -> Int
rightHandSideThree n = (quot (n * (n + 1)) (2))^2


funcThreeTest:: Int -> Bool
funcThreeTest n
  | n > 0 = leftHandSideThree n == rightHandSideThree n
  | otherwise = True



-- Assignment 2
-- started 10:37
-- finished 10:53


fourSet :: Int -> Bool
fourSet n
  | n <  10 && n > 0 = length (subsequences [1..n]) == 2^n
  | otherwise = True

-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: The property is hard to test because it when using a set with high cardinality, the
-- complexity of the problem becomes too high. It takes a lot of time too compute.

-- Q: Give your thoughts on the following issue: when you perform the test for exercise 4,
--    what are you testing actually? Are you checking a mathematical fact? Or are you
--    testing whether subsequences satisfies a part of its specification? Or are you
--    testing something else still?
-- A: I am testing a mathematical hypothese (can become a fact if proven by induction)
--    for a finite set with a cardinality of 10 max. (Refine?)

factorial :: Int -> Int
factorial n = product([1..n])

-- Assignment 3
-- started 11:35
-- finished 11:45
assignThree :: Int -> Bool
assignThree n = length (permutations [1..n]) == factorial n

-- Is the property hard to test? If you find that it is, can you given a reason why?




-- Assignment 4
-- started 12.12
-- finished ...
-- estimated time (30 mins)
primeListReversal :: [Integer]
primeListReversal = filter (\x -> elem (reversal x) primelist) primelist where
                    primelist = takeWhile (< 10000) primes

-- Reverse each prime and compare it to the list, sort the list. check for each
-- Element if prime
testPrimeListReversal :: Bool
testPrimeListReversal = ((sort [ reversal x | x<-primeListReversal]) == primeListReversal) ==  not (elem False (map prime primeListReversal))

-- Assignment 5
-- started 14.23
-- finished 15:35

-- 37447
-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to - from) (drop from xs)

primelist :: Int -> [Integer]
primelist n = take 101 (drop n primes) --slice n (101 + n) primes

-- Feed n = 0
smallestPrime :: Int -> Integer
smallestPrime n = if (prime (sum(primelist n))) then sum(primelist n) else smallestPrime (n + 1)

-- Assignment 6
-- started 16:11
-- finished ~17:10 (estimated)

conjuctureSix :: Int -> [Integer] -> [Integer]
conjuctureSix n total
 | not (prime (product(take n primes) + 1)) = conjuctureSix (n + 1) (total ++ [product(take n primes) + 1])
 | otherwise = conjuctureSix (n +1)  total

conjuctureSixMin :: [Integer]
conjuctureSixMin = take 1 (conjuctureSix 0 [])

 -- Assignment 7
 -- started 17:37
 -- finished
-- estimated time ~ 3 hours
-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

createTable :: Integer -> [(Int, Integer)]
createTable creditcard_number = zip (digits creditcard_number) (combination)
              where
              combination = genericTake (genericLength (digits creditcard_number)) (cycle [2, 1])

doArith :: [(Int, Integer)] -> Int -> Int
doArith [] total = last (digits (toInteger total))
doArith tableCreditcardNumber total = if (length (digits (toInteger prodTuple)) == 2) then
                                      doArith (tail tableCreditcardNumber) (total + sumTuple)
                                      else
                                      doArith (tail tableCreditcardNumber) (total + prodTuple)
                                      where
                                      prodTuple =  (product(head (map (\(x,y) -> [( x), (fromIntegral y)]) ([head tableCreditcardNumber]))))
                                      sumTuple = (sum(head (map (\(x,y) -> [( x), (fromIntegral y)]) ([head tableCreditcardNumber]))))

luhn :: Integer -> Bool
luhn creditcard_number
  | (doArith (createTable creditcard_number) 0) == 0 = True
  | otherwise = False

-- isMaster, isVisa
-- https://en.wikipedia.org/wiki/Payment_card_number#cite_note-GenCardFeatures-6
isAmericanExpress  :: Integer -> Bool
isAmericanExpress creditcard_number = (length listNumbers == 15) &&
                                      (((take 2 listNumbers) == [3,4]) || (take 2 listNumbers) == [3,7])
                                      where
                                      listNumbers = digits (toInteger creditcard_number)

isMaster  :: Integer -> Bool
isMaster creditcard_number = (length listNumbers == 16) &&
                             ((firstTwo > 50  && firstTwo < 56) ||
                             (firstFour > 2221  && firstTwo < 2720))
                             where
                             firstFour =  read (take 4 (show creditcard_number))
                             firstTwo =  read (take 2 (show creditcard_number))
                             listNumbers = digits (toInteger creditcard_number)
isVisa  :: Integer -> Bool
isVisa creditcard_number =  (head listNumbers == 4) && (length listNumbers == 16)
                            where
                            listNumbers = digits (toInteger creditcard_number)


-- Assignment 8
-- started 18:40
-- finished
-- estimated time 1.5 hours

accuses :: Boy -> Boy -> Bool
accuses boyOne boyTwo
        |boyOne == Matthew = (boyTwo /= Carl && boyTwo /= Matthew)
        |boyOne == Peter = (boyTwo == Jack || boyTwo == Matthew)
        |boyOne == Jack =  (not (accuses Matthew boyTwo) &&  not (accuses Peter boyTwo))
        |boyOne == Arnold = (accuses Matthew boyTwo || accuses Peter boyTwo)
        |boyOne == Carl = not (accuses Arnold boyTwo)
        |otherwise = False


accusers :: Boy -> [Boy]
accusers boy = [x | x<-boys, accuses x boy]


-- Not done yet
