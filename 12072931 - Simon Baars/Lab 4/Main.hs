module Main where

import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad
import Debug.Trace
import Data.List

-- Assignment 1 (Haskell Road to Logic Chapter 4)
-- Time: 190 minutes
--
-- Page 122: What does the keyword `undefined` do in Haskell?
-- Page 145: What are the implications of having an atom as your factor?
-- Page 145: How do "logical translations" work?

-- Assignment 2 (Random data generator for Set Int)
-- Time: 90 minutes
randomNumberStream :: Int -> Int -> IO [Int]
randomNumberStream minB maxB = do
    g <- newStdGen
    return $ randomRs (minB, maxB) g

randomNumber :: Int -> Int -> IO Int
randomNumber minB maxB = do
    g <- newStdGen
    return $ fst $ randomR (minB, maxB) g

-- Removes all duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\y x -> if x `elem` y then y else y++ [x]) []

generateRandomSetInt :: IO (Set Int)
generateRandomSetInt = randomNumber 0 100 >>= \y -> randomNumberStream (-y) y >>= \x -> return (list2set (take y x))

randomSetGenerator :: (Ord a, Arbitrary a) => Int -> Gen (Set a)
randomSetGenerator size = replicateM size arbitrary >>= \x -> return (list2set x)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized randomSetGenerator

setPrinter :: Set Int -> Bool
setPrinter x = trace ("QuickCheck random set = " ++ show x) True

-- Assignment 3
-- Time: 120 minutes

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set x) (Set y) = list2set (x `intersect` y)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set x) (Set y) = list2set (x `union` y)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set x) (Set y) = list2set (x \\ y)

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

set2list :: Ord a => Set a -> [a]
set2list (Set a) = a

-- LEFT SET
propOnlyInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` rightSet --> x `elem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propOnlyNotInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyNotInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `elem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propNotOnlyInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` rightSet --> x `notElem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propNotOnlyNotInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyNotInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `notElem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- RIGHT SET
propOnlyInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propOnlyNotInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyNotInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propNotOnlyInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` leftSet --> x `notElem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propNotOnlyNotInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyNotInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` leftSet --> x `notElem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- BOTH SETS
propInBothSets :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propInBothSets setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `elem` newSet) leftSet && all (\x -> x `elem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

propNotInBothSets :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotInBothSets setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `notElem` newSet) leftSet && all (\x -> x `elem` leftSet --> x `notElem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

checkProperties :: [(Set a -> Set a -> Set a) -> Set a -> Set a -> Bool] -> (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
checkProperties a b c d = all (\x -> x b c d) a

testIntersection :: Set Int -> Set Int -> Bool
testIntersection = checkProperties [propOnlyNotInLeftSet, propNotOnlyInLeftSet, propOnlyNotInRightSet, propNotOnlyInRightSet, propInBothSets] setIntersection

testUnion :: Set Int -> Set Int -> Bool
testUnion = checkProperties [propOnlyInLeftSet, propOnlyNotInLeftSet, propOnlyInRightSet, propOnlyNotInRightSet, propInBothSets] setUnion

testDifference :: Set Int -> Set Int -> Bool
testDifference = checkProperties [propOnlyInLeftSet, propNotOnlyNotInLeftSet, propNotOnlyNotInRightSet, propNotInBothSets] setDifference

-- Assignment 4 (Haskell Road to Logic Chapter 5)
-- Time: 240 minutes
--
-- This chapter was really though to get through. The examples were hard to follow and I could solve few of the exercises without looking at the solutions. In the end I did not have time to further solve any exercises.
-- Page 174: What is a "Real Plane"? Why is R2 called a "real plane"?
-- Page 180 & 182: What is a delta set? --> This was answered later on, as it is the identity of a certain set.
-- Example 5.63 is completely unclear to me.
-- Page 208: What does "having a common ancestor down the male line" mean?
-- Page 208 & 209: How to work with stirling set numbers?
-- Page 211: How does a quotient work?
-- Page 211: How to determine the number of elements in an atom? Or does |a| mean something else in this formula?

-- Assignment 5
-- Time: 10 minutes

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = sort $ a `union` [(y,x) | (x,y) <- a, (y,x) `notElem` a]

-- Assignment 6
-- Time: 30 minutes

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos closure
  | closure == closureUntilNow = sort closure
  | otherwise                  = trClos closureUntilNow
  where closureUntilNow = nub $ closure ++ (closure @@ closure)

-- Assignment 7
-- Time: 45 minutes

propAllReversed :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propAllReversed relFunction rel = let applied = relFunction rel in all (\(x,y) -> (y,x) `elem` applied) rel

propAllOriginal :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propAllOriginal relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) rel

propFirstClosure :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propFirstClosure relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) (rel @@ rel)

propSecondClosure :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propSecondClosure relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) (rel @@ applied)

testSymmetricClosure :: Rel Int -> Bool
testSymmetricClosure x = propAllReversed symClos x && propAllOriginal symClos x

testTransitiveClosure :: Rel Int -> Bool
testTransitiveClosure x = propAllOriginal trClos x && propFirstClosure trClos x && propSecondClosure trClos x

-- Assignment 8
-- Time: 30 minutes

-- So, for this example I created the following function to make QuickCheck come up with a counterexample:
testSymmetricTransitiveClosure :: Rel Int -> Bool
testSymmetricTransitiveClosure x = symClos (trClos x) == trClos (symClos x)
-- Using this, QuickCheck will find a counter example after three tests, at both [(0,1)] and [(1,0)]. Hereby my explanation:
-- `trClos (symClos [(0,1)])` results in `[(0,0),(0,1),(1,0),(1,1)]`
-- `symClos (trClos [(0,1)])` results in `[(0,1),(1,0)]`
-- This is because `symClos [(0,1)]` results in `[(0,1),(1,0)]`, and then doing `trClos [(0,1),(1,0)]` we het the result `[(0,0),(0,1),(1,0),(1,1)]`.
-- However, if we calculate the transitive close first (`trClos [(0,1)]`) we get `[(0,1)]`, of which the symbolic closure (`symClos [(0,1)]`) is `[(0,1),(1,0)]`.

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 2 (Random data generator for Set Int) ==\x1b[0m"
  generateRandomSetInt >>= \x -> putStrLn $ "My random list: " ++ show x
  quickCheck (withMaxSuccess 10 setPrinter)

  putStrLn "\x1b[36m== Assignment 3 (Union, Intersect and Differerce on sets) ==\x1b[0m"
  quickCheck testIntersection
  quickCheck testUnion
  quickCheck testDifference

  putStrLn "\x1b[36m== Assignment 5 (Symmetric Closure) ==\x1b[0m"
  print $ symClos [(1,2),(2,3),(3,4)]

  putStrLn "\x1b[36m== Assignment 6 (Transitive Closure) ==\x1b[0m"
  print $ trClos [(1,2),(2,3),(3,4)]

  putStrLn "\x1b[36m== Assignment 7 (Testing Symmetric Closure & Transitive Closure) ==\x1b[0m"
  quickCheck testSymmetricClosure
  quickCheck testTransitiveClosure

  putStrLn "\x1b[36m== Assignment 8 (Please note that quickcheck failure is intended here!) ==\x1b[0m"
  quickCheck testSymmetricTransitiveClosure
