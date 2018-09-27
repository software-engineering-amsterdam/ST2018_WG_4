module Main where

import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad
import Debug.Trace
import Data.List

-- Assignment 1 (Haskell Road to Logic)
-- Time: 190 minutes
--
-- Page 122: What does the keyword `undefined` do in Haskell?

-- Assignment 2 (Random data generator for Set Int)
-- Time: 90 minutes
randomNumberStream :: IO [Int]
randomNumberStream = do
    g <- newStdGen
    return $ randomRs (minBound :: Int, maxBound :: Int) g

randomNumber :: Int -> Int -> IO Int
randomNumber minB maxB = do
    g <- newStdGen
    return $ fst $ randomR (minB, maxB) g

-- Removes all duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\y x -> if x `elem` y then y else y++ [x]) []

generateRandomSetInt :: IO (Set Int)
generateRandomSetInt = randomNumberStream >>= \x -> randomNumber 0 100 >>= \y -> return (list2set (take y x))

randomSetGenerator :: (Ord a, Arbitrary a) => Int -> Gen (Set a)
randomSetGenerator size = replicateM size arbitrary >>= \x -> return (list2set x)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized randomSetGenerator

setPrinter :: Set Int -> Bool
setPrinter x = trace ("Input set = " ++ show x) True

-- Assignment 3
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

testIntersection :: Set Int -> Set Int -> Bool
testIntersection l r = propOnlyNotInLeftSet setIntersection l r && propNotOnlyInLeftSet setIntersection l r && propOnlyNotInRightSet setIntersection l r && propNotOnlyInRightSet setIntersection l r && propInBothSets setIntersection l r

testUnion :: Set Int -> Set Int -> Bool
testUnion l r = propOnlyInLeftSet setUnion l r && propOnlyNotInLeftSet setUnion l r && propOnlyInRightSet setUnion l r && propOnlyNotInRightSet setUnion l r && propInBothSets setUnion l r

testDifference :: Set Int -> Set Int -> Bool
testDifference l r = propOnlyInLeftSet setDifference l r && propNotOnlyNotInLeftSet setDifference l r && propNotOnlyNotInRightSet setDifference l r  && propNotInBothSets setDifference l r

-- Assignment 5

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = sort $ a `union` [(y,x) | (x,y) <- a, (y,x) `notElem` a]

-- Assignment 6

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos closure
  | closure == closureUntilNow = sort closure
  | otherwise                  = trClos closureUntilNow
  where closureUntilNow = nub $ closure ++ (closure @@ closure)

-- Assignment 7


main :: IO ()
main = do
  putStrLn "== Assignment 2 (Random data generator for Set Int) =="
  print =<< generateRandomSetInt
  quickCheck setPrinter

  putStrLn "== Assignment 3 (Union, Intersect and Differerce on sets) =="
  quickCheck testIntersection
  quickCheck testUnion
  quickCheck testDifference

  putStrLn "== Assignment 5 (Symmetric Closure) =="
  print $ symClos [(1,2),(2,3),(3,4)]

  putStrLn "== Assignment 6 (Transitive Closure) =="
  print $ trClos [(1,2),(2,3),(3,4)]
