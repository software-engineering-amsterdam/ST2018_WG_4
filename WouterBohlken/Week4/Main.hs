module Main where

import Data.Char
import Data.Maybe
import Data.String
import Debug.Trace
import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


-- Assignment 1

-- Read chapter 4



-- Assignment 2
-- time: 2 hours

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)



maxSetSize = 50

-- Found on https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

generateSet :: IO (Set Int)
generateSet = getIntL maxSetSize maxSetSize >>= \xs -> return (list2set (take (head xs) (drop 1 xs)))

arbitrarySet :: (Arbitrary a, Ord a) => Gen (Set a)
arbitrarySet = do
                t <- arbitrary
                return (list2set t)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = arbitrarySet



-- Assignment 3
-- Time: 2:30 hours

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = Set []
intersectionSet (Set set1) (Set set2) = list2set (filter (\x -> inSet x (Set set1)) set2)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = Set []
differenceSet (Set set1) (Set set2) = list2set (set1 \\ set2)


-- Tests

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

xor :: Bool -> Bool -> Bool
xor x y = (x && not y) || (not x && y)

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

nor :: Bool -> Bool -> Bool
nor x y = not x && not y

unionSetContainsAllElements, unionSetContainsNoOtherElements :: (Ord a) => Set a -> Set a -> Set a -> Bool
unionSetContainsAllElements (Set a) (Set b) c = all (\x -> (inSet x (Set a) || inSet x (Set b)) --> inSet x c) (a ++ b)
unionSetContainsNoOtherElements (Set a) (Set b) c = all (\x -> (inSet x (Set a) `nor` inSet x (Set b)) == not (inSet x c)) (a ++ b)

intersectionSetContainsAllElements, intersectionSetContainsNoOtherElements :: (Ord a) => Set a -> Set a -> Set a -> Bool
intersectionSetContainsAllElements (Set a) (Set b) c = all (\x -> (inSet x (Set a) && inSet x (Set b)) --> inSet x c) (a ++ b)
intersectionSetContainsNoOtherElements (Set a) (Set b) c = all (\x -> (inSet x (Set a) `nand` inSet x (Set b)) == not (inSet x c)) (a ++ b)

differenceSetOnlyContainsElemsInA, differenceSetContainsNoElemsInB, differenceSetElementsInANotInBAndC :: (Ord a) => Set a -> Set a -> Set a -> Bool
differenceSetOnlyContainsElemsInA    a b (Set c) = all (\x -> inSet x a && not (inSet x b)) c
differenceSetContainsNoElemsInB      a (Set b) c =  all (\x -> not (inSet x c)) b
differenceSetElementsInANotInBAndC  (Set a) b c = all (\x -> inSet x b `nand` inSet x c) a

-- This tests if all elements that are in A or B, are also contained in the Union set and that there are not element in C which are not in A or B
testUnionSet = quickCheck((\a b -> unionSetContainsAllElements a b (unionSet a b) &&
  unionSetContainsNoOtherElements a b (unionSet a b)) :: Set Int -> Set Int -> Bool)

-- This tests if all elements that are in A and B, are also contained in the Intersection set and that there are not element in C which are not in A and B
testIntersectionSet = quickCheck((\a b -> intersectionSetContainsAllElements a b (intersectionSet a b) &&
  intersectionSetContainsNoOtherElements a b (intersectionSet a b)) :: Set Int -> Set Int -> Bool)

-- This test will determine whether the difference set is correct, by checking is all elements occur in either set A or set B, and not both
testDifferenceSet = quickCheck((\a b -> differenceSetOnlyContainsElemsInA a b (differenceSet a b) &&
  differenceSetContainsNoElemsInB a b (differenceSet a b) &&
  differenceSetElementsInANotInBAndC a b (differenceSet a b)) :: Set Int -> Set Int -> Bool)



-- Assignment 4

-- Read chapter 5



-- Assignment 5
-- Time: 30 minutes

type Rel a = [(a,a)]

inverseTuple :: (a,a) -> (a,a)
inverseTuple a = (snd a, fst a)

hasInverse :: Eq a => Rel a -> (a,a) -> Bool
hasInverse r t = inverseTuple t `elem` r

symClos :: (Ord a) => Rel a -> Rel a
symClos r = sort $ r ++ map inverseTuple (filter (not . hasInverse r) r)



-- Assignment 6
-- Time: 1:30 hours

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
 nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a) => Rel a -> Rel a
trClos closure = until (\x -> subSet (list2set (x @@ x)) (list2set x)) trClos (sort $ nub (closure ++ closure @@ closure))




-- Assignment 7
-- Time: 1 hour

hasAllTr :: Ord a => Rel a -> Rel a -> Bool
hasAllTr e r = subSet (list2set (e @@ r)) (list2set r)

isSym :: Eq a => Rel a -> Bool
isSym r = all (hasInverse r) r

isTr :: Ord a => Rel a -> Bool
isTr r = all (\x -> hasAllTr [x] r) r

testSymClos = quickCheck ((isSym . symClos) :: Rel (Int, Int) -> Bool)
testTrClos = quickCheck ((isTr . trClos) :: Rel (Int, Int) -> Bool)



-- Assignment 8
-- Time: 30 minutes

counterExample = [(0,1)]
testTrSymIsSymTr = quickCheck(symClos (trClos counterExample) /= trClos (symClos counterExample))
-- symClos (trClos counterExample
--    [(0,1),(1,0)]
-- trClos (symClos counterExample)
--    [(0,1),(1,0),(0,0),(1,1)]





main :: IO ()
main = do
    putStrLn "+++ Assignment 3 +++"
    putStrLn "Testing union set: "
    testUnionSet
    putStrLn "Testing intersection set: "
    testIntersectionSet
    putStrLn "Testing difference set: "
    testDifferenceSet

    putStrLn "\n+++ Assignment 7 +++"
    putStrLn "Testing symmetric closure: "
    testSymClos
    putStrLn "Testing transitive closure: "
    testTrClos
