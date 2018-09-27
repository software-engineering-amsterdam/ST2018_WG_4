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
-- time: 1:30 hours

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

-- intersectionSet1ElemNotContainedInSet2

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = Set []
intersectionSet (Set set1) (Set set2) = list2set (filter (\x -> inSet x (Set set1)) set2)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = Set []
differenceSet (Set set1) (Set set2) = list2set ((set1 \\ set2) ++ (set2 \\ set1))



-- Assignment 4

-- Read chapter 5



-- Assignment 5
-- Time: 30 minutes

type Rel a = [(a,a)]

inverseTuple :: (a,a) -> (a,a)
inverseTuple a = (snd a, fst a)

hasInverse :: Eq a => Rel a -> (a,a) -> Bool
hasInverse r t = inverseTuple t `elem` r

symClos :: (Eq a, Ord a) => Rel a -> Rel a
symClos r = sort $ r ++ map inverseTuple (filter (not . hasInverse r) r)



-- Assignment 6
-- Time: 1:30 hours

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
 nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Eq a) => Rel a -> Rel a
trClos closure  | subSet (list2set (closure @@ closure)) (list2set closure) = closure
                | otherwise = trClos (nub (closure ++ (closure @@ closure)))



-- Assignment 7

hasAllTr :: Ord a => Rel a -> Rel a -> Bool
hasAllTr e r = subSet (list2set (e @@ r)) (list2set r)

isSym :: Eq a => Rel a -> Bool
isSym r = all (hasInverse r) r

isTr :: Ord a => Rel a -> Bool
isTr r = all (\x -> hasAllTr [x] r) r

testSymClos = quickCheck ((isSym . symClos) :: Rel (Int, Int) -> Bool)
testTrClos = quickCheck ((isTr . trClos) :: Rel (Int, Int) -> Bool)

-- Assignment 8

counterExample = [(0,1)]
testTrSymIsSymTr = quickCheck(symClos (trClos counterExample) /= trClos (symClos counterExample))
-- symClos (trClos counterExample
--    [(0,1),(1,0)]
-- trClos (symClos counterExample)
--    [(0,1),(1,0),(0,0),(1,1)]


-- Assignment 9





main :: IO ()
main = do
    putStrLn "empty"
    putStrLn "empty"
