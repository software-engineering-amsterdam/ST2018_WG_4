module Main where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd

import Data.Char
import Data.Maybe
import Data.String

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

createSet :: (Ord a) => [a] -> Set a
createSet xs = Set (rmdups xs)

generateSet :: IO (Set Int)
generateSet = getIntL maxSetSize maxSetSize >>= \xs -> return (createSet (take (head xs) (drop 1 xs)))

arbitrarySet :: (Arbitrary a, Ord a) => Gen (Set a)
arbitrarySet = do
                t <- arbitrary
                return (list2set t)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = arbitrarySet


-- Assignment 3

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = Set []
intersectionSet (Set set1) (Set set2) = createSet (filter (\x -> inSet x (Set set1)) set2)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = Set []
differenceSet (Set set1) (Set set2) = createSet ((set1 \\ set2) ++ (set2 \\ set1))

-- Assignment 4

-- Read chapter 5



-- Assignment 5
-- Time: 30 minutes


type Rel a = [(a,a)]

inverseTuple :: (a,a) -> (a,a)
inverseTuple a = (snd a, fst a)

hasInverse :: Eq a => Rel a -> (a,a) -> Bool
hasInverse r t = inverseTuple t `elem` r

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r ++ map inverseTuple (filter (not . hasInverse r) r)


-- Assignment 6


infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
 nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a



-- Assignment 7

isSym :: Eq a => Rel a -> Bool
isSym r = all (hasInverse r) r

-- testSym = quickCheck(isSym generateSet)

-- tests



-- Assignment 8




-- Assignment 9





main :: IO ()
main = do
    putStrLn "empty"
