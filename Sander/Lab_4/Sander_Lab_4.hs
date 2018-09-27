-- NAME : Sander Meester
-- CKNUM : 11014822
-- STUDY : Master Software Engineering
-- COURSE : Software Specification, Verification and Testing
--
-- Lab3

module Lab3

where

import SetOrd
import Lecture4
import System.Random
import Test.QuickCheck
import System.IO.Unsafe
import Data.List
import Data.List.Unique
import Data.Char
import Debug.Trace

--Exercise 1
-- Chapter 4 of Haskell road
-- time spent: 60 mins

-- Questions:
--   - If R were a legitimate set, this would unavoidably lead us to the conclusion that R ∈ R ⇐⇒ R 6∈ R. I do not understand how that would lead us to that conclusion.

--Exercise 2
-- time spent: 60 mins
-- Creates random list (adaptation of function from previous lab)
randomList :: Int -> IO [Int]
randomList n = do
    g <- newStdGen
    return (take n (randoms g :: [Int]))

sortIOList :: IO [Int] -> IO [Int]
sortIOList is = do tmp <- is
                   return (sortUniq tmp)

getRandomSet :: Int -> IO (Set Int)
getRandomSet n = sortIOList(randomList n) >>= \x -> return (Set x)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = arbitrarySet

arbitrarySet :: (Arbitrary a, Ord a) => Gen (Set a)
arbitrarySet = do
  t <- arbitrary
  return (list2set t)


--Exercise 3
-- time spent:  mins

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set xs1) (Set xs2)  = Set (filter (`elem` xs2) xs1)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs1) (Set xs2)  = Set (filter (`notElem` xs2) xs1)

unionSet' :: (Ord a) => Set a -> Set a -> Set a
unionSet' (Set [])     set2  =  set2
unionSet' (Set (x:xs)) set2  =
   insertSet x (unionSet (Set xs) set2)

-- Testing

--Exercise 4
-- Chapter 5 of Haskell road
-- time spent: 60 mins

-- Questions:
--   -

--Exercise 5
-- time spent: 15 mins

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = sortUniq(a ++ [(y,x) | (x,y) <- a])

--Exercise 6
-- time spent: 30 mins
-- Only saw the @@ definition later, which would have sped up the exercize

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos a
    | a == currentTrClos = a
    | otherwise = trClos currentTrClos
    where currentTrClos = sortUniq(a ++ [(x,z) | (x,y) <- a, (y,z) <- a, ((elem (x,y) a) && (elem (y,z) a))])

--Exercise 7
-- time spent:  mins

-- instance (Arbitrary a) => Arbitrary (Rel a) where
--   arbitrary = arbitraryRel
--
-- arbitraryRel :: (Arbitrary a, Ord a) => Gen (Rel a)
-- arbitraryRel = do
--   ts <- arbitrary
--   return (ts)

-- property that checks if every tuple in symclos a also has its sym in symclos a
propSymClosSym :: Rel Int -> Bool
propSymClosSym a = trace ("My relation: " ++ show a) (all (\(x,y) -> elem (y,x) (symClos a)) (symClos a))

-- property that checks if every tuple in a also is in symClos a
propSymClosIn :: Rel Int -> Bool
propSymClosIn a = all (\(x,y) -> elem (y,x) (symClos a)) a


--Exercise 8
-- time spent:  mins

-- No: e.g.:
-- let rel1 = [(1,2),(2,3),(3,3)]
-- symClos(trClos(rel1)) == trClos(symClos(rel1))
--  -False
