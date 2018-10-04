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
-- Simon:
-- Page 122: What does the keyword `undefined` do in Haskell?
-- Page 145: What are the implications of having an atom as your factor?
-- Page 145: How do "logical translations" work?
--
-- Sander:
-- If R were a legitimate set, this would unavoidably lead us to the conclusion that R ∈ R ⇐⇒ R 6∈ R. I do not understand how that would lead us to that conclusion.

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
-- I have defined 10 properties. Each property's documentation is documented in comment above the property itself. Each test function expect a certain set of properties to be true.

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
-- Checks for all elements in the first set, that if the element is NOT in the second set then it should be in the resulting set.
propOnlyInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` rightSet --> x `elem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the first set, that if the element is in the second set then it should be in the resulting set.
propOnlyNotInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyNotInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `elem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the first set, that if the element is NOT in the second set then it should NOT be in the resulting set.
propNotOnlyInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` rightSet --> x `notElem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the first set, that if the element is in the second set then it should NOT be in the resulting set.
propNotOnlyNotInLeftSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyNotInLeftSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `notElem` newSet) leftSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- RIGHT SET
-- Checks for all elements in the second set, that if the element is NOT in the first set then it should be in the resulting set.
propOnlyInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the second set, that if the element is in the first set then it should be in the resulting set.
propOnlyNotInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propOnlyNotInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the second set, that if the element is NOT in the first set then it should NOT be in the resulting set.
propNotOnlyInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `notElem` leftSet --> x `notElem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in the second set, that if the element is NOT in the first set then it should NOT be in the resulting set.
propNotOnlyNotInRightSet :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propNotOnlyNotInRightSet setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` leftSet --> x `notElem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- BOTH SETS
-- Checks for all elements in that exist in both sets that they are also in the resulting set.
propInBothSets :: Ord a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
propInBothSets setFunction (Set leftSet) (Set rightSet) = all (\x -> x `elem` rightSet --> x `elem` newSet) leftSet && all (\x -> x `elem` leftSet --> x `elem` newSet) rightSet
  where newSet = set2list (setFunction (Set leftSet) (Set rightSet))

-- Checks for all elements in that exist in both sets that they are NOT in the resulting set.
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
-- Simon:
-- This chapter was really though to get through. The examples were hard to follow and I could solve few of the exercises without looking at the solutions. In the end I did not have time to further solve any exercises.
-- Page 174: What is a "Real Plane"? Why is R2 called a "real plane"?
-- Page 180 & 182: What is a delta set? --> This was answered later on, as it is the identity of a certain set.
-- Example 5.63 is completely unclear to me.
-- Page 208: What does "having a common ancestor down the male line" mean?
-- Page 208 & 209: How to work with stirling set numbers?
-- Page 211: How does a quotient work?
-- Page 211: How to determine the number of elements in an atom? Or does |a| mean something else in this formula?
-- 
-- Wouter
-- p 172, "To see that R+ is transitive, assume xR + y and yR+z", how can you assume this, when we have to prove transitivity? Seems to me that the assumption is that R+ is transitive
-- Example 5.103 is unclear to me

-- Sander:
-- Show that a relation R on a set A is symmetric iff ∀x, y ∈ A(xRy ⇔ yRx).
-- - This is the definition of a symmetric relation, how would one go on to show that that is indeed so?

-- To show that R is the smallest relation S that has all the properties in
-- O, show the following:
-- 1. R has all the properties in O,
-- 2. If S has all the properties in O, then R ⊆ S.
-- - I dont get what the point of this is

-- Show that an intersection of arbitrarily many transitive relationsis transitive.
-- - I can intuitively see that this is true, but how would one formally prove it?

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
trClos clos
  | clos == x = sort clos
  | otherwise = trClos x
  where x = nub $ clos ++ (clos @@ clos)

-- Assignment 7
-- Time: 45 minutes
-- After defining the properties for the relations, they can be tested by QuickCheck. This is because `arbitrary` can automatically create random parameters of type `[(Int,Int)]`. By making the test methods accept a Rel Int, QuickCheck can automatically generate random parameters.

-- The property that are reversed relations are in the resulting relation.
propAllReversed :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propAllReversed relFunction rel = let applied = relFunction rel in all (\(x,y) -> (y,x) `elem` applied) rel

-- The property that are original (non-reversed) relations are in the resulting relation.
propAllOriginal :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propAllOriginal relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) rel

-- The property that the direct (one step) links of a transitive closure are in the resulting relation.
propFirstClosure :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propFirstClosure relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) (rel @@ rel)

-- The property that the indirect (two step) links of a transitive closure are in the resulting relation.
propSecondClosure :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
propSecondClosure relFunction rel = let applied = relFunction rel in all (\(x,y) -> (x,y) `elem` applied) (rel @@ applied)

-- Tests the symmetric closure by the `propAllReversed` and `propAllOriginal` properties (see the properties themselves for more information about them).
testSymmetricClosure :: Rel Int -> Bool
testSymmetricClosure x = propAllReversed symClos x && propAllOriginal symClos x

-- Tests the transitive closure by the `propAllOriginal`, `propFirstClosure` and `propSecondClosure` properties (see the properties themselves for more information about them).
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
  generateRandomSetInt >>= \x -> putStrLn $ "My random set: " ++ show x
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
