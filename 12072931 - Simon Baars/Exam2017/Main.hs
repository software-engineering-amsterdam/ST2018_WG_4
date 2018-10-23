module Main where

import Test.QuickCheck
import Data.List
import Data.Maybe

-- Assignment 4A
type Rel a = [(a,a)]

isLinear :: Eq a => [a] -> Rel a -> Bool
isLinear x y = all (\a -> all (\b -> a == b || (a,b) `elem` y || (b,a) `elem` y) x) x

-- Assignment 4B
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

propEmptyAndSingletonListLinear :: [Int] -> [(Int, Int)] -> Bool
propEmptyAndSingletonListLinear x y = length x < 2 --> isLinear x y

propRelationsBetweenDifferentElements :: [Int] -> [(Int, Int)] -> Bool
propRelationsBetweenDifferentElements x y = length x >= 2 && all (uncurry (==)) y --> not(isLinear x y)

propMinimalAmountOfPairsWithDifferentElements :: [Int] -> [(Int, Int)] -> Bool
propMinimalAmountOfPairsWithDifferentElements x y = length x >= 2 && length(filter (uncurry (/=)) y) < length x --> not(isLinear x y)

-- Assignment 5A
jos :: Int -> Int -> Int -> Int
jos n k 0 = rem (k-1) n
jos n k i = rem (k + jos (n-1) k (i-1)) n

propFirstElementIsZero :: Int -> Bool
propFirstElementIsZero k = jos 1 k 0 == 0

propILowerThanKLessThanOrEqualToZero :: Int -> Int -> Bool
propILowerThanKLessThanOrEqualToZero n i = i < n && i>=0 --> jos n 0 i <= 0

-- Assignment 5B
meeny :: Int -> Int -> [String] -> String
meeny k i list = list !! jos (length list) k (i + 1)

propChooseFirstPerson :: Int -> [String] -> Bool
propChooseFirstPerson k s = not (null s) --> meeny k 1 s == head s

propEeny :: Int -> Int -> Bool
propEeny n i = i < n && i>=0 --> jos n 0 i <= 0

-- Assignment 6
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a
                     else fGcd b (rem a b)

fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
  if b == 0
  then (1,0)
  else
     let
       (q,r) = quotRem a b
       (s,t) = fctGcd b r
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

tree2list :: Tree a -> [a]
tree2list (T e trees) = e : concatMap tree2list trees

tree1 n = grow (step1 n) (1,1)
step1 n (x,y) = if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function
tree2 n = grow (step2 n) (1,1)
step2 n (x,y) = if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

step1list n (x,y) = [(x, y)| x<-[1..n], y<-[1..n], coprime x y]

propListEqual :: Integer -> Bool
propListEqual n = n>0 --> sort (tree2list (tree1 n)) == sort (step1list n (1,1))

propList2Equal :: Integer -> Bool
propList2Equal n = n>0 --> sort (tree2list (tree2 n)) == sort(step1list n (1,1))

-- Assignment 7
pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

tree n m = grow (stepCoprime n m) (1,1)

stepCoprime :: Integer -> Integer -> (Integer, Integer) -> [(Integer, Integer)]
--stepCoprime n m (x,y) = [(i,j) | i <- [x+1..m], j <- [y+1..n], coprime i j, i<=j] -- step function
stepCoprime n m t = let index = fromJust $ elemIndex t coprimes in [coprimes !! (index + 1) | fromIntegral index<n-1]

propStepCoprimeContainsAllCoprimes :: Integer -> Integer -> Bool
propStepCoprimeContainsAllCoprimes n m = n>0 --> (let treeList = tree2list (tree n m) in treeList == take (fromIntegral n) coprimes)

-- Assignment 8
probWin1 n i p = let q = 1 - p in p^(n-i) * sum [ p^j * q^(i-j-1) | j <- [0..i-1] ] / sum [ p^j * q^(n-j-1) | j <- [0..n-1] ]
probWin2 n i p = let r = (1-p)/p in sum [ r^j | j <- [0..i-1] ] / sum [ r^j | j <- [0..n-1]]
probWin3 n i p = let q = 1 - p in (1 - (q/p)^i)/(1-(q/p)^n)

newtype ChanceDouble = ChanceDouble Double deriving (Eq, Ord, Read, Show)

instance Arbitrary ChanceDouble where
    arbitrary = abs `fmap` (arbitrary :: Gen Double) `suchThat` (\x -> x>=0 && x<=1) >>= \x -> return (ChanceDouble x)

propBetween0and1 :: (Integer -> Integer -> Double -> Double) -> Integer -> Integer -> ChanceDouble -> Bool
propBetween0and1 f n i (ChanceDouble p) = n >= 1 && i >= 0 && i<=n --> let result = f n i p in result >= 0 && result<=1

main :: IO ()
main = do
  putStrLn "\x1b[36m== Assignment 4 ==\x1b[0m"
  quickCheck propEmptyAndSingletonListLinear
  quickCheck propRelationsBetweenDifferentElements
  quickCheck propMinimalAmountOfPairsWithDifferentElements

  putStrLn "\x1b[36m== Assignment 5 ==\x1b[0m"
  quickCheck propFirstElementIsZero
  quickCheck propILowerThanKLessThanOrEqualToZero

  putStrLn "\x1b[36m== Assignment 6 ==\x1b[0m"
  quickCheck propListEqual
  quickCheck propList2Equal

  putStrLn "\x1b[36m== Assignment 7 ==\x1b[0m"
  quickCheck propStepCoprimeContainsAllCoprimes

  putStrLn "\x1b[36m== Assignment 8 ==\x1b[0m"
  quickCheck (propBetween0and1 probWin1)
  quickCheck (propBetween0and1 probWin2)
  quickCheck (propBetween0and1 probWin3)
