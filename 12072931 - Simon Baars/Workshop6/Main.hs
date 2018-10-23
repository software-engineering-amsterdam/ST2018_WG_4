module Main where

import Data.Char
import Data.List
import Test.QuickCheck

-- Question 1
data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

leafCount :: Blt a -> Int
leafCount (Leaf node) = 1
leafCount (Node node1 node2) = leafCount node1 + leafCount node2

-- Question 2
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf node) = Leaf (f node)
mapB f (Node node1 node2) = Node (mapB f node1) (mapB f node2)

-- Question 3
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]
example3 = T 0 [example2,example1,example2,example2,example1,example2]

count :: Tree a -> Int
count (T x nodes) = foldr (\x acc -> acc + count x) 1 nodes

-- Question 4
depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

-- Question 5
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x nodes) = T (f x) (map (mapT f) nodes)

-- Question 7
collect2 :: Tree a -> [a]
collect2 (T x nodes) = x : foldr (\x acc -> collect2 x++acc) [] nodes

-- Question 8
foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' = foldT (\i j -> 1 + sum j)

depth' :: Tree a -> Int
depth' (T _ []) = 0
depth' l = foldT (\x y -> if not (null y) then head y + 1 else 0) l

collect' :: Tree a -> [a]
collect' = foldT (\x y -> x:concat y)

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f = foldT (\x y -> T (f x) y)

-- Question 9
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

growCountExample :: Int -> Int
growCountExample n = count (grow (\x -> if x < n then [x+1, x+1] else []) 0)

propTreeGrowthFormula :: Positive Int -> Bool
propTreeGrowthFormula (Positive n) = n<20 --> 2^(n+1)-1 == growCountExample n

-- Question 10
infTree :: Tree Integer
infTree = grow (\ n -> [n+1,n+1]) 0

takeT :: Int -> Tree a -> Tree a
takeT 0 (T a l) = T a []
takeT x (T a l) = T a (map (takeT $ x-1) l)

-- Question 11
tree2list :: Tree a -> [a]
tree2list (T e trees) = e : concatMap tree2list trees

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

tree n = grow (step1list n) (1,1)
step1list n (x,y) = [(x, y)| x<-[1..n], y<-[1..n], coprime x y]

tree2 n = grow (step2 n) (1,1)
step2 n (x,y) = if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

propListEqual :: Integer -> Bool
propListEqual n = n>0 --> sort (tree2list (tree2 n)) == sort(step1list n (1,1))

main :: IO ()
main = putStrLn "Hello world"
