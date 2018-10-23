module Main where

import Data.Char
import Data.List

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
count (T x nodes) = length nodes + foldr (\x acc -> acc + count x) 0 nodes

-- Question 4
depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

-- Question 5
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x nodes) = T (f x) (map (mapT f) nodes)

-- Question 7
collect :: Tree a -> [a]
collect (T x nodes) = x : foldr (\x acc -> collect x++acc) [] nodes

-- Question 8
foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' x = foldT (\i j -> 1 + sum j) x - 1

depth' :: Tree a -> Int
depth' (T _ []) = 0
depth' l = foldT (\x y -> if not (null y) then head y + 1 else 0) l

collect' :: Tree a -> [a]
collect' = foldT (\x y -> x:concat y)

main :: IO ()
main = putStrLn "Example tree size: "
