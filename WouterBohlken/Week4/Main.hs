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



maxSetSize = 50 :: Int

-- Found on https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

createSet :: [Int] -> Set Int
createSet xs = Set (rmdups xs)

generateSet :: IO (Set Int)
generateSet = getIntL maxSetSize maxSetSize >>= \xs -> return (createSet (take (head xs) (drop 1 xs)))



-- Assignment 3

-- Read chapter 5



-- Assignment 4




-- Assignment 5

-- type Rel a = [(a,a)]
-- symClos :: Ord a => Rel a -> Rel a
-- symClos a b =


-- Assignment 6

-- infixr 5 @@
--
-- (@@) :: Eq a => Rel a -> Rel a -> Rel a
-- r @@ s =
--  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
--
--trClos :: Ord a => Rel a -> Rel a



-- Assignment 7

-- tests



-- Assignment 8




-- Assignment 9





main :: IO ()
main = do
    putStrLn "empty"
