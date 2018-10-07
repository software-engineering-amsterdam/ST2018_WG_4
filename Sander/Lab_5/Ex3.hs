-- NAME : Sander Meester
-- CKNUM : 11014822
-- STUDY : Master Software Engineering
-- COURSE : Software Specification, Verification and Testing
--
-- Lab5

module Ex3

where

import Lecture5
import System.Random
import Test.QuickCheck
import Data.List
import Data.Char
import Debug.Trace

import Control.Conditional
import Control.Monad

--Exercise 3
-- time 180 mins
genNonSolvedSudoku :: IO Node
genNonSolvedSudoku = (genRandomSudoku  >>=  genProblem)

checkWithElemRemoved :: Node -> IO Bool
checkWithElemRemoved nd = do
       let fp = filledPositions (fst nd)
       let erasedSuds = map (eraseN nd) fp
       return (all not $ map uniqueSol erasedSuds)

checkSudMinimal :: IO ()
checkSudMinimal = do
        nd <- genNonSolvedSudoku
        quickCheck(uniqueSol nd)
        toCheck <- checkWithElemRemoved nd
        quickCheck(toCheck)
