module Lab3 where

import Lecture3
import Data.List
import System.Random
import Test.QuickCheck


-- | If all are false then it is an contradiction, else not
contradiction :: Form -> Bool
contradiction f = all (\v -> not (evl v f)) (allVals f)

-- | If all are true then it is an tautology, else not
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f ) (allVals f)

genTruthTable :: Form -> [Bool]
genTruthTable f = map (\x -> evl x f) (allVals f)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = genTruthTable f1 == genTruthTable f2

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = all (==True) (map (\(x, y) -> if (x --> y) then True else False) (zip (genTruthTable f1) (genTruthTable f2)))
