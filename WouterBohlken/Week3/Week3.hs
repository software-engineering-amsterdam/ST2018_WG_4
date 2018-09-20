module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

import Data.Char
import Data.Maybe
import Data.String

-- Assignment 1
-- Time: 1 hour

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

allEvals :: Form -> [Bool]
allEvals f = map (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g = all (\ v -> evl v f --> evl v g) (allVals f)
-- First compare the length of both forms, return false if the lengths differ, in that case they cannot entail because the amount of props are different
-- Then use a lambda to loop though all values of f and an implies to g

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = allEvals f == allEvals g

checkTestResult :: Bool -> String
checkTestResult True  = "\x1b[32mTest succeeded!\x1b[0m"
checkTestResult False = "\x1b[31mTest failed!\x1b[0m"

-- Assignment 2



-- Assignment 3
-- Time: 1 hour
-- TODO: Include arrowFree and nnf before the rules below
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj [Neg x, Neg y]) = Neg (Cnj [x, y]) -- De morgan law
cnf (Dsj [ Cnj [x1, y1], Cnj [x2, y2] ]) = Cnj [ Dsj [x1, y1], Dsj [x2, y2] ] -- Distributive property
