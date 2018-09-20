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


-- Assignment 2


-- Assignment 3
-- TODO: Include arrowFree and nnf before the rules below
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj ([Neg x, Neg y])) = Neg (Cnj [x, y]) -- De morgan law
cnf (Dsj [ (Cnj [x1, y1]), (Cnj [x2, y2]) ]) = (Cnj [ (Dsj [x1, y1]), (Dsj [x2, y2]) ]) -- Distributive property

-- arrowfree :: Form -> Form
-- arrowfree (Prop x) = Prop x
-- arrowfree (Neg f) = Neg (arrowfree f)
-- arrowfree (Cnj fs) = Cnj (map arrowfree fs)
-- arrowfree (Dsj fs) = Dsj (map arrowfree fs)
-- arrowfree (Impl f1 f2) =
--   Dsj [Neg (arrowfree f1), arrowfree f2]
-- arrowfree (Equiv f1 f2) =
--   Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
--   where f1' = arrowfree f1
--         f2' = arrowfree f2
--
-- nnf :: Form -> Form
-- nnf (Prop x) = Prop x
-- nnf (Neg (Prop x)) = Neg (Prop x)
-- nnf (Neg (Neg f)) = nnf f
-- nnf (Cnj fs) = Cnj (map nnf fs)
-- nnf (Dsj fs) = Dsj (map nnf fs)
-- nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
-- nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
