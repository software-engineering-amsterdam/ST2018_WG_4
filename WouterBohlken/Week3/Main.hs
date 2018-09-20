module Main where

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
tautology f = all (`evl` f) (allVals f)

allEvals :: Form -> [Bool]
allEvals f = map (`evl` f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g = all (\ v -> evl v f --> evl v g) (allVals f)
-- First compare the length of both forms, return false if the lengths differ, in that case they cannot entail because the amount of props are different
-- Then use a lambda to loop though all values of f and an implies to g

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = allEvals f == allEvals g

pOrNotP = Dsj[p,Neg p]
pAndNotP = Cnj[p,Neg p]
pOrQ = Dsj[p,q]
pAndQ = Cnj[p,q]


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

main :: IO ()
main = do
  putStrLn "== Assignment 1 (Definitions of contradiction, tautology, entails and equiv) =="
  putStrLn $ "Testing if just a single atom (p) is a tautology (Expected: False): " ++ checkTestResult (not (tautology p))
  putStrLn $ "Testing if just a single atom (p) is a contradiction (Expected: False): " ++ checkTestResult (not (contradiction p))
  putStrLn $ "Testing if `p ∨ ¬p` is a tautology (Expected: True): " ++ checkTestResult (tautology pOrNotP)
  putStrLn $ "Testing if `p ∨ ¬p` is a contradiction (Expected: False): " ++ checkTestResult (not (contradiction pOrNotP))
  putStrLn $ "Testing if `p ∧ ¬p` is a tautology (Expected: False): " ++ checkTestResult (not(tautology pAndNotP))
  putStrLn $ "Testing if `p ∧ ¬p` is a contradiction (Expected: True): " ++ checkTestResult (contradiction pAndNotP)

  putStrLn $ "\nTesting if `p` logically entails `p` (Expected: True): " ++ checkTestResult (entails p p)
  putStrLn $ "Testing if `p` is logically equivalent to `p` (Expected: True): " ++ checkTestResult (equiv p p)
  putStrLn $ "Testing if `p` logically entails `p ∨ q` (Expected: True): " ++ checkTestResult (entails p pOrQ)
  putStrLn $ "Testing if `p` is logically equivalent to `p ∨ q` (Expected: False): " ++ checkTestResult (not(equiv p pOrQ))
  putStrLn $ "Testing if `p` logically entails `p ∧ q` (Expected: False): " ++ checkTestResult (not(entails p pAndQ))
  putStrLn $ "Testing if `p` is logically equivalent to `p ∧ q` (Expected: False): " ++ checkTestResult (not(equiv p pAndQ))
  putStrLn $ "Testing if `p ∧ q` logically entails `p` (Expected: True): " ++ checkTestResult (entails pAndQ p)
  putStrLn $ "Testing if `p ∧ q` is logically equivalent to `p` (Expected: False): " ++ checkTestResult (not(equiv pAndQ p))
