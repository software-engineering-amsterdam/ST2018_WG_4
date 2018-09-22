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
equiv f g = all (\ v -> evl v f == evl v g) (allVals f)

pOrNotP = Dsj[p,Neg p]
pAndNotP = Cnj[p,Neg p]
pOrQ = Dsj[p,q]
pAndQ = Cnj[p,q]

checkTestResult :: Bool -> String
checkTestResult True  = "\x1b[32mTest succeeded!\x1b[0m"
checkTestResult False = "\x1b[31mTest failed!\x1b[0m"

-- Assignment 2



-- Assignment 3
-- Time: 3 hours
-- TODO: Include arrowFree and nnf before the rules below
-- cnf :: Form -> Form
-- cnf (Prop x)                            = Prop x
-- cnf (Neg (Prop x))                      = Neg (Prop x)
--
-- cnf (Dsj [Prop p, Prop q])              = Dsj [Prop p, Prop q]
-- cnf (Dsj [Neg (Prop p), Prop q])        = Dsj [Neg (Prop p), Prop q]
-- cnf (Dsj [Prop p, Neg (Prop q)])        = Dsj [Prop p, Neg (Prop q)]
-- cnf (Dsj [Neg (Prop p), Neg (Prop q)])  = Dsj [Neg (Prop p), Neg (Prop q)]
--
-- cnf (Cnj [Prop p, Prop q])              = Cnj [Prop p, Prop q]
-- cnf (Cnj [Neg (Prop p), Prop q])        = Cnj [Neg (Prop p), Prop q]
-- cnf (Cnj [Prop p, Neg (Prop q)])        = Cnj [Prop p, Neg (Prop q)]
-- cnf (Cnj [Neg (Prop p), Neg (Prop q)])  = Cnj [Neg (Prop p), Neg (Prop q)]
--
--  -- De morgan law
-- cnf (Cnj [Neg x, Neg y])                = cnf (Neg (Cnj [x, y]))
--
-- cnf (Dsj [Cnj [x1, y1], Cnj [x2, y2] ]) = cnf (Cnj [Dsj [x1, x2], Dsj [y1, y2] ])
--
-- -- Distributive property
-- -- Rule of replacement
-- cnf (Dsj [Dsj [q, r], p])               = cnf (Dsj [cnf (Dsj [p, q]), cnf (Dsj [p, r])])
-- cnf (Dsj [Neg (Dsj [q, r]), p])         = cnf (Dsj [cnf (Dsj [p, q]), cnf (Dsj [p, r])])
-- -- cnf (Dsj [Cnj [q, r], p])               = cnf (Cnj [cnf (Dsj [p, q]), cnf (Dsj [p, r])])
-- -- cnf (Dsj [p, Cnj [q, r]])               = cnf (Cnj [cnf (Dsj [p, q]), cnf (Dsj [p, r])])
-- cnf (Dsj [p, Neg (Cnj [q, r])])         = cnf (Cnj [cnf (Dsj [p, Neg q]), cnf (Dsj [p, Neg r])])
-- cnf (Dsj [Neg (Cnj [q, r]), p])         = cnf (Cnj [cnf (Dsj [p, Neg q]), cnf (Dsj [p, Neg r])])
--
--
-- -- Distribution of conjunction over conjunction
-- cnf (Cnj [p, Cnj [q, r]])               = cnf (Cnj [ cnf (Cnj [p, q]), cnf (Cnj [p, r])])
-- cnf (Cnj [Cnj [q, r], p])               = cnf (Cnj [ cnf (Cnj [p, q]), cnf (Cnj [p, r])])
--
-- -- Leaf
-- cnf (Cnj [p, q])                        = Cnj [p, q]

valuationToClause :: Valuation -> Form
valuationToClause v = Dsj (map (\x -> if snd x then Prop (fst x) else Neg (Prop (fst x))) v)

falseEvals :: Form -> [Valuation]
falseEvals f = filter (\x -> not (evl x f)) (allVals f)

cnf :: Form -> Form
cnf f = Cnj (map valuationToClause (falseEvals f))



form4 = Dsj [r, Cnj [p, q]]




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
