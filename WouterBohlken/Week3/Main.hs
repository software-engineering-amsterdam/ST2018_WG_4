module Main where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

import Data.Char
import Data.Maybe
import Data.String

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

-- Testing the parser using the form generator of assignment 4
-- Use and tweak Simon's parser test, due to time shortage
-- We test whether the parser gives exactly 1 match for a given generated form, and thus it is not ambiguous, then we test whether the parsed formula is equivalent to it's initial counterpart
parserTest :: Int -> Int -> IO ()
parserTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateForm >>= \TreeEnvironment {form = x} -> let resultingForm = parse (show x) in if length resultingForm == 1 && equiv x (head resultingForm) then
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)


-- Assignment 3
-- Time: 4 hours
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

valuationToDsjClause :: Valuation -> Form
valuationToDsjClause v = Dsj (map (\x -> if snd x then Neg (Prop (fst x)) else Prop (fst x)) v)

falseEvals :: Form -> [Valuation]
falseEvals f = filter (\x -> not (evl x f)) (allVals f)

cnf :: Form -> Form
cnf f = Cnj (map valuationToDsjClause (falseEvals f))


-- Bonus: DNF
valuationToCnjClause :: Valuation -> Form
valuationToCnjClause v = Cnj (map (\x -> if snd x then Prop (fst x) else Neg (Prop (fst x))) v)

trueEvals :: Form -> [Valuation]
trueEvals f = filter (`evl` f) (allVals f)

dnf :: Form -> Form
dnf f = Dsj (map valuationToCnjClause (trueEvals f))



-- Assignment 4
-- Time: 4 hours

maxTotalProps = 5
maxForms = 10
maxPropertiesInForm = 4

data TreeEnvironment = TreeEnvironment {form :: Form, randomCounter :: Int, randomNumbers :: [Int]} deriving (Eq)

instance Show TreeEnvironment where
  show (TreeEnvironment form rc rn) = show form

shiftRand :: Int -> TreeEnvironment -> TreeEnvironment
shiftRand n TreeEnvironment {form = f, randomCounter = rc, randomNumbers = rn} = TreeEnvironment f (rc+n) rn

getRand :: TreeEnvironment -> Int
getRand TreeEnvironment {form = f, randomCounter = rc, randomNumbers = rn} = rn!!rc

getRands :: Int -> [Int] -> [Int]
getRands n xs = take ((xs!!n `mod` (maxPropertiesInForm - 2)) + 2) $ drop n xs

getRandsForTreeEnv :: TreeEnvironment -> [Int]
getRandsForTreeEnv TreeEnvironment {form = f, randomCounter = rc, randomNumbers = rn} = getRands rc rn

getProp :: Int -> Form
getProp n = Prop ((n `mod` maxTotalProps) + 1)

getImpl, getEquiv :: Int -> Int -> Form
getImpl p1 p2 = Impl (getProp p1) (getProp p2)
getEquiv p1 p2 = Equiv (getProp p1) (getProp p2)

getCnj, getDsj :: [Form] -> Form
getCnj = Cnj
getDsj = Dsj

maybeNegate :: Int -> Form -> Form
maybeNegate n f | n `mod` 3 == 0 = Neg f
                | otherwise = f

maybeFlip :: Int -> (Form, Form) -> (Form, Form)
maybeFlip n f | n `mod` 3 /= 0 = f
               | otherwise = (snd f, fst f)

addFormToForm :: Int -> Int -> Form -> Form -> Form
addFormToForm neg n f1 f2 = case n `mod` 4 of
                            0 -> maybeNegate neg (Cnj [ff1, ff2])
                            1 -> maybeNegate neg (Dsj [ff1, ff2])
                            2 -> maybeNegate neg (Impl ff1 ff2)
                            3 -> maybeNegate neg (Equiv ff1 ff2)
                            where
                                form = maybeFlip(n+1) (maybeNegate (neg+n) f1, maybeNegate (neg+n+1) f2)
                                ff1 = fst form
                                ff2 = snd form

addToFormEnvironment :: TreeEnvironment -> Form -> TreeEnvironment
addToFormEnvironment TreeEnvironment {form = tf, randomCounter = rc, randomNumbers = rn} nf = TreeEnvironment (addFormToForm (rn!!rc) (rn!!rc) tf nf) (rc+2) rn

maybeAddRandomForm :: TreeEnvironment -> TreeEnvironment
maybeAddRandomForm te   | getRand te `mod` 2 == 0 = addRandomForm newForm
                        | otherwise = newForm
                        where newForm = shiftRand 1 te

addRandomForm :: TreeEnvironment -> TreeEnvironment
addRandomForm te = case getRand te `mod` 4 of
                    0 -> addToFormEnvironment newEnv (maybeNegate fstRand (getImpl sndRand trdRand))
                    1 -> addToFormEnvironment newEnv (maybeNegate fstRand (getEquiv sndRand trdRand))
                    2 -> addToFormEnvironment nShiftedEnv (getCnj mappedProps)
                    3 -> addToFormEnvironment nShiftedEnv (getDsj mappedProps)
                    where
                        fstRand = getRand (shiftRand 1 te)
                        sndRand = getRand (shiftRand 2 te)
                        trdRand = getRand (shiftRand 3 te)
                        mappedProps = map getProp (getRandsForTreeEnv (shiftRand 1 te))
                        newEnv = maybeAddRandomForm (shiftRand 4 te)
                        nShiftedEnv = shiftRand (getRand (shiftRand 1 te)) te

addRandomForms :: Int -> TreeEnvironment -> TreeEnvironment
addRandomForms 0 te = te
addRandomForms n te = addRandomForms (n-1) (shiftRand 1 (addRandomForm te))

randomRoot :: [Int] -> Form
randomRoot xs = case head xs `mod` 4 of
                0 -> getImpl (xs!!1) (xs!!2)
                1 -> getEquiv (xs!!1) (xs!!2)
                2 -> getCnj (map getProp (getRands 0 xs))
                3 -> getDsj (map getProp (getRands 0 xs))

createTreeEnvironment :: Form -> [Int] -> TreeEnvironment
createTreeEnvironment f = TreeEnvironment f 0

generateForm :: IO TreeEnvironment
generateForm = getIntL maxPropertiesInForm (maxForms * 30) >>= \rands -> return (addRandomForms ((head rands `mod` (maxForms - 1)) + 1) (createTreeEnvironment (randomRoot rands) (drop 5 rands)))

addForm :: [IO TreeEnvironment] -> [IO TreeEnvironment]
addForm tes = tes ++ [generateForm]

generateForms :: Int -> [IO TreeEnvironment] -> [IO TreeEnvironment]
generateForms 0 fs = []
generateForms n fs = generateForms (n-1) (addForm fs)


-- We test whether the CNF forms are equivalent to their initial counter part
cnfTest :: Int -> Int -> IO ()
cnfTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateForm >>= \TreeEnvironment {form = x} -> let cnfForm = cnf x in if equiv x cnfForm then
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)


-- We test whether the CNF forms are equivalent to their initial counter part
dnfTest :: Int -> Int -> IO ()
dnfTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateForm >>= \TreeEnvironment {form = x} -> let dnfForm = dnf x in if equiv x dnfForm then
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)


-- Sanders cnf2cls function, I didn't implement my own, due to time shortage
type Clause  = [Int]
type Clauses = [Clause]

lexNumCls cs = TokenInt (read num) : lexerCls rest
     where (num,rest) = span isDigit cs

lexerCls :: String -> [Token]
lexerCls [] = []
lexerCls (c:cs) | isSpace c = lexerCls cs
             | isDigit c = lexNumCls (c:cs)
lexerCls ('[':cs) = [TokenCnj, TokenOP] ++ lexerCls cs
lexerCls (']':cs) = TokenCP : lexerCls cs
-- lexerCls (']':',':'[':cs) = [TokenOP, TokenCnj, TokenCP] ++ lexerCls cs
lexerCls (',':cs) = [TokenDsj, TokenOP] ++ lexerCls cs ++ [TokenCP]
lexerCls ('-':cs) = TokenNeg : lexerCls cs
lexerCls (x:_) = error ("unknown token: " ++ [x])

parseCls :: String -> [Form]
parseCls s = [ f | (f,_) <- parseForm (lexerCls s) ]

convertToCNF :: Form -> Form
convertToCNF = cnf

-- Converts a single element (p or Neg p) to a Clause
convertPropToClause :: Form -> Clause
convertPropToClause (Prop name) = [name]
convertPropToClause (Neg (Prop name)) = [-name]

-- Converts conjunctions and disjunctions to Clauses
convertToClauses :: Form -> Clauses
convertToClauses (Cnj [Dsj fs1, Dsj fs2]) = [concatMap convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [fs1, Dsj fs2]) = [convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [Dsj fs1, fs2]) = [concatMap convertPropToClause fs1, convertPropToClause fs2]
convertToClauses (Cnj fs) = [concatMap convertPropToClause fs]
convertToClauses (Dsj fs) = [concatMap convertPropToClause fs]

-- Convert a form to a Clause format
cnf2cls :: Form -> Clauses
cnf2cls = convertToClauses

-- Convert a form to a CNF to a Clause format
formToCls :: Form -> Clauses
formToCls f = cnf2cls(convertToCNF f)

-- Returns number of negative atoms in a form
negAtoms :: Form -> Int -> Int
negAtoms (Prop name) i = i
negAtoms (Neg f) i = negAtoms f (i+1)
negAtoms (Cnj fs) i = sum(map (`negAtoms` i) fs)
negAtoms (Dsj fs) i = sum(map (`negAtoms` i) fs)
negAtoms (Impl f1 f2) i = sum(map (`negAtoms` i) [f1,f2])
negAtoms (Equiv f1 f2) i = sum(map (`negAtoms` i) [f1,f2])

-- Returns number of negative atoms in a clause
negDigs :: Clauses -> Int
negDigs (c:cls) = fromIntegral(length(filter (< 0) c)) + negDigs cls
negDigs [] = 0

-- Property: Amount of negations in (CNF) form equal to number of negative numbers in clause
propNumNegs :: Form -> Bool
propNumNegs f = negAtoms (convertToCNF f) 0 == negDigs(formToCls f)

-- Returns number of numbers in a clause
numDigs :: Clauses -> Int
numDigs (c:cls) = fromIntegral(length c) + numDigs cls
numDigs [] = 0

-- Returns number of atoms in a form
numAtoms :: Form -> Int -> Int
numAtoms (Prop name) i = i+1
numAtoms (Neg f) i = numAtoms f i
numAtoms (Cnj fs) i = sum(map (`numAtoms` i) fs)
numAtoms (Dsj fs) i = sum(map (`numAtoms` i) fs)
numAtoms (Impl f1 f2) i = sum(map (`numAtoms` i) [f1,f2])
numAtoms (Equiv f1 f2) i = sum(map (`numAtoms` i) [f1,f2])

--  Property: Amount of atoms in (CNF) form equal to amount of digits in clause
propNumAtoms :: Form -> Bool
propNumAtoms f = numAtoms (convertToCNF f) 0 == numDigs(formToCls f)


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
  putStrLn $ "Testing if `p ∧ q` logically entails `p` (Expected: True): " ++ checkTestResult (entails pAndQ p)
  putStrLn $ "Testing if `p ∧ q` is logically equivalent to `p` (Expected: False): " ++ checkTestResult (not(equiv pAndQ p))

  putStrLn "Testing the parse function: "
  parserTest 0 100
  putStrLn "Testing the CNF function: "
  cnfTest 0 100
  putStrLn "Testing the DNF function: "
  dnfTest 0 100
