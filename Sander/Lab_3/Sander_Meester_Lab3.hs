-- NAME : Sander Meester
-- CKNUM : 11014822
-- STUDY : Master Software Engineering
-- COURSE : Software Specification, Verification and Testing
--
-- Lab3

module Lab3

where

import Lecture3

--Exercise 1
-- time: 120 mins

ttable :: Form  -> [Bool]
ttable f = map (`evl` f) (allVals f)

unpackAllVals :: Form -> [[Bool]]
unpackAllVals f = map (map snd) (allVals f)

satisfiable :: Form -> Bool
satisfiable f = any (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not (any (`evl` f) (allVals f))

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2
     | length(allVals f1) /= length(allVals f2) = False
     | otherwise = all (\ v -> evl v f1 --> evl v f2) (allVals f1)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2
     | length(allVals f1) /= length(allVals f2) = False
     | otherwise = all (\ v -> evl v f1 == evl v f2) (allVals f1)

--Exercise 3
-- time: 30 mins on final solution, 300 mins on other attempt done using pattern matching which did not work.
-- Works only for non-tautology Formulas
getTTable :: Form -> [([Bool], Bool)]
getTTable f = zip (unpackAllVals f) (ttable f)

getNumString :: [Bool] -> Int -> [Int]
getNumString (b:bs) i
     | not b = i : getNumString bs (i+1)
     | otherwise = (-i) : getNumString bs (i+1)
getNumString [] i = []

getBin :: ([Bool], Bool) -> [Int]
getBin (bs, False) = getNumString bs 1
getBin (_, True) = []

digToForm :: [Int] -> Form
digToForm xs = Dsj (map (\x -> (Prop x)) (xs))

convToForm :: [[Int]] -> [Form]
convToForm is = map digToForm (filter (not . null) is)

convertToCNF :: Form -> Form
convertToCNF f = Cnj (convToForm(map getBin (getTTable f)))


-- Bonus
-- Time taken: 125 mins (not included automated testing)

--Type definitions for clauses
type Clause  = [Int]
type Clauses = [Clause]

-- Converts a single element (p or Neg p) to a Clause
convertPropToClause :: Form -> Clause
convertPropToClause (Prop name) = [name]
convertPropToClause (Neg (Prop name)) = [-name]

-- Converts conjunctions and disjunctions to Clauses
convertToClauses :: Form -> Clauses
convertToClauses (Cnj [Dsj fs1, Dsj fs2]) = [concatMap convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [fs1, Dsj fs2]) = [convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [Dsj fs1, fs2]) = [concatMap convertPropToClause fs1, convertPropToClause fs2]
convertToClauses (Cnj fs) = concatMap convertToClauses fs
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
