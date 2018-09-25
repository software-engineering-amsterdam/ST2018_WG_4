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
-- time:  mins

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


--Exercise 2
-- time: mins

  --Exercise 3
  -- time: 30 mins on final solution, 180 mins on other attempt done using pattern matching which did not work.
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

-- conjoDing :: Form -> [Form] -> [Form]
-- conjoDing f1 (f:fs) = Dsj (f1:[f]) : conjoDing f1 fs
-- conjoDing f1 [] = []--[Dsj (f1:[f])]
--
-- standardizeVars :: Form -> Form
-- standardizeVars (Cnj [f1, Dsj(f:fs)]) = Cnj (Dsj (f1:[f]) : conjoDing f1 fs)
-- standardizeVars (Dsj [Cnj [f1, f2], Cnj [g1, g2]]) = standardizeVars (Cnj [Cnj [Dsj [f1, g1], Dsj [f1, g2]], Cnj [Dsj [g1, f1], Dsj [g1, f2]]])
-- standardizeVars f = f


-- disj = + = V
-- conj = x = ^
-- testest :: Form -> [Bool]
-- testest f = zip (map (\[xs] -> xs) (unpackAllVals f)) (ttable f)

-- convertToCNF :: Form -> Form
-- convertToCNF f = standardizeVars(nnf(arrowfree f))



-- Bonus
type Clause  = [Int]
type Clauses = [Clause]

convertPropToClause :: Form -> Clause
convertPropToClause (Prop name) = [name]
convertPropToClause (Neg (Prop name)) = [-name]

convertToClauses :: Form -> Clauses
convertToClauses (Cnj [Dsj fs1, Dsj fs2]) = [concatMap convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [fs1, Dsj fs2]) = [convertPropToClause fs1, concatMap convertPropToClause fs2]
convertToClauses (Cnj [Dsj fs1, fs2]) = [concatMap convertPropToClause fs1, convertPropToClause fs2]
convertToClauses (Cnj fs) = [concatMap convertPropToClause fs]
convertToClauses (Dsj fs) = [concatMap convertPropToClause fs]

cnf2cls :: Form -> Clauses
cnf2cls = convertToClauses

formToCls :: Form -> Clauses
formToCls f = cnf2cls(convertToCNF f)

-- Returns number of negative atoms in a form
negProps :: Form -> Int -> Int
negProps (Prop name) i = i
negProps (Neg f) i = negProps f i+1
negProps (Cnj fs) i = sum(map (`negProps` i) fs)
negProps (Dsj fs) i = sum(map (`negProps` i) fs)
negProps (Impl f1 f2) i = sum(map (`negProps` i) [f1,f2])
negProps (Equiv f1 f2) i = sum(map (`negProps` i) [f1,f2])

negDigs :: Clauses -> Int
negDigs (c:cls) = fromIntegral(length(filter (< 0) c)) + negDigs cls
negDigs [] = 0

-- Number of negations in form equal to number of negative numbers in clause
-- propNumNegs :: Form -> Bool
-- propNumNegs f = negProps f == negDigs(formToCls f)

-- Use automated testing to check whether your translation is correct, employing some appropriate properties to check.
--
-- Deliverables: Conversion program, test generator, test properties, documentation of the automated testing process. Also, give an indication of time spent.
