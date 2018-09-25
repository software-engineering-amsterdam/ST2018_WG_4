module Main where
import           Data.Char
import           Data.List
import           Lecture3
import           System.IO.Unsafe
import           System.Random
import           Test.QuickCheck

-- Assignment 1 (Definitions of contradiction, tautology, entails and equiv)
-- Time: 1.5 hours
-- How I tested: I tested my solution using tests that test every edge case in the functions which I implemented. A few of these edge case were taken from `http://logic.stanford.edu/classes/cs157/2011/lectures/lecture03.pdf`.
-- Result:
--            Testing if just a single atom (p) is a tautology (Expected: False): Test succeeded!
--            Testing if just a single atom (p) is a contradiction (Expected: False): Test succeeded!
--            Testing if `p ∨ ¬p` is a tautology (Expected: True): Test succeeded!
--            Testing if `p ∨ ¬p` is a contradiction (Expected: False): Test succeeded!
--            Testing if `p ∧ ¬p` is a tautology (Expected: False): Test succeeded!
--            Testing if `p ∧ ¬p` is a contradiction (Expected: True): Test succeeded!
--
--            Testing if `p` logically entails `p` (Expected: True): Test succeeded!
--            Testing if `p` is logically equivalent to `p` (Expected: True): Test succeeded!
--            Testing if `p` logically entails `p ∨ q` (Expected: True): Test succeeded!
--            Testing if `p` is logically equivalent to `p ∨ q` (Expected: False): Test succeeded!
--            Testing if `p` logically entails `p ∧ q` (Expected: False): Test succeeded!
--            Testing if `p` is logically equivalent to `p ∧ q` (Expected: False): Test succeeded!
--            Testing if `p ∧ q` logically entails `p` (Expected: True): Test succeeded!
--            Testing if `p ∧ q` is logically equivalent to `p` (Expected: False): Test succeeded!

none :: Foldable t => (a -> Bool) -> t a -> Bool
none x y = not (any x y)

-- Compares the truth tables of two formulas. When the two forms have different amounts of atoms, the most extensive truth table will be used to test values from.
truthTable :: Form -> Form -> [(Bool,Bool)]
truthTable x y = take maxList (map (\i -> (evl i x, evl i y)) longestList)
                 where valsX = allVals x
                       valsY = allVals y
                       maxList = max (length valsX) (length valsY)
                       longestList = if length valsX == maxList then valsX else valsY

contradiction :: Form -> Bool
contradiction f = none (`evl` f) (allVals f)

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g = all (uncurry (-->)) (truthTable f g)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = all (uncurry (==)) (truthTable f g)

pOrNotP = Dsj[p,Neg p]
pAndNotP = Cnj[p,Neg p]
pOrQ = Dsj[p,q]
pAndQ = Cnj[p,q]

-- Assignment 2 (Testing the propositional formula parser)
-- Time: 60 minutes
-- This assignment can best be tested by using a random form generator, as written for assignment 4. Also, I am applying the custom QuickCheck implementation
-- as provided in the lecture code of Lecture2. For each random form generated, I use `show` to convert it to a String and then use `parse` to parse it again
-- to a Form. Please note that this tests both the `show` code for Forms and the `parse` functionality. To check if the parsed form is still correct, I compare
-- truth tables using the `equiv` function which had to be written for assingnment 2.
--
-- Result:
--            pass on: -*(+(-(-1==>(-(-2==>-+(-2))==>*(-0 -2 -+(2 3) -0))) 4) -*(-5))
--            pass on: +(-1 -(2<=>+(*(-+(-2) 2 1) (-2==>-1))))
--            pass on: -(-*(-1)==>1)
--            pass on: -+((0==>1))
--            pass on: +(*(-+(0 -(-1<=>-(-0<=>(1<=>1)))) 0) (-+(*((-1==>1) -2 2))==>-(-1==>(0==>-1))))
--            pass on: -(1<=>-+(+((1<=>-(2<=>1)) *(-0)) -2 ((-3==>-*(-3))<=>-*(0))))
--            pass on: (+(-(-1==>*((1<=>-0))))==>2)
--            pass on: +(-*(-(-((-0<=>1)==>-(-+(1 1)<=>(-2==>3)))<=>(-4<=>5)) 0 -(-0<=>0) -+((-2==>+(0)))))
--            pass on: *((-*(*(1 0 -2) -3 -+(*(2) 3 0 -1 -2) -2)==>(3<=>-(-2<=>-*((0==>-0))))) (-3<=>+(3)) -0 -+(*(*(1 2) 1) -*(1)))
--            pass on: (*(-+(+(-(0==>1))))<=>+(1 -0 0))
--            10 tests passed
--
-- For this example I tested just 10 times not to spam the console, but this number (10) can easily be replaced by higher numbers (for instance 100 like QuickCheck)
-- to get a more trustworthy test result.

parserTest :: Int -> Int -> IO ()
parserTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateActualForm >>= \x -> let resultingForm = parse (show x) in if length resultingForm == 1 && equiv x (head resultingForm) then
                    do putStrLn ("pass on: " ++ show x)
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)

-- Assignment 3
-- Time: 610 minutes
-- This assignment took quite long because I wanted to try to do the most conventional method to convert to CNF: first applying the De Morgan law and
-- then distributing till we have a CNF form. Esoecially the distributive law seemed to be much more complex than I initially would've guessed, and
-- it took a lot of debugging and optimizing to come to an end result that can convert ANY formula to CNF. I have written the general rules the
-- methods enforce above the methods. I have tested this solution with my random form generator of assignment 4.
--
-- Result:
--            pass on: -1
--            pass on: -(-*(-(0<=>0))==>-(-*(1 -2 3 (-*(-1)<=>-2) 0)<=>-*(((2<=>-1)<=>-0) 3)))
--            pass on: -+(+(-(+(*(-1))==>-(0<=>-0))))
--            pass on: +(*(-((0<=>-((0==>1)<=>(-1<=>-1)))==>-(1<=>1)) 1 -1 *(-0) -1 -0) +(-*(2)))
--            pass on: -((-0<=>-1)==>(-*(-(+(1 -1 +(2 -2 1 -1) -(0==>3))<=>-4) -2)<=>2))
--            pass on: *(1 -2 1)
--            pass on: -+(1)
--            pass on: +(-+(-(0<=>1)))
--            pass on: +(-(0==>(-1==>1)) 1)
--            pass on: +(*(-*(-0 0 +(1) 1)))
--            10 tests passed
--
-- I also created a `cnfProbe` function which actually shows the conversion process and the properties for which it is tested. This is the result of that:
--
--            Trying: (+(-((-0==>-1)<=>-2))==>1)
--            CNF form: *(+(-2 -0 1) +(-2 1))
--             Equiv: True
--             isCnf True
--
--            Trying: -*(+(+(-(1==>0))))
--            CNF form: +(-1 0)
--             Equiv: True
--             isCnf True
--
--            Trying: -*(-+(0) +(-1 -+(-0 -*(-1) -(-(+(2)==>0)<=>1) -2 (1==>-1) 3)))
--            CNF form: +(1 0)
--             Equiv: True
--             isCnf True
--
--            Trying: 0
--            CNF form: 0
--             Equiv: True
--             isCnf True
--
--            Trying: -(0==>-(-(+(1)==>*(-2 -+(2 2 0) -+(2 0) 2 (-2==>1)))==>(-+(3)<=>-3)))
--            CNF form: 0
--             Equiv: True
--             isCnf True
--
--            Trying: -+((-*(*(-+(0) -+(-1 (-2==>2))))<=>-2) -1 -3 -+(-3 +(-3 (-*(0 4 1 2)==>-1) -4 1)) -+(3) (-4==>-0))
--            CNF form: *(+(2 -0) +(2 1) 1 3 -4 0)
--             Equiv: True
--             isCnf True
--
--            Trying: 0
--            CNF form: 0
--             Equiv: True
--             isCnf True
--
--            Trying: *(0)
--            CNF form: 0
--             Equiv: True
--             isCnf True
--
--            Trying: *(((-(1==>-2)==>-0)<=>-(-1==>-+(-*(-0 -1 3 -3)))) -(4<=>(5==>-+(-(-4==>-0) 4 -5))))
--            CNF form: *(+(-1 2) +(-1 0) +(5 -4) +(4 -0 -5))
--             Equiv: True
--             isCnf True
--
--            Trying: -0
--            CNF form: -0
--             Equiv: True
--             isCnf True
--
--            10 times succesfully ran
--
-- For this example I tested just 10 times not to spam the console, but this number (10) can easily be replaced by higher numbers (for instance 100 like QuickCheck)
-- to get a more trustworthy test result.

-- Conversion to cnf is simply applying the De Morgan law and the Distributive law in order.
convertToCNF :: Form -> Form
convertToCNF form = applyDistributiveLaw $ while (not . isCnf) applyDistributiveLaw (applyDeMorganLaw form)

-- Remove negations of non atoms, implications and equivalences in the following way:
-- ¬(p ∨ q) == ¬p ∧ ¬q
-- ¬(p ∧ q) == ¬p ∨ ¬q
-- ¬(p ⇒ q) == p ∧ ¬q
-- ¬(p ⇔ q) == (¬p ∨ ¬q) ∧ (p ∨ q)
-- ¬(¬p) == p
-- p ⇒ q == ¬p ∨ q
-- p ⇔ q == (¬p ∨ q) ∧ (p ∨ ¬q)
applyDeMorganLaw :: Form -> Form
applyDeMorganLaw (Neg (Cnj formList)) = Dsj (map (applyDeMorganLaw . Neg) formList)
applyDeMorganLaw (Neg (Dsj formList)) = Cnj (map (applyDeMorganLaw . Neg) formList)
applyDeMorganLaw (Neg (Impl form1 form2)) = Cnj [applyDeMorganLaw form1,applyDeMorganLaw (Neg form2)]
applyDeMorganLaw (Neg (Equiv form1 form2)) = Cnj [Dsj [applyDeMorganLaw (Neg form1),applyDeMorganLaw (Neg form2)],Dsj [applyDeMorganLaw form1,applyDeMorganLaw form2]]
applyDeMorganLaw (Neg (Neg form)) = applyDeMorganLaw form
applyDeMorganLaw (Neg form) = Neg (applyDeMorganLaw form)
applyDeMorganLaw (Impl form1 form2) = Dsj [applyDeMorganLaw (Neg form1),applyDeMorganLaw form2]
applyDeMorganLaw (Equiv form1 form2) = Cnj [Dsj [applyDeMorganLaw (Neg form1),applyDeMorganLaw form2],Dsj [applyDeMorganLaw form1,applyDeMorganLaw (Neg form2)]]
applyDeMorganLaw (Cnj formList) = Cnj (map applyDeMorganLaw formList)
applyDeMorganLaw (Dsj formList) = Dsj (map applyDeMorganLaw formList)
applyDeMorganLaw form = form

-- Distribute in the following way:
-- (p ∧ q) ∨ (r ∧ t) == (p ∨ r) ∧ (p ∨ t) ∧ (q ∨ r) ∧ (q ∨ t)
-- (p ∧ q) ∨ r == (p ∨ r) ∧ (q ∨ r)
-- p ∨ (q ∧ r) == (p ∨ q) ∧ (p ∨ r)
applyDistributiveLaw :: Form -> Form
applyDistributiveLaw (Neg form) = Neg (applyDistributiveLaw form)
applyDistributiveLaw (Cnj [form]) = applyDistributiveLaw form
applyDistributiveLaw (Dsj [form]) = applyDistributiveLaw form
applyDistributiveLaw (Cnj formList) = Cnj (removeDuplicates $ optimizeOR(foldr (\x acc -> let y = applyDistributiveLaw x in if isConjunction y then getAsList y++acc else y:acc) [] formList))
applyDistributiveLaw (Dsj formList) = Dsj (foldr (\x acc -> let y = applyDistributiveLaw x in
           if not (null acc) && (isConjunction x || isConjunction (head acc)) then let cnjOne = head acc
                                                                                       cnjTwo = y
                                                                                       conj = Cnj (if isDisjunction cnjOne then map (\ x -> Dsj (x : getAsList cnjOne)) (getAsList cnjTwo)
                                                                                                    else if isDisjunction cnjTwo then map (\ x -> Dsj (x : getAsList cnjTwo)) (getAsList cnjOne)
                                                                                                    else [Dsj [x,y] | x <- getAsList cnjOne, y <- getAsList cnjTwo]) in conj: tail acc
           else if isDisjunction y then getAsList y++acc
           else y:acc) [] formList)
applyDistributiveLaw x = x

-- Removes all clauses containing `p ∧ ¬p` as they are always true.
optimizeOR :: [Form] -> [Form]
optimizeOR (x:list) = let disjList = getAsList x in if isDisjunction x && any (\y -> any (\x -> x == negateLiteral y) disjList) disjList then optimizeOR list
                                                    else if isDisjunction x then Dsj (removeDuplicates disjList):optimizeOR list
                                                    else x:optimizeOR list
optimizeOR [] = []

-- Negates a form or literal
negateLiteral :: Form -> Form
negateLiteral (Neg x) = x
negateLiteral x = Neg x

-- Removes all duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\y x -> if x `elem` y then y else y++ [x]) []

-- Checks if a form is a disjunction
isDisjunction :: Form -> Bool
isDisjunction (Dsj x) = True
isDisjunction x = False

-- Checks if a form is a conjunction
isConjunction :: Form -> Bool
isConjunction (Cnj x) = True
isConjunction x = False

-- Checks if a form is a literal (an atom or a negation of an atom)
isLiteral :: Form -> Bool
isLiteral (Prop x) = True
isLiteral (Neg (Prop x)) = True
isLiteral x = False

-- Returns the list if the given form is a discunction or conjunction, or creates a list if it's not.
getAsList :: Form -> [Form]
getAsList (Dsj x) = x
getAsList (Cnj x) = x
getAsList x = [x]

-- Checks if a given formula is CNF. If the Form is a disjunction, all formulas in it's list must be literals.
-- If the Form is a conjunction, all formulas in it's list must either be literals or disjunctions consisting of literals.
-- This is a property (actually consisting of multiple properties) of cnf converted forms.
isCnf :: Form -> Bool
isCnf (Cnj formList) = all (\x -> (isDisjunction x && isCnf x) || isLiteral x) formList
isCnf (Dsj formList) = all isLiteral formList
isCnf form = isLiteral form

-- A debugging oriented function showing all results of every executed test for the CNF converter.
cnfProbe :: Int -> Int -> IO ()
cnfProbe testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " times succesfully ran")
                else generateActualForm >>= \x -> let cnfForm = convertToCNF x in do putStrLn ("Trying: " ++ show x ++ "\nCNF form: " ++ show cnfForm ++ "\n Equiv: " ++ show (equiv x cnfForm) ++ "\n isCnf " ++ show (isCnf cnfForm) ++ "\n")
                                                                                     cnfProbe (testsExecuted+1) totalTests

-- The test function for the CNF converter, using randomly generated forms from the random form generator of assignment 4.
cnfTest :: Int -> Int -> IO ()
cnfTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateActualForm >>= \x -> let cnfForm = convertToCNF x in if isCnf cnfForm && equiv x cnfForm then
                                                                                      do putStrLn ("pass on: " ++ show x)
                                                                                         cnfTest (testsExecuted+1) totalTests
                                                                                  else error ("failed test on: " ++ show x ++ "\nCNF form: " ++ show cnfForm ++ "\n Equiv: " ++ show (equiv x cnfForm) ++ "\n isCnf " ++ show (isCnf cnfForm))

-- Assignment 4
-- Time: 270 minutes
-- I wanted to create a form generator that could generate EVERY FORM POSSIBLE. I made a design for it, and discussing it with Ana we decided that the generator
-- should be finite (so not that there is ANY chance that it would be infinite). So that's what I made. 270 minutes later I have a form generator that can generate
-- every possible Form (which is ideal for random testing purposes because there is no chance that my form generator would not support some form that would fail
-- for any of the tests). I added comments to every method within this chapter, to explain the functionality of each part of the generator.
-- Possible result:
--            Example of a random form: -(-*(1 -*(-2 *(-(-1==>3) 1 -2) +(1) -0 1 +(4 -+(-(-0==>1) 1) *(-4)) -*(-2 2 -(5==>1)) -(1==>1) -4) -*(-(1<=>(-3==>5)) -4) *(4 -(0<=>4) 1 (*(3 -4)<=>(-4<=>(4<=>1))) -*(-1) (5<=>0)))==>+(5))
--
-- The assignment said: Use your random testing method to test the correctness of the conversion program from the previous exercise. Formulate a number of relevant properties to test, and carry out the tests, either with your own random formula generator or with QuickCheck.
-- These properties (equiv and isCnf) and the test method can be observed above.

-- This number, the `maxFormDepth`, is the big reason that this algorithm is actually finite. The algorithm will not create a form with clauses nesting deeper
-- than the max depth, and when coming closer to the depth the chance actually becomes less that the algorithm will choose a non-atom over an atom.
-- Note: If you want to see cool things in your console, make this number nice and high (the form is created lazily so a huge random form will be written to your console!).
maxFormDepth = 7

-- This is the data type that contains the state of creating the Form at any point during the runtime of the algorithm. It contains three fields:
--            amountOfProperties: The current amount of different types of atoms that have been added to the form (for instance, a form `p ∧ ¬q` would contain two atoms: p and q).
--            form: The current state of the formula.
--            curRand: The current index that will be fetched from the random number list (more on this later).
data TreeState = TreeState {amountOfProperties :: Int, form :: Form, curRand :: Int}

-- This method generates an infinite list of random floating-point numbers between 0 and 1. These numbers are generated in advance, so I can unwrap the monad
-- they're in and don't have to further bother any IO monads till the algorithm is over. The `curRand` field of the `TreeState` datatype holds the current
-- index that will be fetched from this list.
randomNumberStream :: IO [Float]
randomNumberStream = do
    g <- newStdGen
    return $ randomRs (0.00,1.00) g

-- Generates a random formula and returns it as an `IO Form`.
generateActualForm :: IO Form
generateActualForm = randomNumberStream >>= \x -> return (form (generateForm x (maxFormDepth-1) (TreeState 1 p 0)))

-- Retrieves the random number at the given index from the infinite random number list. Multiplies the random number so it is in between 0 and a given maximum,
-- then converts it to an integer by using the `floor` function.
getCurRandNum :: Int -> [Float] -> Int -> Int
getCurRandNum x randList maxNum = floor ((randList !! x) * fromIntegral maxNum)

-- Gives the current random number for a given treestate.
getCurRand :: TreeState -> [Float] -> Int -> Int
getCurRand TreeState{curRand = x} = getCurRandNum x

-- Randomly chooses whether to add an atom or a non-atom to the formula. This is done by generating a random chace based on the current depth. For instance,
-- if the depth is 2 and the `maxFormDepth` constant was set to 7, the chance that an atom will be generated is 2 and the chance that a non-atom will be generated
-- is 7 - 2 = 5.
generateForm :: [Float] -> Int -> TreeState -> TreeState
generateForm randList leftTreeChance state = if randomChoice < leftTreeChance then doRandomlyNegate randList (chooseRandomSplitForm randList (leftTreeChance - 1) newState) else doRandomlyNegate randList (chooseRandomProperty randList newState)
  where randomChoice = getCurRand state randList maxFormDepth
        newState = TreeState (amountOfProperties state) (form state) (curRand state + 1)

-- Randomly chooses a non-atom to be added to the formula. Two random numbers will be generated, which is shown in this tree:
--
--                    <-- firstRand -->
--                           /\
--                          /  \
--                         /    \
--                        /      \
--                       /        \
--                      /          \
--                     /            \
--                    /              \
--                   /                \
--                  /                  \
--                 /                    \
--                /                      \
--       <-- secondRand -->       <-- secondRand -->
--              /\                         /
--             /  \                       / \
--            /    \                     /   \
--           /      \                   /     \
--          /        \                 /       \
--         -          -               -         -
--  Conjunction   Disjunction   Implication  Equivalence
--
-- As visible in the tree, the second random number chooses (Conjunction or Disjunction) OR (Implication or Equivalence). The implication and equivalence call
-- the `generateForm` method again twice to generate two new atoms or non-atoms. The Conjunction and Disjunction generate a list of a random size of random forms.
chooseRandomSplitForm :: [Float] -> Int -> TreeState -> TreeState
chooseRandomSplitForm randList leftTreeChance state = case firstRand of 0 -> let x = generateFormList randList leftTreeChance newState maxFormDepth []
                                                                             in if secondRand == 0 then treeStateListToTreeState Cnj x
                                                                                                           else treeStateListToTreeState Dsj x
                                                                        1 -> let x = generateForm randList leftTreeChance newState
                                                                                 y = generateForm randList leftTreeChance x
                                                                             in if secondRand == 0 then TreeState (amountOfProperties y) (Impl (form x) (form y)) (curRand y)
                                                                                                   else TreeState (amountOfProperties y) (Equiv (form x) (form y)) (curRand y)
        where firstRand = getCurRand state randList 2
              secondRand = getCurRandNum (curRand state + 1) randList 2
              newState = TreeState (amountOfProperties state) (form state) (curRand state + 2)

-- Converts a list of TreeStates (as retrieved when calling the `generateForm` for every item of the form list of a Conjunction or Disjunction) to a single treestate
-- (which is a Conjunction or Disjunction respectively).
treeStateListToTreeState :: ([Form] -> Form) -> [TreeState] -> TreeState
treeStateListToTreeState f states = TreeState (amountOfProperties (last states)) (f (map form states)) (curRand (last states))

-- Generates a random sized list of random forms for Conjunctions and Disjunctions.
generateFormList :: [Float] -> Int -> TreeState -> Int -> [TreeState] -> [TreeState]
generateFormList randList leftTreeChance state maxListSize currentFormList = case randNum of 0 -> generatedState:currentFormList
                                                                                             1 -> generatedState:generateFormList randList leftTreeChance generatedState (maxListSize-1) currentFormList
                                                            where randNum = getCurRand state randList 2
                                                                  generatedState = generateForm randList leftTreeChance (TreeState (amountOfProperties state) (form state) (curRand state + 1))

-- Generates a random atom.
chooseRandomProperty :: [Float] -> TreeState -> TreeState
chooseRandomProperty randList state = TreeState (if chosenProperty == props then props + 1 else props) (Prop chosenProperty) (curRand state + 1)
  where props = amountOfProperties state
        chosenProperty = getCurRand state randList (props + 1)

-- Randomly negates a TreeState.
doRandomlyNegate :: [Float] -> TreeState -> TreeState
doRandomlyNegate randNums state = TreeState (amountOfProperties state) (if doNegate == 0 then Neg (form state) else form state) (curRand state + 1)
  where doNegate = getCurRand state randNums 2

-- Bonus
-- Time taken: 150 mins

--Type definitions for clauses
type Clause  = [Int]
type Clauses = [Clause]

-- Converts a literal (p or Neg p) to a Clause
convertPropToClause :: Form -> Clause
convertPropToClause (Prop name) = [name]
convertPropToClause (Neg (Prop name)) = [-name]

-- Converts conjunctions and disjunctions to Clauses
cnf2cls :: Form -> Clauses
cnf2cls (Cnj [Dsj fs1, Dsj fs2]) = [concatMap convertPropToClause fs1, concatMap convertPropToClause fs2]
cnf2cls (Cnj [fs1, Dsj fs2]) = [convertPropToClause fs1, concatMap convertPropToClause fs2]
cnf2cls (Cnj [Dsj fs1, fs2]) = [concatMap convertPropToClause fs1, convertPropToClause fs2]
cnf2cls (Cnj fs) = [concatMap convertPropToClause fs]
cnf2cls (Dsj fs) = [concatMap convertPropToClause fs]
cnf2cls atom = [convertPropToClause atom]

-- Convert a form to a CNF to a Clause format
formToCls :: Form -> Clauses
formToCls f = cnf2cls(convertToCNF f)

-- Returns number of negative atoms in a form
negAtoms :: Form -> Int -> Int
negAtoms (Prop name) i = i
negAtoms (Neg (Prop f)) i = i+1
negAtoms (Neg f) i = negAtoms f i
negAtoms (Cnj fs) i = sum(map (`negAtoms` i) fs)
negAtoms (Dsj fs) i = sum(map (`negAtoms` i) fs)
negAtoms (Impl f1 f2) i = sum(map (`negAtoms` i) [f1,f2])
negAtoms (Equiv f1 f2) i = sum(map (`negAtoms` i) [f1,f2])

-- Returns number of negative atoms in a clause
negDigs :: Clauses -> Int
negDigs (c:cls) = foldr ((+) . fromIntegral . length . filter (< 0)) 0 cls
negDigs [] = 0

-- Property: Amount of negations in (CNF) form equal to number of negative numbers in clause
propNumNegs :: Form -> Bool
propNumNegs f = negAtoms (convertToCNF f) 0 == negDigs(formToCls f)

-- Returns number of numbers in a clause
numDigs :: Clauses -> Int
numDigs (c:cls) = foldr ((+) . fromIntegral . length) 0 cls
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

testFormProperty :: (Form -> Bool) -> Int -> Int -> IO ()
testFormProperty formProperty testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateActualForm >>= \x -> if formProperty x then
                    do putStrLn ("pass on: " ++ show x)
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)


checkTestResult :: Bool -> String
checkTestResult True  = "\x1b[32mTest succeeded!\x1b[0m"
checkTestResult False = "\x1b[31mTest failed!\x1b[0m"

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

  putStrLn "\n== Assignment 2 (Testing the propositional formula parser) =="
  parserTest 0 10

  putStrLn "\n== Assignment 3 (Converting forms to CNF) =="
  cnfProbe 0 10
  cnfTest 0 10

  putStrLn "\n== Assignment 4 (Creating a random form generator) =="
  generateActualForm >>= \x -> putStrLn $ "Example of a random form: " ++ show x

  putStrLn "\n== BONUS Assignment 5 (Converting forms to clause format) =="
  testFormProperty propNumNegs 0 10
  testFormProperty propNumAtoms 0 10

  putStrLn "Done!"
