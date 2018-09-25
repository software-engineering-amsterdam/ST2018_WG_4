module Main where
import Lecture3
import Data.List
import Data.Char
import Test.QuickCheck
import System.IO.Unsafe
import System.Random

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

truthTable :: Form -> Form -> [(Bool,Bool)]
truthTable x y = take (max (length valsX) (length valsY)) (map (\(i,j) -> (evl i x, evl j y)) (zip (cycle valsX) (cycle valsY)))
                 where valsX = allVals x
                       valsY = allVals y

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
-- This assignment can best be tested

parserTest :: Int -> Int -> IO ()
parserTest testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else generateActualForm >>= \x -> let resultingForm = parse (show x) in if length resultingForm == 1 && equiv x (head resultingForm) then
                    do putStrLn ("pass on: " ++ show x)
                       parserTest (testsExecuted+1) totalTests
                  else error ("failed test on: " ++ show x)

-- Assignment 3
convertToCNF :: Form -> Form
convertToCNF form = applyDistributiveLaw(applyDeMorganLaw form)

-- Remove negations of non atoms in the following way:
-- ¬(p ∨ q) == ¬p ∧ ¬q
-- ¬(p ∧ q) == ¬p ∨ ¬q
-- ¬(p ⇒ q) == p ∧ ¬q
-- ¬(p ⇔ q) == (¬p ∨ ¬q) ∧ (p ∨ q)
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

applyDistributiveLaw :: Form -> Form
applyDistributiveLaw (Neg form) = Neg (applyDistributiveLaw form)
applyDistributiveLaw (Cnj [form]) = applyDistributiveLaw form
applyDistributiveLaw (Cnj formList) = Cnj (map applyDistributiveLaw formList)
applyDistributiveLaw (Dsj [form]) = applyDistributiveLaw form
applyDistributiveLaw (Dsj (Cnj cnjOne: Cnj cnjTwo: xs)) = let conj = Cnj [Dsj [x,y] | x <- map applyDistributiveLaw cnjOne, y <- map applyDistributiveLaw cnjTwo] in if not (null xs) then applyDistributiveLaw (Dsj $ conj:xs) else applyDistributiveLaw conj
applyDistributiveLaw (Dsj (Cnj cnjOne: cnjTwo : xs)) = let conj = Cnj [Dsj [x,y] | x <- map applyDistributiveLaw cnjOne, y <- [applyDistributiveLaw cnjTwo]] in if not (null xs) then applyDistributiveLaw (Dsj $ conj:xs) else applyDistributiveLaw conj
applyDistributiveLaw (Dsj (cnjOne: Cnj cnjTwo : xs)) = let conj = Cnj [Dsj [x,y] | x <- [applyDistributiveLaw cnjOne], y <- map applyDistributiveLaw cnjTwo] in if not (null xs) then applyDistributiveLaw (Dsj $ conj:xs) else applyDistributiveLaw conj
--applyDistributiveLaw (Dsj (form1:form2:xs)) = Dsj (applyDistributiveLaw (Dsj [form1,form2]) : [applyDistributiveLaw (Dsj (form2 : xs)) | not (null xs)])--if length (filter (\x -> x == Cnj) (form1:form2:xs)) == 0 then Dsj (map applyDistributiveLaw (form1:form2:xs)) else Cnj ((Dsj [form1, form2]):(applyDistributiveLaw form2:xs))
applyDistributiveLaw x = x

-- Assignment 4
-- Time: 270 minutes

maxTreeChance = 10 -- This number decides the size of the resulting form.

data TreeState = TreeState {amountOfProperties :: Int, form :: Form, curRand :: Int}

randomNumberStream :: IO [Float]
randomNumberStream = do
    g <- newStdGen
    return $ randomRs (0.00,1.00) g

generateActualForm :: IO Form
generateActualForm = randomNumberStream >>= \x -> return (form (generateForm x (maxTreeChance-1) (TreeState 1 p 0))) -- This number decides the maximum amount of subforms in the resulting form.

getCurRandNum :: Int -> [Float] -> Int -> Int
getCurRandNum x randList maxNum = floor ((randList !! x) * fromIntegral maxNum)

getCurRand :: TreeState -> [Float] -> Int -> Int
getCurRand TreeState{curRand = x} = getCurRandNum x

generateForm :: [Float] -> Int -> TreeState -> TreeState
generateForm randList leftTreeChance state = if randomChoice < leftTreeChance then doRandomlyNegate randList (chooseRandomSplitForm randList (leftTreeChance - 1) newState) else doRandomlyNegate randList (chooseRandomProperty randList newState)
  where randomChoice = getCurRand state randList maxTreeChance
        newState = TreeState (amountOfProperties state) (form state) (curRand state + 1)

chooseRandomSplitForm :: [Float] -> Int -> TreeState -> TreeState
chooseRandomSplitForm randList leftTreeChance state = case firstRand of 0 -> let x = generateFormList randList leftTreeChance newState maxTreeChance []
                                                                             in if secondRand == 0 then treeStateListToTreeState Cnj x
                                                                                                           else treeStateListToTreeState Dsj x
                                                                        1 -> let x = generateForm randList leftTreeChance newState
                                                                                 y = generateForm randList leftTreeChance x
                                                                             in if secondRand == 0 then TreeState (amountOfProperties y) (Impl (form x) (form y)) (curRand y)
                                                                                                   else TreeState (amountOfProperties y) (Equiv (form x) (form y)) (curRand y)
        where firstRand = getCurRand state randList 2
              secondRand = getCurRandNum (curRand state + 1) randList 2
              newState = TreeState (amountOfProperties state) (form state) (curRand state + 2)

treeStateListToTreeState :: ([Form] -> Form) -> [TreeState] -> TreeState
treeStateListToTreeState f states = TreeState (amountOfProperties (last states)) (f (map form states)) (curRand (last states))

generateFormList :: [Float] -> Int -> TreeState -> Int -> [TreeState] -> [TreeState]
generateFormList randList leftTreeChance state maxListSize currentFormList = case randNum of 0 -> generatedState:currentFormList
                                                                                             1 -> generatedState:generateFormList randList leftTreeChance generatedState (maxListSize-1) currentFormList
                                                            where randNum = getCurRand state randList 2
                                                                  generatedState = generateForm randList leftTreeChance (TreeState (amountOfProperties state) (form state) (curRand state + 1))


chooseRandomProperty :: [Float] -> TreeState -> TreeState
chooseRandomProperty randList state = TreeState (if chosenProperty == props then props + 1 else props) (Prop chosenProperty) (curRand state + 1)
  where props = amountOfProperties state
        chosenProperty = getCurRand state randList (props + 1)

doRandomlyNegate :: [Float] -> TreeState -> TreeState
doRandomlyNegate randNums state = TreeState (amountOfProperties state) (if doNegate == 0 then Neg (form state) else form state) (curRand state + 1)
  where doNegate = getCurRand state randNums 2


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
  parserTest 0 100

  putStrLn "\n== Assignment 4 (Creating a random form generator) =="
  print =<< generateActualForm

  putStrLn "Done!"
