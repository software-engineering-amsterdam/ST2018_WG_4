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

--chooseTreeOption :: (Int -> Form) -> Int -> Property
--chooseTreeOption function maximumNumber = forAll (resize 1 (abs `fmap` (arbitrary :: Gen Int) `suchThat` (>maximumNumber-2))) (\x -> function (x `mod` maximumNumber))

data TreeState = TreeState {amountOfProperties :: Int, form :: Form}

chooseTreeOption :: Int -> Int
chooseTreeOption maximumNumber = unsafePerformIO (getStdRandom(randomR (0,maximumNumber-1)))

maxTreeChance = 10 -- This number decides the size of the resulting form.

generateActualForm :: Form
generateActualForm = form (generateForm (maxTreeChance-1) (TreeState 1 p)) -- This number decides the maximum amount of subforms in the resulting form.

generateForm :: Int -> TreeState -> TreeState
generateForm leftTreeChance state = if chooseTreeOption maxTreeChance < leftTreeChance then chooseRandomSplitForm (leftTreeChance - 1) state else chooseRandomProperty state

chooseRandomSplitForm :: Int -> TreeState -> TreeState
chooseRandomSplitForm leftTreeChance state = case chooseTreeOption 2 of 0 -> let x = generateFormList leftTreeChance state maxTreeChance []
                                                                             in if chooseTreeOption 2 == 0 then treeStateListToTreeState Cnj x
                                                                                                           else treeStateListToTreeState Dsj x
                                                                        1 -> let x = generateForm leftTreeChance state
                                                                                 y = generateForm leftTreeChance x
                                                                             in if chooseTreeOption 2 == 0 then TreeState (amountOfProperties y) (Impl (form x) (form y))
                                                                                                           else TreeState (amountOfProperties y) (Equiv (form x) (form y))

treeStateListToTreeState :: ([Form] -> Form) -> [TreeState] -> TreeState
treeStateListToTreeState f states = TreeState (amountOfProperties (last states)) (f (map form states))

generateFormList :: Int -> TreeState -> Int -> [TreeState] -> [TreeState]
generateFormList leftTreeChance state maxListSize currentFormList = case chooseTreeOption 2 of 0 -> generatedState:currentFormList
                                                                                               1 -> generatedState:generateFormList leftTreeChance generatedState (maxListSize-1) currentFormList
                                                                                               _ -> error "Something impossible just happened!"
                                                            where generatedState = generateForm leftTreeChance state

chooseRandomProperty :: TreeState -> TreeState
chooseRandomProperty state = TreeState (if chosenProperty == props then props + 1 else props) (Prop chosenProperty)
  where props = amountOfProperties state
        chosenProperty = chooseTreeOption (props + 1)

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

  putStrLn "Done!"
