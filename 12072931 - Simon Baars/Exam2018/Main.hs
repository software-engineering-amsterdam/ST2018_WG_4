module Main where

-- Just some imports I commonly used over the labs. Probably not all are used in the exam.
import System.Random
import Test.QuickCheck
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import Data.List
import Control.Exception
import Test.QuickCheck.Monadic
import Data.Maybe
import SetOrd

-- Useful stuff from earlier lectures:
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Positive = Natural numbers (1,2,3...)
-- NonNegative = Non-Negative natural numbers (0,1,2,3...)
-- LargeNumber = Numbers >=100
-- BoundedNumberAlt = Bad performance but more "functional" way to generate numbers between 0 and 10 (this notation takes a condition).
-- BoundedNumber = "Imperative" way of generating numbers between 0 and 10 (or whatever you want to do with the int generated).
newtype LargeNumber = LargeNumber Int deriving (Eq, Ord, Show)
newtype BoundedNumberAlt = BoundedNumberAlt Int deriving (Eq, Ord, Show)
newtype BoundedNumber = BoundedNumber Int deriving (Eq, Ord, Show)

instance Arbitrary BoundedNumberAlt where -- (Somewhat) more readable oneliner but worse performance
   arbitrary = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x>=0 && x<10) >>= \x -> return (BoundedNumberAlt x)

instance Arbitrary LargeNumber where
   arbitrary = do
     NonNegative x <- arbitrary
     return $ LargeNumber (x + 100) -- min 100

instance Arbitrary BoundedNumber where
   arbitrary = do
     NonNegative x <- arbitrary
     return $ BoundedNumber (x `mod` 10) -- max bound

propExample :: NonNegative Int -> Positive Int -> BoundedNumber -> LargeNumber -> Bool
propExample (NonNegative n) (Positive k) (BoundedNumber i) (LargeNumber l) = trace ("NonNegative n = "++show n++", Positive k = "++show k++", BoundedNumber i = "++show i++", LargeNumber l = "++show l) True

-- EXAM HASKELL STUFF

-- Assignment 1a
-- Answer: λx → x > 0
-- Reason: λx → x > -2 is weaker but is not valid.

-- Assignment 1b
-- Answer: λx → x > A
-- Reason: λx 7→ x > 5 is not always valid.

-- Assignment 1c
-- Answer: ??

-- Assignment 1d
-- Answer λn → n>=-1 ∧ n<=1
-- Reason: When we plot this formula, it is a parabool between 0 and 1 for any n

-- Assignment 2a
-- Only the equivalence {(0,0),(1,1),(2,2),(0,1),(1,0),(2,0),(0,2),(1,2),(2,1)} with partition {{0,1,2}} has no singleton subsets.
-- Reason: There is a singleton subset if no relations between a certain element and other elements are present (∀x ∈ A ∃y ∈ A, x \= y). This is only the case for this relation.

-- Assignment 2b
-- The number of non-singleton equivalences s on a set with n elements is s=0 for n=0 and s=fib(n-1) for n>0. This can be proven as follows:
-- Base case: n=0 (empty set). As indicated, an empty set has no non-singleton equivalences, so the base case is correct.
-- Additional base case n=1 (singleton relation). A singleton relation has no singleton relations. s = fib(1-1) = fib(0) = 0, so this base case is also correct.
-- Induction step: assume that for n the formula is correct: s=0 for n=0 and s=fib(n-1) for n>0
-- Prove that for n+1 it still holds: s=fib(n) for n>1
-- ??? I have no idea how to continue...

-- Assignment 2c
-- ∆A = {(0,0),(1,1),(2,2),(3,3)}
-- 

-- Assignment 3a
-- R⁺ = {(0,0),(0,2),(1,3),(2,0),(2,2)}
-- Reason: I used my trClos function that I wrote for Lab 4 to determine the transitive closure. This implementation was correct, so this transitive closure is correct.

-- Assignment 3b
-- S = R⁺ ∪ ∆A = {(0,0),(0,2),(1,3),(2,0),(2,2)} ∪ {(0,0),(1,1),(2,2),(3,3)} = {(0,0),(0,2),(1,1),(1,3),(2,0),(2,2),(3,3)}
-- An equivalence is a relation which is reflexive, symmetric, transitive.
-- This relation is reflexive because it contains (0,0) and (1,1) and (2,2) and (3,3)
-- However, this relation is not symmetric because (1,3) ∈ S but not (3,1) ∈ S
-- So this is not an equivalence.

-- Assignment 3c
-- Prove by induction: `aClos s = s ◦ s ∪ s` is the transitive closure of a relation s.
-- Base case s=Ø (empty relation). Ø ◦ Ø ∪ Ø = Ø. Ø is indeed the transitive closure of a relation Ø, so the base case holds.
-- Induction step: assume that for s the formula is correct: aClos s = s ◦ s ∪ s.
-- Prove for ???? again no idea how to continue...

-- Assignment 4a
type Rel a = [(a,a)]
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial s r = all (\x -> any (\y -> (x,y) `elem` r) s) s

-- Assignment 4b
-- If the relation has a lower size than the domain set, it can never be serial.
propNotSerialIfRelationHasLowerSize :: [Int] -> Rel Int -> Bool
propNotSerialIfRelationHasLowerSize s r = length r < length s --> not (isSerial s r)

-- If the relation is reflexive on the domain, it is always serial.
propReflexiveAlwaysSerial :: [Int] -> Bool
propReflexiveAlwaysSerial s = isSerial s [(x,x)|x<-s]

-- Assignment 5a
type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

instance Show Form where
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

p = Prop 1
q = Prop 2
r = Prop 3

form0 = Cnj [p,Neg p]
form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

setLength :: Eq a => Set a -> Int
setLength (Set x) = length x

-- My properties
-- A form always maps to minimum 1 set
propSetNotEmpty :: Form -> Bool
propSetNotEmpty f = setLength (sub f) /= 0

-- If the form is no atom it has at least 2 subforms
propIfNoAtomSetSizeMoreThanOne :: Form -> Bool
propIfNoAtomSetSizeMoreThanOne (Prop x) = True
propIfNoAtomSetSizeMoreThanOne f = setLength (sub f) > 1

-- Assignment 5b
nsub :: Form -> Int
nsub (Prop x) = 1
nsub (Neg f) = 1 + nsub f
nsub (Cnj f) = 1 + sum (map nsub f)
nsub (Dsj f) = 1 + sum (map nsub f)
nsub (Impl f1 f2) = 1 + nsub f1 + nsub f2
nsub (Equiv f1 f2) = 1 + nsub f1 + nsub f2

-- Plus the corresponding quickcheck property
propEqualSize :: Form -> Bool
propEqualSize f = setLength (sub f) == nsub f

-- Assignment 6
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)
grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

tree n = grow (step611 n) (6,11)
step611 n (x,y) = if x+y <= 60+n then [(x+y,x),(x,x+y)] else []

-- Assignment 7
-- See exam paper for explaination
gordon1 d0 n g r = d0*(1-((1+g)/(1+r))^n)/(1-(1+g)/(1+r))
gordon2 d0 n g r = d0*sum [((1+g)/(1+r))^k|k <-[1..n]]
gordon3 d0 n g r = d0*(1-((1+g)/(1+r))^n)/(r-g)*(1+g)
gordon4 d0 n g r = sum [d0*((1+g)/(1+r))^k|k <-[0..n-1]]

quickCheckGordon :: (Double -> Int -> Double -> Double -> Double) -> Double -> Int -> Double -> Double -> Bool
quickCheckGordon gordon d0 n g r = d0>=0 && n>=0 && g>=0 && r>=0 && g<=r --> gordon d0 n g r >= 0.00

main :: IO ()
main  = do
  putStrLn "\x1b[36m== Assignment 1 ==\x1b[0m"
  putStrLn "For this assignment, please see my exam paper. The full answer to this assignment is there :-)."

  putStrLn "\x1b[36m== Assignment 2 ==\x1b[0m"
  putStrLn "For this assignment, please see my exam paper. The full answer to this assignment is there :-)."

  putStrLn "\x1b[36m== Assignment 3 ==\x1b[0m"
  putStrLn "For this assignment, please see my exam paper. The full answer to this assignment is there :-)."

  putStrLn "\x1b[36m== Assignment 4 ==\x1b[0m"
  quickCheck propNotSerialIfRelationHasLowerSize
  quickCheck propReflexiveAlwaysSerial

  putStrLn "\x1b[36m== Assignment 5 ==\x1b[0m"


  putStrLn "\x1b[36m== Assignment 6 ==\x1b[0m"


  putStrLn "\x1b[36m== Assignment 7 ==\x1b[0m"
  quickCheck (quickCheckGordon gordon1)
  quickCheck (quickCheckGordon gordon2)
  quickCheck (quickCheckGordon gordon3)
  quickCheck (quickCheckGordon gordon4)
