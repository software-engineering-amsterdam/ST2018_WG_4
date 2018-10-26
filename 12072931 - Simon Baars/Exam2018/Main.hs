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
-- See exam paper for explanation
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
