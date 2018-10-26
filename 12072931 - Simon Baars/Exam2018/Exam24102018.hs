module Exam
  where
import SetOrd

type Rel a = [(a,a)]
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial = error "not yet implemented"

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

nsub :: Form -> Int
nsub = error "Not yet implemented"

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)
grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

tree n = grow (step611 n) (6,11)
step611 = error "Not yet implemented"

gordon1 d0 n g r = d0*(1-((1+g)/(1+r))^n)/(1-(1+g)/(1+r))

gordon2 d0 n g r = d0*sum [((1+g)/(1+r))^k|k <-[1..n]]

gordon3 d0 n g r = d0*(1-((1+g)/(1+r))^n)/(r-g)*(1+g)

gordon4 d0 n g r = sum [d0*((1+g)/(1+r))^k|k <-[0..n-1]]
