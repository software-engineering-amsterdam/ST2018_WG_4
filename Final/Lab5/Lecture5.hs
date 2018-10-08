
module Lecture5

where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           System.Random
import           Test.QuickCheck

type Row    = Int
type Column = Int
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = putStrLn $ "| " ++ showVal a1 ++ " " ++ showVal a2 ++ " " ++ showVal a3 ++ " | " ++ showVal a4 ++ " "
     ++ showVal a5 ++ " " ++ showVal a6 ++ " | " ++ showVal a7 ++ " " ++showVal a8 ++ " " ++ showVal a9 ++ " |"

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn "+-------+-------+-------+"
    showRow as; showRow bs; showRow cs
    putStrLn "+-------+-------+-------+"
    showRow ds; showRow es; showRow fs
    putStrLn "+-------+-------+-------+"
    showRow gs; showRow hs; showRow is
    putStrLn "+-------+-------+-------+"

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> (Row,Column) -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions]
                ++
               [ colInjective s c |  c <- positions]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nrcSubgridInjective s (a,b) |
                    a <- [2,6], b <- [2,6]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> Bool -> [Node]
extendNode (s,constraints) (r,c,vs) isNRC =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune (r,c,v) constraints isNRC) | v <- vs ]

sameBlock :: (Row,Column) -> (Row,Column) -> Bool
sameBlock (r,c) (x,y) = bl r == bl x && bl c == bl y

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
              if (not . consistent) s then []
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,
                            c <- positions,
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
    [(r,c, freeAtPositions s (r,c)) |
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _)  = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal (children x ++ xs)

solveNs :: [Node] -> Bool -> [Node]
solveNs n isNRC = search (succNode isNRC) solved n

solveNs' :: [Node] -> [Node]
solveNs' n = solveNs n True

succNode :: Bool -> Node -> [Node]
succNode isNRC (s,[])   = []
succNode isNRC (s,p:ps) = extendNode (s,ps) p isNRC

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = traverse showNode . solveNs'

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8],
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

emptyN :: Node
emptyN = (const 0,constraints (const 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs
                  if null y
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs)
  where f []     = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Bool -> Node -> IO [Node]
rsuccNode isNRC (s,cs) = do xs <- getRandomCnstr cs
                            if null xs
                            then return []
                            else return (extendNode (s,cs\\xs) (head xs) isNRC)

rsolveNs :: [Node] -> Bool -> IO [Node]
rsolveNs ns isNRC = rsearch (rsuccNode isNRC) solved (return ns)

rsearch :: (node -> IO [node])
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes =
  do xs <- ionodes
     if null xs
       then return []
       else
         if goal (head xs)
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys
                      then return [head ys]
                      else if null (tail xs) then return []
                           else
                             rsearch
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = genRandomSudokuType True

genRandomSudokuType :: Bool -> IO Node
genRandomSudokuType isNrc = do [r] <- rsolveNs [emptyN] isNrc
                               return r

randomS = genRandomSudoku >>= showNode

uniqueSol' :: Node -> Bool -> Bool
uniqueSol' node isNRC = singleton (solveNs [node] isNRC) where
  singleton []       = False
  singleton [x]      = True
  singleton (x:y:zs) = False

uniqueSol :: Node -> Bool
uniqueSol node = uniqueSol' node True

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s pos pos2 | pos == pos2 = 0
                  | otherwise = s pos2

eraseN :: Node -> (Row,Column) -> Node
eraseN n pos = (s, constraints s)
  where s = eraseS (fst n) pos

eraseS' :: Sudoku -> [(Row,Column)] -> Sudoku
eraseS' = foldr (flip eraseS)

eraseN' :: Node -> [(Row,Column)] -> Node
eraseN' n pos = (s, constraints s)
  where s = eraseS' (fst n) pos

minimalize :: Node -> [(Row,Column)] -> Bool -> Node
minimalize n [] isNRC = n
minimalize n ((r,c):rcs) isNRC | uniqueSol' n' isNRC = minimalize n' rcs isNRC
                               | otherwise          = minimalize n  rcs isNRC
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = genProblemType n True

genProblemType :: Node -> Bool -> IO Node
genProblemType n isNRC = do ys <- randomize xs
                            return (minimalize n ys isNRC)
   where xs = filledPositions (fst n)


-- Assignment 1
-- Time: 110 minutes
-- Result (of running the given example):
--                                +-------+-------+-------+
--                                | 4 7 8 | 3 9 2 | 6 1 5 |
--                                | 6 1 9 | 7 5 8 | 3 2 4 |
--                                | 2 3 5 | 4 1 6 | 9 7 8 |
--                                +-------+-------+-------+
--                                | 7 2 6 | 8 3 5 | 1 4 9 |
--                                | 8 9 1 | 6 2 4 | 7 5 3 |
--                                | 3 5 4 | 9 7 1 | 2 8 6 |
--                                +-------+-------+-------+
--                                | 5 6 7 | 2 8 9 | 4 3 1 |
--                                | 9 8 3 | 1 4 7 | 5 6 2 |
--                                | 1 4 2 | 5 6 3 | 8 9 7 |
--                                +-------+-------+-------+
nrcExample :: Grid
nrcExample = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) = [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c]

freeInNrcSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcSubgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))

nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
nrcSubgridInjective s (r,c) = injective vs where
  vs = filter (/= 0) (nrcSubGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
               freeInRow s r
   `intersect` freeInColumn s c
   `intersect` freeInSubgrid s (r,c)
   `intersect` freeInNrcSubgrid s (r,c)

-- Assignment 2
-- Time: 190 min
-- Which of the two versions is easier to modify for NRC sudokus, and why?
-- Definately the refactored version. With the refactored version we just add a new Constrnt and add it to the list of Constrnts. For the original version we had to create and modify a big bunch of methods, which is not needed with the refactored one.
-- Which of the two versions is more efficient?
-- Doesn't matter that much, but the original one used to be more efficient. For the original one smaller lists where used within the `freeAtPos` function. Because of this, it was more efficient.
-- Short test report:
-- To check if both implementations are equal I use quickcheck to check if my `freeAtPositions` method yields the same results as `freeAtPos`. I created my own QuickCheck generator which automatically generates sudoku positions. I perform all checks on the NRC sudoku example problem.
-- Result:
--                                +++ OK, passed 100 tests.
type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values]
columnConstrnt = [[(r,c)| r <- values ] | c <- values]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]
--blockConstrnt = map (`createBlock` (3,3)) [(i,j) | i <- [1,4..9], j <- [1,4..9]]
nrcConstrnt = map (`createBlock` (3,3)) [(i,j) | i <- [2,6], j <- [2,6]]
constrnts = [rowConstrnt, columnConstrnt, blockConstrnt, nrcConstrnt]

createBlock :: Position -> (Int, Int) -> [Position]
createBlock (posx, posy) (sizex,sizey) = [(i,j) | i <- [posx..(posx-1)+sizex], j <- [posy..(posy-1)+sizey]]

freeAtPositions :: Sudoku -> Position -> [Value]
freeAtPositions s pos = foldl1 intersect (map (freeAtPos' s pos) constrnts)

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s pos xs = let ys = filter (elem pos) xs in if null ys then values else concatMap ((values \\) . map s) ys

genSudokuPositions :: Gen Position
genSudokuPositions = (arbitrary :: Gen Position) `suchThat` (\(x,y) -> x `elem` values && y `elem` values)

freePosTest :: Position -> Bool
freePosTest pos = freeAtPositions (grid2sud nrcExample) pos == freeAtPos (grid2sud nrcExample) pos

-- Assignment 3
-- Time: 210 minutes
-- I wrote my own QuickCheck because the default QuickCheck doesn't handle IO Bool. The implemententation of this test checks if a random problem has a unique solution, and all it's erased hints have no unique solution.
-- Result (for running it 10 times, printing the sudoku problem being tested whether it is minimal):
--                                The test passed for the following sudoku problem (1 out of 10):
--                                +-------+-------+-------+
--                                |       |       |       |
--                                |     3 | 5     |     2 |
--                                | 7     |   6   |       |
--                                +-------+-------+-------+
--                                |     1 |       |   2   |
--                                |       |   1   |   5   |
--                                |       | 4 3   |       |
--                                +-------+-------+-------+
--                                |       |   9   |   8   |
--                                |   7   | 3   4 |       |
--                                |       |       |   6   |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (2 out of 10):
--                                +-------+-------+-------+
--                                |   7   |       |       |
--                                |       | 5     |     7 |
--                                |       | 1 2   | 9 5   |
--                                +-------+-------+-------+
--                                |     3 |       |     1 |
--                                |       |   1   |       |
--                                |       |       | 6     |
--                                +-------+-------+-------+
--                                |   1 8 |       |       |
--                                | 4     |   7   |     3 |
--                                |       |       |   1   |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (3 out of 10):
--                                +-------+-------+-------+
--                                |       |   1   |     6 |
--                                |   4   | 9     |       |
--                                |   3 1 |       |       |
--                                +-------+-------+-------+
--                                |       |   9 2 |     1 |
--                                |       |       |       |
--                                |       |     7 |   5   |
--                                +-------+-------+-------+
--                                |     5 | 3   6 |   8   |
--                                |       |       | 9     |
--                                |       | 2     |       |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (4 out of 10):
--                                +-------+-------+-------+
--                                |   8   |       |     9 |
--                                |   4   |       |       |
--                                | 5   3 |       |   2   |
--                                +-------+-------+-------+
--                                | 1     |       |   3   |
--                                | 3     |       | 5   6 |
--                                |   5   | 9     |     2 |
--                                +-------+-------+-------+
--                                |     4 |       |       |
--                                | 8     | 7     |       |
--                                |       |     2 |       |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (5 out of 10):
--                                +-------+-------+-------+
--                                |       |   6 9 | 1     |
--                                |       | 5   3 |       |
--                                |       |       |       |
--                                +-------+-------+-------+
--                                |       |   5   | 6   4 |
--                                |   1   |   8   |       |
--                                |   4   | 6     |       |
--                                +-------+-------+-------+
--                                |   5   |       |     7 |
--                                |       |       |   4   |
--                                |     3 |       |   5 2 |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (6 out of 10):
--                                +-------+-------+-------+
--                                |       | 3     |   6   |
--                                |       |       |     1 |
--                                |   8   |     7 |       |
--                                +-------+-------+-------+
--                                |       |   2   |       |
--                                |       |   4   |       |
--                                |     8 |       | 9     |
--                                +-------+-------+-------+
--                                |       | 4     |       |
--                                |   6 2 |       | 8     |
--                                |   4   |   3 9 |     6 |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (7 out of 10):
--                                +-------+-------+-------+
--                                | 2 4   |       |   7   |
--                                |       | 3     |       |
--                                |   6   |     9 |   2   |
--                                +-------+-------+-------+
--                                |       |       |       |
--                                |       |       | 2   1 |
--                                |       |       |   9 7 |
--                                +-------+-------+-------+
--                                |   3 6 | 8     |       |
--                                |     7 |       |       |
--                                |   5   |       | 1 3   |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (8 out of 10):
--                                +-------+-------+-------+
--                                |   2   |       |   7   |
--                                | 9     |     6 |       |
--                                |     4 | 9 5   |       |
--                                +-------+-------+-------+
--                                |   8   |       |       |
--                                |       | 8   3 |       |
--                                |       |     1 |     5 |
--                                +-------+-------+-------+
--                                |       |       | 3     |
--                                |     7 |   3   |   4   |
--                                |       |       |   2   |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (9 out of 10):
--                                +-------+-------+-------+
--                                |     3 |       |     5 |
--                                | 6   2 |       |     3 |
--                                |       |       |   7   |
--                                +-------+-------+-------+
--                                | 8 5   |     4 |       |
--                                |       |       |       |
--                                | 3     |     6 |   2   |
--                                +-------+-------+-------+
--                                |       |       |       |
--                                |       | 2 1 3 |     6 |
--                                |       |       |     7 |
--                                +-------+-------+-------+
--
--                                The test passed for the following sudoku problem (10 out of 10):
--                                +-------+-------+-------+
--                                |       |       |   2   |
--                                |       |   2   |       |
--                                |       |       |       |
--                                +-------+-------+-------+
--                                |   5   |       |     9 |
--                                | 8 2   | 7   4 | 5     |
--                                |       |   1   |   8   |
--                                +-------+-------+-------+
--                                | 5   9 |       |       |
--                                |       |       |   3   |
--                                |   3   |     6 |       |
--                                +-------+-------+-------+
--                                10 tests passed
checkAllErasedHints :: Node -> Bool
checkAllErasedHints node = all (not . uniqueSol) (map (eraseN node) (filledPositions $ fst node))

testIsMinimal :: Int -> Int -> IO ()
testIsMinimal testsExecuted totalTests = if testsExecuted == totalTests then putStrLn (show totalTests ++ " tests passed")
                else (genRandomSudoku >>= genProblem) >>= \x -> if uniqueSol x && checkAllErasedHints x then
                                                                   do putStrLn $ "\nThe test passed for the following sudoku problem (" ++ show (testsExecuted + 1) ++ " out of " ++ show totalTests ++ "):"
                                                                      showNode x
                                                                      testIsMinimal (testsExecuted+1) totalTests
                                                                else do showNode x
                                                                        error "I failed on this sudoku problem :-("

-- Assignment 4
-- Time: 230 minutes
-- Short report on findings:
--        I made a code that tries to generate sudoku problems with a number of empty blocks within a certain
--        number of attempts. The 3 and 4 empty blocks ones have always found a sudoku problem within 10 attempts.
--        For 5 empty blocks a problem is found most of the time. For 6 empty blocks a problem is never found.
-- Result:
--                                Trying max 10 times to find a sudoku problem with at least 3 empty blocks.
--                                +-------+-------+-------+
--                                |   8   | 2   4 |       |
--                                |       |       |       |
--                                |   3   | 9 1   |       |
--                                +-------+-------+-------+
--                                |       |       |   7 5 |
--                                |       |       |     8 |
--                                |       |       |       |
--                                +-------+-------+-------+
--                                |     9 |       |     2 |
--                                | 5 1   |       |   6 7 |
--                                | 4     |       |   5   |
--                                +-------+-------+-------+
--
--                                Trying max 10 times to find a sudoku problem with at least 4 empty blocks.
--                                +-------+-------+-------+
--                                | 6   9 |       |       |
--                                |     1 |       | 9     |
--                                |   7   |       |   8   |
--                                +-------+-------+-------+
--                                | 1     |       |       |
--                                |       |       |       |
--                                |   2 5 |       |       |
--                                +-------+-------+-------+
--                                |       | 8   5 | 3   4 |
--                                |       |   7 1 | 2     |
--                                |       | 4     |       |
--                                +-------+-------+-------+
--
--                                Trying max 10 times to find a sudoku problem with at least 5 empty blocks.
--                                +-------+-------+-------+
--                                |       |       | 6 9 2 |
--                                |       |       | 4     |
--                                |       |       |     1 |
--                                +-------+-------+-------+
--                                |       | 4 7 2 |       |
--                                |       | 3   8 |       |
--                                |       |       |       |
--                                +-------+-------+-------+
--                                |     9 |   1 3 |       |
--                                |   4   | 2     |       |
--                                | 5     |   6   |       |
--                                +-------+-------+-------+
--
--                                Trying max 10 times to find a sudoku problem with at least 6 empty blocks.
--                                Not found
generateSudokuProblem :: Int -> Int -> IO (Maybe (IO Node))
generateSudokuProblem emptyBlocks 0 = return Nothing
generateSudokuProblem 0 tries       = return (Just (genRandomSudoku >>= genProblem))
generateSudokuProblem nEmptyBlocks tries = let newSudoku = removeBlocksFromSudoku nEmptyBlocks values genRandomSudoku in newSudoku >>= \x -> if uniqueSol x then return (Just (genProblem x)) else generateSudokuProblem nEmptyBlocks (tries - 1)

-- Source https://www.reddit.com/r/haskell/comments/22o44v/delete_nth_item_another_noob_post/
deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

getPositionForBlock :: Value -> Position
getPositionForBlock x
  | x<=0 = error "Negative blocks are not allowed"
  | x<=3 = (1+(3*((x-1) `mod` 3)),1)
  | x<=6 = (1+(3*((x-1) `mod` 3)),4)
  | x<=9 = (1+(3*((x-1) `mod` 3)),7)
  | otherwise = error "Sudoku's only have 9 blocks."

removeBlocksFromSudoku :: Int -> [Int] -> IO Node -> IO Node
removeBlocksFromSudoku 0 valueList randSudoku = randSudoku
removeBlocksFromSudoku nBlocks valueList randSudoku = randSudoku >>= \x -> getRandomInt (length valueList - 1) >>= \r -> removeBlocksFromSudoku (nBlocks-1) (deleteN r valueList) (return (eraseN' x (createBlock (getPositionForBlock (valueList !! r)) (3,3))))

-- Assignment 5
-- Time: 50 minutes
-- Result:
--                                +-------+-------+-------+
--                                |       | 2     |       |
--                                | 5 8   |       |       |
--                                |       |   4   |       |
--                                +-------+-------+-------+
--                                |       |     4 |     8 |
--                                |     9 |   2   |       |
--                                |       |       | 9   3 |
--                                +-------+-------+-------+
--                                | 6     |       |   5 4 |
--                                |     7 |       |   2   |
--                                |     5 |       | 3   9 |
--                                +-------+-------+-------+
nrcSameBlock :: (Row,Column) -> (Row,Column) -> Bool
nrcSameBlock (r,c) (x,y) | all (\v -> v/=1 && v/=5 && v/=9) [r,c,x,y] = nrcBl r == nrcBl x && nrcBl c == nrcBl y
                         | otherwise = False

-- A slightly modified version of the example prune code.
prune :: (Row,Column,Value)
      -> [Constraint] -> Bool -> [Constraint]
prune _ [] isNRC = []
prune (r,c,v) ((x,y,zs):rest) isNRC
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest isNRC
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest isNRC
  | sameBlock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest isNRC
  | isNRC && nrcSameBlock (r,c) (x,y) =
      (x,y,zs\\[v]) : prune (r,c,v) rest isNRC
  | otherwise = (x,y,zs) : prune (r,c,v) rest isNRC

-- Assignment 7 (Bonus)
-- Minimal problems for NRC Sudokus need fewer hints than standard Sudoku problems.
-- Investigate the difference.
-- What is the average number of hints in a minimal standard Sudoku problem?
-- - Avarage number of tips from ten generated sudokus (standard):
-- -  23.9

-- What is the average number of hints in a minimal NRC Sudoku problem?
-- - Avarage number of tips from ten generated sudokus (NRC):
-- -  16.6
--
-- Result:
--                                Average tips from ten generated sudokus:
--                                23.9
--
--                                Average tips from ten generated NRC sudokus:
--                                16.6
genNonSolvedSudoku :: IO Node
genNonSolvedSudoku = genRandomSudokuType False >>= genProblem

genNumHints :: IO [Int]
genNumHints = forM [1..10] $ \_ -> do
       nd <- genNonSolvedSudoku
       return (length (filledPositions (fst nd)))

genNonSolvedSudokuNRC :: IO Node
genNonSolvedSudokuNRC = genRandomSudoku >>= genProblem

genNumHintsNRC :: IO [Int]
genNumHintsNRC = forM [1..10] $ \_ -> do
       nd <- genNonSolvedSudokuNRC
       return (length (filledPositions (fst nd)))
