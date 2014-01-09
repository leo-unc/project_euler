module Main where

-- Sudoku solver for Project Euler.

import Data.Array
import Data.Char (isDigit,ord,chr)
import Data.List (groupBy,sortBy)
import Data.Ord (compare)
import Data.Function (on)
import Data.Maybe (isJust,fromJust,isNothing)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

type Cell = (Int,Int)

newtype Sudoku = Sudoku {
  getElements :: Array Cell (Maybe Int)
  }

printC x = case x of 
  Nothing -> '0'
  Just y -> chr $ (ord '0') + y
  
readSudokuFromFile :: String -> IO Sudoku
readSudokuFromFile fname =
  fmap (readSingleSudoku . lines ) (readFile fname)

readSingleSudoku :: [String] -> Sudoku
readSingleSudoku rows = 
  Sudoku { 
    getElements = arrayDims $ transformMatrix rows_2D
    }
  where parseRow r = map parsePosition r
        parsePosition c = 
          case c of
            '0' -> Nothing
            _ -> Just ((read [c])::Int)
        arrayDims = array ((1,1),(9,9))
        rows_2D = map parseRow rows
        transformMatrix =            
          concatMap flattenIndices . numberObjectsInList . map numberObjectsInList
        flattenIndices (a, bs) = map (\(b,e) -> ((a,b),e)) bs
        numberObjectsInList xs = zipWith (,) ([1..]::[Int]) xs
  
-- | Parse ProjectEuler's file containing multiple sudoku.
readMultipleSudoku :: String -> IO [Sudoku]
readMultipleSudoku fname = do
  -- Obtain file content.
  lines_fc <- fmap lines $ readFile fname
  -- take while starts with digit.
  let startsWithDigit a b = (isDigit $ head a) == (isDigit $ head b)
  let groups = groupBy startsWithDigit lines_fc
  let sudoku_strings = filter (\x->length x>1) groups
  return (map readSingleSudoku sudoku_strings) 

printMaybeSudoku Nothing = print "Nothing"
printMaybeSudoku (Just s) = printSudoku s

printSudoku :: Sudoku -> IO ()
printSudoku s = 
  putStrLn (printRows [1,2,3] ++
   printHor ++
   printRows [4,5,6] ++
   printHor ++ 
   printRows [7,8,9])
  where printE x = case (getElements s ! x) of
          x@(Just _) -> (printC x:' ':[])
          _ -> "* "
        printColumn = "|"
        printEol = "\n"
        printHor = replicate 21 '_' ++ printEol
        printRows rs = concatMap printRow rs
        printRow r = ((printRowCell 1 r) ++ printColumn ++ 
                      (printRowCell 2 r) ++ printColumn ++ 
                      (printRowCell 3 r) ++ printEol)
        printRowCell c r  = concatMap printE $ range ((r, 3*(c-1)+1), (r,3*c))
  
allElements = IntSet.fromList [1..9]

intSetFromMaybe :: [Maybe Int] -> IntSet.IntSet
intSetFromMaybe = IntSet.fromList . map fromJust . filter isJust

-- | Retunrs elements of sudoku in given set of cells.
subsetElementsOf :: Sudoku -> [Cell] -> IntSet.IntSet
subsetElementsOf s cs = 
  intSetFromMaybe . map (getElements s !) $ cs 

-- Map of cell to block cell
cellToBlock :: Cell -> Cell
cellToBlock (i,j) = (toCell i, toCell j)
  where toCell x = (ceiling (fromIntegral x / 3))::Int

blockBoundaries :: Cell -> (Cell, Cell)
blockBoundaries b = 
  case b of 
    (1,1) -> ((1,1),(3,3))
    (1,2) -> ((1,4),(3,6))
    (1,3) -> ((1,7),(3,9))
    (2,1) -> ((4,1),(6,3))
    (2,2) -> ((4,4),(6,6))
    (2,3) -> ((4,7),(6,9))
    (3,1) -> ((7,1),(9,3))
    (3,2) -> ((7,4),(9,6))
    (3,3) -> ((7,7),(9,9))
    _ -> error $ "Invalid block " ++ show b

-- | Returns elements of row i.              
rowElements :: Sudoku -> Int -> IntSet.IntSet
rowElements s i = 
  subsetElementsOf s $ range ((i,1),(i,9))

-- | Returns elements of column j.
columnElements :: Sudoku -> Int -> IntSet.IntSet
columnElements s j  = 
  subsetElementsOf s $ range ((1, j), (9,j))

-- | Returns elements of block (i,j) cell of s belongs to.
blockElements :: Sudoku -> Cell -> IntSet.IntSet
blockElements s (i,j) = 
  blockElements' s $ cellToBlock (i,j)

blockElements' s =
  subsetElementsOf s . range . blockBoundaries

-- | Returns possible elements to fill (i,j) cell of s.
candidateElements :: Sudoku -> Cell -> IntSet.IntSet
candidateElements s (i,j) = 
  let c = columnElements s j
      r = rowElements s i 
      b = blockElements s (i,j)
  in IntSet.difference allElements (IntSet.union r (IntSet.union c b))

isFull :: Sudoku -> Bool
isFull s = 
  let r = map (rowElements s) [1..9]
      c = map (columnElements s) [1..9]
      b = map (blockElements' s) (range ((1,1),(3,3)))
  in all (allElements==) (r ++ c ++ b)

-- | Returns empty cells.
findEmptyCells :: Sudoku -> [Cell]
findEmptyCells = map fst . filter (\x->isNothing $ snd x) . assocs . getElements

data Completion = Completion {
  cell :: Cell,
  possibleElement :: Int
  } deriving (Show)

data CompletionList = 
  UniqueCompletions {
    getCompletions :: [Completion]
    } | 
  NonUniqueCompletions {
    getCompletions :: [Completion]
    } | 
  InvalidPath deriving (Show)
                             
type CompletionsForCell = (Cell, IntSet.IntSet) 

-- | Transforms completion for a single cell to a list of 
-- | atomic completions.
toCompletionList :: CompletionsForCell -> [Completion]
toCompletionList (c, s) =
  map (\x -> Completion {
          cell = c,
          possibleElement = x
          }) $ IntSet.toList s
  
--
sortByVariants :: [CompletionsForCell] -> [CompletionsForCell]
sortByVariants = sortBy (compare `on` (IntSet.size . snd))
  
-- | Finds completions for incomplete sudoku
findCompletions :: Sudoku -> CompletionList
findCompletions s = 
  if any (\(c, cs) -> IntSet.null cs) completions || null completions
  then InvalidPath
  else case filterUniqueCompletions completions of
            [] -> NonUniqueCompletions {
              getCompletions = toCompletionList $ head completions
              }
            cs@(c:_) -> UniqueCompletions {
              getCompletions = concatMap toCompletionList cs
              }
  where emptyCells = findEmptyCells s
        completions = map (\c -> (c, candidateElements s c)) emptyCells
        filterUniqueCompletions = filter (\(c, cs)-> IntSet.size cs == 1)

-- | Applies a single completion to Sudoku matrix and
-- | obtain a new matrix.
applyCompletion :: Completion -> Sudoku -> Sudoku
applyCompletion c s = 
  s {
    getElements = elms // [(cell c, Just e)]
  }
  where elms = getElements s
        e = possibleElement c

-- | Signature of sudoku.

newtype SudokuSpaceStep = SudokuSpaceStep {
  getCandidates :: [Sudoku]
  }
                       
initialStep  s = SudokuSpaceStep {
  getCandidates = [s]
  }
                          
printSpaceStepHead :: SudokuSpaceStep -> IO ()
printSpaceStepHead sss =
  case getCandidates sss of
    []  -> putStrLn "Empty"
    c:_ -> printSudoku c

-- | Step of Sudoku solver,
-- | Take head, find completions, apply all possible.
matrixStep :: SudokuSpaceStep -> SudokuSpaceStep
matrixStep (sss@SudokuSpaceStep {
               getCandidates = cs
               }) =
  if null $ cs
  then sss
  else case findCompletions h of
    InvalidPath -> SudokuSpaceStep t
    UniqueCompletions {
      getCompletions = cmps
      } -> SudokuSpaceStep $ (foldr applyCompletion h cmps):t
    NonUniqueCompletions {  
      getCompletions = cmps
      } -> SudokuSpaceStep $ (map (\c->applyCompletion c h) cmps) ++ t
    where h = head cs
          t = tail cs

-- | Given incomplete sudoku, this will return 
-- | solved sudoku or Nothing if there is no solution.
solveSingleSudoku :: Sudoku -> Maybe Sudoku
solveSingleSudoku begin =  
  case head $ dropWhile (not . stopCondition) queue of
    SudokuSpaceStep {
      getCandidates = [] 
      } -> Nothing
    SudokuSpaceStep {
      getCandidates = c:_
      } -> Just c
  where queue = iterate matrixStep $ initialStep begin
        stopCondition s = (
          (null $ getCandidates s) || 
          (isFull $ head $ getCandidates s))

peekDigits :: Sudoku -> Int
peekDigits s =
  let a = fromJust (getElements s ! (1,1))
      b = fromJust (getElements s ! (1,2))
      c = fromJust (getElements s ! (1,3))
  in ( a * 100 + b * 10 + c)

main = do
  ss <- readMultipleSudoku "sudoku.txt"
  let sols = map solveSingleSudoku ss
  print $ (if any isNothing sols then "bad" else "all good")
  print $ sum $ map (peekDigits.fromJust) sols