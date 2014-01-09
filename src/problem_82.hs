module Main where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.Tree(Gr)
import Data.Array
import Data.Maybe(fromJust,isJust)
import System.Environment(getArgs)
import Text.CSV
import Data.Monoid

type Cell = (Int, Int)
type Matrix = Array Cell Integer

nonSingleton = filter (/=[""])

readMatrix :: String -> IO Matrix
readMatrix fname = do
  r <- fmap (fmap (string2DToInteger2D . nonSingleton)) (parseCSVFromFile fname)
  case r of 
    Right ns -> return (arrayDims ns $ transformMatrix ns)
    _ -> undefined
  where arrayDims ns = array ((0,0),(length ns - 1, length (head ns) - 1))
        string2DToInteger2D = map (map (\x->read x::Integer) . filter (not . null)) . filter (not . null)
        transformMatrix = 
          concatMap flattenIndices . numberObjectsInList . map numberObjectsInList
        flattenIndices (a, bs) = map (\(b,e) -> ((a,b),e)) bs
        numberObjectsInList xs = zipWith (,) ([0..]::[Int]) xs

data AdmissibleDirection = UP|DOWN|LEFT|RIGHT
                          deriving(Show)

allDirections = [UP, DOWN, LEFT, RIGHT]

neighbor :: Matrix -> AdmissibleDirection -> Cell -> Maybe Cell
neighbor m d c = 
  let bnds = bounds m
      neighbor' UP c = (-1 + fst c, snd c)
      neighbor' DOWN c = (1 + fst c, snd c)
      neighbor' LEFT c = (fst c, -1 + snd c)
      neighbor' RIGHT c = (fst c, 1 + snd c)
  in let n' = neighbor' d c
     in if inRange bnds n' 
        then Just n'
        else Nothing

neighbors_m :: Matrix -> [AdmissibleDirection]-> Cell -> [Cell]
neighbors_m m ds c =
  map fromJust $ filter isJust $ map (\d -> neighbor m d c) ds

mIndexToNodeIndex :: Matrix -> Cell -> Int
mIndexToNodeIndex m c = (index (bounds m) c) + 1

lowerBound = fst
upperBound = snd

firstIndex bnds = 
  index bnds (lowerBound bnds)

lastIndex bnds = 
  index bnds (upperBound bnds)

constructTo :: Matrix -> Cell -> Cell -> LEdge Integer
constructTo m from to = 
  (mIndexToNodeIndex m from, mIndexToNodeIndex m to, m ! to)
                  
nodesOfM :: Matrix -> [LNode String]
nodesOfM m = map (\x -> (mIndexToNodeIndex m  x, show x)) $ indices m

edgesOfM :: Matrix -> [AdmissibleDirection] -> [LEdge Integer]
edgesOfM m ds =   
  concatMap (\(n, ns) -> map (constructTo m n) ns) cellsTo
  where cellsTo = map (\x -> (x, neighbors_m m ds x)) $ (indices m)
   
endNode = 1000000

listRowNodes m cl = 
  let row_indices = (\(x,y)->(fst x, fst y)) (bounds m) 
  in map (\x->(x,cl)) $ range row_indices

-- from Matrix, and list of admissible dimensions, 
-- this constructs searcheable graph.
constructGraph :: [AdmissibleDirection] -> Matrix -> Gr String Integer
constructGraph ds m =
  let left_nodes = listRowNodes m 0
      right_nodes = listRowNodes m (snd . snd $ bounds m)
  in mkGraph ((0,"Start"):(endNode,"End"):(nodesOfM m)) ((map (\x->(0,mIndexToNodeIndex m x, m!x)) left_nodes) ++ (map (\x->(mIndexToNodeIndex m x,endNode,0)) right_nodes) ++ (edgesOfM m ds))

-- Returns some edge that presents in the collection
findEdges haystack [] = []
findEdges haystack needles =
  map snd . filter fst . map (\n@(a,b,c)-> ((a,b) `elem` haystack, n)) $ needles
      
computePathCost :: Gr String Integer -> Path -> Integer
computePathCost g p = ufold pathFn 0 g
  where path_edges = zipWith (,) (init p) (tail p)
        pathFn cntx c = 
          c + (sum $ map (\(a,b,x)->x) es)
          where es = findEdges path_edges ((out' cntx) ++ (inn' cntx))
        
main = do
  ags <- getArgs
  m <- readMatrix (ags!!0)
  let g = constructGraph [DOWN, RIGHT, UP] m
  let bnds = bounds m
  let start = 0
  let end = endNode
  let p = sp start end g
  writeFile "small_matrix.dot" (graphviz' g)
  print $ map (lab g) p
  print $ computePathCost g p