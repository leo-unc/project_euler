module Main where

import Math.Combinat.Sets (choose)
import Data.List (nub)

oneCube = choose 6 [0,1,2,3,4,5,6,6,7,8]

testArrangement :: ([Int],[Int]) -> Bool
testArrangement (as,bs) = 
  all id $ map (test as bs) [(0,1), (0,4), (0,6), (1,6), 
                             (2,5), (3,6), (4,6), (8,1)]
  where test as bs (a,b) = (a `elem` as && b `elem` bs || 
                            b `elem` as && a `elem` bs)
    
    
    
allCubes = [(i,j) | i <- oneCube, j <- oneCube] 


main = do
  let q = allCubes
  let p = filter testArrangement q
  print $ (length p) `div` 2