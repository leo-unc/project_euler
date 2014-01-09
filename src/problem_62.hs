module Main (main) where

import qualified Data.Map as Map
import Math.NumberTheory.Powers.Cubes (isCube')
import Math.NumberTheory.Powers (exactCubeRoot)
import Data.Digits (digits,unDigits)
import Data.List (sort)

keyFromNumber :: Integer -> String
keyFromNumber = sort . show 

cubes = map (^3) $ [2..]

keysHaveSameLength k1 k2 =
  length k1 == length k2

-- testHead takes map m, 
testHead (c:cs) =  
  testHead' (Map.singleton (keyFromNumber c) [c]) cs
  where testHead' m (c:cs) = 
          let m' = Map.insertWith (++) (keyFromNumber c) [c] m
              k = (keyFromNumber c)
          in let vs = Map.findWithDefault [] k m'
             in if length vs == 5
                then vs
                else let m'' = if keysHaveSameLength k (fst . Map.findMin $ m') 
                               then m'
                               else (Map.filterWithKey (\k' _ -> length k' >= length k) m') 
                     in testHead' m'' cs

main = do
  print $ testHead cubes