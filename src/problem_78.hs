module Main where

import Math.NumberTheory.Primes(divisorSum)
import System.Environment(getArgs)

data GeneratorStep = GeneratorStep {
  getN :: Integer,
  partitionCount :: [Integer]
  } deriving (Show)

emptyStep = GeneratorStep {
  getN = 1,
  partitionCount = [1, 1]
 }
            
divisors = map divisorSum [1..]

partitionGeneratorStep g = 
  let n = getN g + 1
  in let gc' = (sum(zipWith (*) divisors (partitionCount g))) `div` n
     in g { getN = n , partitionCount = (gc':(partitionCount g)) }
  
printG x = show (getN x) ++ "  " ++ show (head $ partitionCount x)

main = do
  n <- fmap ((\x -> read x::Integer) . head) getArgs
  let all_p = iterate partitionGeneratorStep emptyStep
  let p' = dropWhile (\GeneratorStep {getN=a, partitionCount=b} -> ((head b) `rem` n) /= 0) all_p
  print $ printG $ head  p'