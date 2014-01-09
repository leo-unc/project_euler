module Main where

import Math.NumberTheory.Primes(divisorSum)
import Math.NumberTheory.Primes.Factorisation(factorise')
import System.Environment(getArgs)

data GeneratorStep = GeneratorStep {
  an :: Integer,
  partitionCount :: [Integer]
  } deriving (Show)

emptyStep = GeneratorStep {
  an = 1,
  partitionCount = [0, 1]
 }
            
-- | divisors = map divisorSum [1..]
primeDivisors = map primeDivisorSum [1..]

primeDivisorSum 1 = 0
primeDivisorSum x = 
  case factorise' x of    
    [] -> 0
    f@(f1:_) -> sum $ map fst $ f

partitionGeneratorStep g = 
  let n = 1 + an g
  in let gc' = (sum(zipWith (*) primeDivisors (partitionCount g))) `div` n
     in g { an = n, 
            partitionCount = (gc':(partitionCount g)) }
  
printG x = (an x, head $ partitionCount x)

main = do
  n <- fmap ((\x -> read x::Int) . head) getArgs
  let all_p = iterate partitionGeneratorStep emptyStep
  -- let p' = dropWhile (\GeneratorStep {getN=a, partitionCount=b} -> ((head b) `rem` n) /= 0) all_p
  let p' = drop n all_p
  print $ printG $ head p'