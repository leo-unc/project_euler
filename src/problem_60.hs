module Main (main) where

import Debug.Trace (trace)
import Control.Applicative
import Data.Maybe (isJust, fromJust)
import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Logarithms (integerLogBase)

concatenateTwoNumbers :: Integer -> Integer -> Integer
concatenateTwoNumbers x y = 10 ^ ((integerLogBase 10 y) + 1) * x + y

concatenationIsPrime x y = isPrime $ concatenateTwoNumbers x y

concatenationForSetIsPrime s x = 
  and . map (\y -> (concatenationIsPrime y x) && (concatenationIsPrime x y)) $ s

data PartialSolution = PartialSolution 
                       {
                         partialSet :: [Integer]
                       , partialSum :: Integer
                       , restNums :: [Integer]
                       } deriving (Show)

unitPartialSolution x ns = PartialSolution { 
  partialSet = [x], partialSum = x, restNums = ns }

extendPartialSolution m = 
  if null $ restNums m
  then []
  else let h = head $ restNums m
       in if concatenationForSetIsPrime (partialSet m) h
          then (PartialSolution { 
                   partialSet = h:(partialSet m), 
                   partialSum = partialSum m + h, 
                   restNums = (tail $ restNums m) } :
                (extendPartialSolution $  PartialSolution {
                    partialSet = partialSet m,
                    partialSum = partialSum m,
                    restNums = (tail $ restNums m)}
                 ))
          else extendPartialSolution (PartialSolution {
                   partialSet = partialSet m,
                   partialSum = partialSum m,
                   restNums = (tail $ restNums m)
                   })
               

extendSolutions = (concatMap extendPartialSolution . 
                   concatMap extendPartialSolution . 
                   concatMap extendPartialSolution . 
                   concatMap extendPartialSolution)
  
main = do
  print . (sum . partialSet) . head . extendSolutions $ [unitPartialSolution 13 ((take 5000 . dropWhile (<=13)) $ primes)]