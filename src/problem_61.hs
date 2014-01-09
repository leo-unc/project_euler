module Main (main) where

import Control.Applicative
import NumberLib (triangleNumber, squareNumber, pentagonalNumber, hexagonalNumber, heptagonalNumber, octagonalNumber)

import Data.List (permutations)

-- This represents a chain of numbers consisting of one elemnt
unitPath = (:[])

-- This tests if suffix of a is prefix of b.
testPrefixNumber a b = (a `mod` 100) == (b `div` 100)
testSuffixNumber a b = (b `mod` 100) == (a `div` 100)  
                       
-- This returns a list of paths that can be created by concatenating p and 
-- element x in numberSet such that the last two digits of p are equal to
-- the first two digits of x. If p cannot be grown, it is dropped.
growPath numSet p = map (:p) . filter (testSuffixNumber (head p)) $ numSet

growPaths numSet = concatMap (growPath numSet)

filterNums genF = dropWhile (<=1000) . takeWhile (<= 9999) . map genF $ [1..] 

isCyclic p = testSuffixNumber (head p) (last p)

constructPaths ps = foldr growPaths (map unitPath (last ps)) (init ps) 

main = do
  let triNums = filterNums triangleNumber
  let sqNums = filterNums squareNumber
  let pNums = filterNums pentagonalNumber
  let hexNums = filterNums hexagonalNumber
  let hepNums = filterNums heptagonalNumber
  let octNums = filterNums octagonalNumber
  -- print . constructPaths $ [triNums, sqNums, pNums, hexNums, hepNums, octNums] 
  print . map sum . filter isCyclic . concatMap constructPaths . permutations $ [triNums, sqNums, pNums, hexNums, hepNums, octNums] 