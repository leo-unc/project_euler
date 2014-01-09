module Main (main,isPermutation) where

import Math.Sieve.Phi (sieve,phi)

import Data.List(sort,minimumBy)
import Data.Ratio
import Data.Digits
import Control.Monad.Instances 

-- Checks that (x,phi(x)) is a permutation, that is
-- x and phi(x) are permutation of each other.
isPermutation :: (Integral a, Ord a) => (a,a) -> Bool
isPermutation (a,b) =
  whenSorted (digits 10 a) (digits 10 b)
  where whenSorted x y =
          if length x /= length y
          then False
          else (sort x == sort y)

bb = 10^7-1

asRatio x = fst x % snd x

main = do
  let sv = sieve bb
  let l = [2..bb]
  print . minimumBy (\x y -> compare (asRatio x) (asRatio y)) . filter isPermutation $ zip l (map (phi sv) l)
  --print "foo"