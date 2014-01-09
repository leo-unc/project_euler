module Main (main) where

import Data.List (sort)
import Math.NumberTheory.Primes.Sieve (primes)


isPandigital x = let s = sort . show $ x
                 in if length s <= 1 || length s > 9
                    then False
                    else all id $ zipWith (==) ['1'..'9'] s

main = do
  print . maximum . filter isPandigital . takeWhile (<=987654321) $ primes