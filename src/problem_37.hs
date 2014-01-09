module Main (main) where

import Data.List (inits,tails)
import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)

truncations x = let is1 = map (\x -> read x::Integer) . init . tail . inits . show $ x
                    is2 = map (\x -> read x::Integer) . init . tail . tails . show $ x
                in (x, is1 ++ is2)

main = do
  print . sum . take 11 . map fst . filter (\(x, xs) -> all isPrime xs) . (map truncations) . drop 4 $ primes