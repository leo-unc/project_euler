module Main (main) where

import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)

goldbachTest x =  not . null . filter ((0==) . snd) .map (properFraction . sqrt . (/2) . fromIntegral . (x+) . negate) . takeWhile (<=(x - 2)) $ primes

main = do
  print . head . dropWhile goldbachTest . filter (not.isPrime) . map ((1+) . (2*)) $ [1..]