module Main (main) where

import Math.NumberTheory.Primes.Sieve (primes)

main = do
    print . product . take 7 $ primes