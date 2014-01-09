module Main (main) where

import Math.NumberTheory.Primes.Factorisation (factorise)

primeFactors = map fst . factorise
numPrimeFactors = length . primeFactors

testFactors n xs = let ts = dropWhile ((n/=) . numPrimeFactors) xs
                 in let ss = span ((n==) . numPrimeFactors) ts
                    in if (length $ fst ss) == n
                       then head $ fst ss
                       else testFactors n $ snd ss

main = do
  print "FOO"