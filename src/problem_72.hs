module Main (main) where

import Math.Sieve.Phi (sieve,phi)

b = 10^6
--b = 8

main = do
  let sv = sieve b
  print "foo"
  print $ sum $ map (phi sv) [2..b]
