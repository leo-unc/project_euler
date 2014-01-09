module Main where

import Math.NumberTheory.Moduli (powerMod)

m = 10000000000

main = do
  print ((28433 * powerMod 2 (7830457::Integer) m + 1) `mod` m)