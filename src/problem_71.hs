module Main where

import Data.Ratio

target = 3 % 7 

numerators = map (\x->(x * numerator target - 1) `div` denominator target)

main = do
  print $ maximum $ zipWith (%) (numerators [2..1000000]) [2..1000000]