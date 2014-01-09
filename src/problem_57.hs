module Main (main) where

import Data.Digits (digits)
import Data.Ratio

f x = 2 + 1 / x

numeratorHasMoreDigits x = (length . digits 10 . numerator) x > (length . digits 10 . denominator) x

main = do
  print . length . filter numeratorHasMoreDigits .  map ((1+) . (1 / )) . (take 1000) $ iterate f (2 % 1)