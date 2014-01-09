module Main (main) where

import Data.Digits (digits)

digitalSum = sum . digits 10

main = do
  print . maximum . map digitalSum $ [a^b | a <- [1..99], b <- [1..99]]