module Main (main) where

import Data.Digits (digits)
import Data.List (sort)

testNumber x = let z = map sort . map (digits 10) $ [x, 2*x, 3*x, 4*x, 5*x, 6*x]
               in and . map (head z ==) $ z

main = do
  print . head . dropWhile (not . testNumber) $ [1..]