module Main (main) where

import Data.Ord (comparing)
import Data.Foldable (maximumBy)

rightTriangles p = [(a, b ,p-a-b) | a <- [1..p-1], b <- [1..(p-a)], a < b && a^2 + b^2 == (p-a-b)^2]

numRightTriangles p = sum [1 | a <- [1..p-1], b <- [1..(p-a)], a < b && a^2 + b^2 == (p-a-b)^2]

main = do
  print . maximumBy (comparing snd) . map (\x -> (x, numRightTriangles x)) $ [1..1000]