module Main(main) where

import Data.Ratio

digits = [1..9] 
         
createTwoDigitRatio i (a,b) = [ ((a*10 + i, b*10 + i), (a,b)),
                                ((i*10 + a, i*10 + b), (a,b)),
                                ((a*10 + i, i*10 + b), (a,b)),
                                ((i*10 + a, b*10 + i), (a,b)) ]

oneDigitRatios = filter (\x->fst x < snd x) [(a,b) | a <- [1..9], b <- [1..9]]
  
-- 

isUnorthodox ((a, b), (c, d)) = (a % b == c % d)

main = do
  print . product . map (\((a, b),(c,d)) -> c % d) . filter isUnorthodox . concat $ [y j| y <- map (\x -> createTwoDigitRatio x) digits, j <- oneDigitRatios]