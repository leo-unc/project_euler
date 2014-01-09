module Main (main) where

import NumberLib (binomial)

main = do
  print . length . filter (>1000000) $ [binomial i j | i <- [1..100], j<-[0..i]]