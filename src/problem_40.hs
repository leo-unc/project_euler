module Main (main) where

import Control.Applicative
import Data.Char(digitToInt)

irrationalSequence = concat . map show $ [1..]

selectIndices ixs xs = (fmap (\x y -> y !! x) ixs) <*> pure xs

main = do
  let s = irrationalSequence
  let idx = (map (\x -> x - 1) . take 7 $ iterate (10*) 1)
  print . product . map digitToInt $ selectIndices idx s
                                       