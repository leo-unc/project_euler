module Main (main) where

import Data.Digits (digits, unDigits)

addReverse x = x + ((unDigits 10 . reverse . digits 10) x)

isPalindromeNumber x = let ds = digits 10 x
                       in let p = if even $ length ds
                                  then div (length ds) 2
                                  else div (length ds - 1) 2
                          in let (a, b) = splitAt p ds
                             in if even $ length ds 
                                then a == reverse b
                                else a == reverse (drop 1 b)

hasPalindromeNumber x = not . null . filter isPalindromeNumber $ (tail x)

main = do
  print . length . filter (not . hasPalindromeNumber) . map (take 50) . map  (iterate addReverse) $ [1..10000]