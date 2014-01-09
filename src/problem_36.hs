module Main(main) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isPalindromicBase a x = let s = showIntAtBase a intToDigit x ""
                        in s == reverse s

isPalindromic x = isPalindromicBase 2 x && isPalindromicBase 10 x

main = do
  print . sum . filter isPalindromic $ [1..999999]