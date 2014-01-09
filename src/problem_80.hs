module Main (main) where

import Debug.Trace
import Math.NumberTheory.Powers
import Data.Ratio
import Data.List (iterate)
import Data.Digits (digits)

-- Computes rational approximation of square root of n with precision eps
-- abs(x - n^(1/2)) < eps using Newton's method
computeSquareRoot :: Ratio Integer -> Integer -> Ratio Integer
computeSquareRoot eps n = 
  getValue $ advance $ iterate rootStep (n % 1, n % 1)
  where getValue = fst . head
        advance = dropWhile (\(x,err) -> err > eps) 
        rootStep (x, err) = 
          let y = x - (x ^ 2 - ( n % 1)) / ( (2 % 1) * x)
              e = err ^ 2 / ( (2 % 1) * x)
          in (y, e)
               
takeNDigits :: Integer -> Ratio Integer -> [Integer]
takeNDigits n x = digits (10::Integer) $ (\x -> (floor x)::Integer) $ x * ((10::Integer) ^ (n - 1) % 1)
  
takeNDecimalDigits :: Integer -> Ratio Integer -> [Integer]  
takeNDecimalDigits n x =
  takeNDigits n (10 * (x - (((floor x)::Integer) % 1)))

epsilon = (1 % (10::Integer)^100)

sumManyDecimalDigits :: Integer -> Integer
sumManyDecimalDigits =
  sum . takeNDigits 100 . computeSquareRoot epsilon

known2 = digits 10 4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727
known51 = digits 10 1414284285428499979993998113672652787661711599027338332084308827658204064400218862589882135328204182
known99 = digits 10 9498743710661995473447982100120600517812656367680607911760464383494539278271315401265301973848719527  

main = do
  --print (known2 == takeNDecimalDigits 100 (computeSquareRoot epsilon 2))
  --print (known51 == takeNDecimalDigits 100 (computeSquareRoot epsilon 51))
  --print (known99 == takeNDecimalDigits 100 (computeSquareRoot epsilon 99))
  print $ sum $ map sumManyDecimalDigits $ filter (not . isSquare) [2..99]
  -- print $ takeNDigits 100 $ computeSquareRoot epsilon 99
  