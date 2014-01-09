module Main where

import Math.NumberTheory.Powers (isSquare,integerSquareRoot)
import Data.Ratio

squares = map (^2) [1..]

goodSquares = filter isSquare . map (\x->3 * x + 1) $ squares

--isSquare :: Integral a => Ratio a -> Bool
--isSquare x = isSquare (numerator x) && isSquare (denominator x)

-- Heron's formula for the area of triangle (squared)
triAreaSquared :: Integer -> Integer -> Integer -> Ratio Integer
triAreaSquared a b c = 
  let p = (a+b+c) % 2
  in p * (p - a%1) * (p - b%1) * (p - c%1)

equilateralAreaSquared a b =
  triAreaSquared a a b

-- Finds integer solutions to Pell equation
-- x^2 - D* y^2 = 1, where D=3 and (x0,y0)=(2,1) is fundamental solution.
sequenceOfX :: [Integer]
sequenceOfX = 
  map fst ((x0,y0):(sequenceOfX' n0 x0 y0))
  where x0 = 2
        y0 = 1
        n0 = 3
        sequenceOfX' n x y = 
          let xn = x0*x + n * y0 * y
              yn = x0*y + y0 * x
          in (xn, yn):(sequenceOfX' n xn yn)

-- Triangle (a a a-1)
-- p = (3a-1) / 2
-- S^2 = (3a-1)/2 * (a-1)^2/4 * (a+1)/2 = 
--     = (a-1)^2/4 * 1/4 * (3a-1)(a+1) -- needs to be Int
--
-- (3a-1)*(a+1) = 4 * d^2
-- a1 = 1/3(2*sqrt(3*d^2+1) - 1) 
--
-- Triangle (a a a+1)
-- p = (3a+1) / 2
-- S^2 = (3a+1)/2 * (a+1)^2/4 * (a-1) / 2 =
--     = (a+1)^2/4 * 1/4 * (3a+1)(a-1) -- needs to be Int
--
--
-- (3a+1)*(a-1) = 4 * d^2
-- a2 = 1/3(2*sqrt(3*d^2+1) + 1) 
main = do
  let a1 = map numerator . filter (\x->denominator x ==1 ) . map (\x->(-1 + 2 * x) % 3) $ sequenceOfX
  let a2 = map numerator . filter (\x->denominator x ==1 ) . map (\x->(1 + 2 * x) % 3) $ sequenceOfX
  print $takeWhile (\x-> 3*x-1 <=1000000000) a1
  print $takeWhile (\x-> 3*x+1 <=1000000000) a2
  let p1 = takeWhile (<=1000000000) . map (\x->3 * x - 1) $ a1    
  let p2 = takeWhile (<=1000000000) . map (\x->3 * x + 1) $ a2      
  print $ (sum p1) + (sum p2) - 2 -- 2 is perimiter of (1,1,0) triangle