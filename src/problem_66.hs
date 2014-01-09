module Main (main) where

import Data.Ord
import Data.List
import Data.Ratio
import Math.NumberTheory.Powers

-- Representation of (a*sqrt(x) + b) / c
data IrrationalSquareRoot  = IrrationalSquareRoot { ia :: Integer,
                                                    ib :: Integer,
                                                    ic :: Integer,
                                                    ix :: Integer } 
                       deriving (Show, Ord, Eq);

simpleSquareRoot x = IrrationalSquareRoot 1 0 1 x

floatRep a b c x = ((fromInteger a) * 
                    (sqrt $ fromInteger x) + 
                    (fromInteger b)) / (fromInteger c)

maxIntegerLessThan' a b c x = 
  floor $ floatRep a b c x
  
maxIntegerLessThan :: IrrationalSquareRoot -> Integer
maxIntegerLessThan (IrrationalSquareRoot a b c x) = 
  maxIntegerLessThan' a b c x

normalizeSQ :: IrrationalSquareRoot -> IrrationalSquareRoot
normalizeSQ (IrrationalSquareRoot a b c x) = 
  if c < 0
  then normalizeSQ (IrrationalSquareRoot (-a) (-b) (-c) x)
  else let t = ((a `gcd` c) `gcd` b)
       in if t == 1
          then IrrationalSquareRoot a b c x
          else IrrationalSquareRoot (a `div` t) (b `div` t) (c `div` t) x

chainedExpansion :: IrrationalSquareRoot -> [(Integer,IrrationalSquareRoot)]
chainedExpansion (IrrationalSquareRoot a b c x) = do
  let x0 = maxIntegerLessThan' a b c x
  let y1 = normalizeSQ (IrrationalSquareRoot (c * a) ((x0 * c - b) * c) (a^2 * x - (b - x0 * c) ^ 2) x)  
  let e = chainedExpansion y1
  (x0, y1) : e                      

convergent xs = do
  let fn x y = x + 1 / y
  foldr1 fn xs

convergents :: Fractional a => [a] -> [a]
convergents = map convergent .  tail . inits

simpleRatio :: Integer -> Ratio Integer
simpleRatio x = x % 1


findSolutionX :: Integer -> Integer
findSolutionX d =
  numerator . head . dropWhile (not . testSolution) $ c
  where c = convergents . map (simpleRatio . fst) . chainedExpansion . simpleSquareRoot $ d
        testSolution r = ((numerator r) ^ 2 - d * (denominator r) ^ 2 == 1)


main = do
  print $ maximumBy (comparing fst) . map (\t -> (findSolutionX t, t)) . filter (not . isSquare) $ [2..1000]