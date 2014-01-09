module Main (main) where

import Data.Ratio
import Data.List (inits)
import Data.Digits

convergent xs = do
  let fn x y = x + 1 / y
  foldr1 fn xs

convergents :: Fractional a => [a] -> [a]
convergents = map convergent .  tail . inits

simpleRatio :: Integer -> Ratio Integer
simpleRatio x = x % 1

fractionsOfE :: [Ratio Integer]
fractionsOfE = simpleRatio 2 : (concatMap (\x -> [simpleRatio 1, 
                                                  simpleRatio (2 * x), 
                                                  simpleRatio 1]) [1..])
               
answer k = sum . digits 10 . numerator . head . drop (k - 1) . convergents $ fractionsOfE

main = do
  print $ answer 100