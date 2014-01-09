module Main(main) where

import Data.List(sortBy)
import Math.NumberTheory.Primes.Testing(isPrime)

genNumbers a b = map (\x-> x^2+a*x+b) [0..]

seriesForCoeff (a, b) = genNumbers a b

lengthPrimeList xs = if head xs <= 1
                     then 0
                     else length $ fst $ break (not . isPrime) xs



findCoeff = let all_coeffs = [(a,b) | a <- [-999..999], b <- [2..999]]
            in  filter (\x-> snd x > 1) (map (\x->(x, lengthPrimeList $ seriesForCoeff x)) all_coeffs)

compareSnd x y = if snd x < snd y 
                 then GT
                 else if snd x == snd y
                      then EQ
                      else LT
main = do
  putStrLn $ show $ (\x-> fst x * snd x) $ fst $ head $ sortBy compareSnd findCoeff