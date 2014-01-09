module Main(main) where

import Math.NumberTheory.Primes.Testing (isPrime)

diffN n = 2 * (ceiling ((n - 1)/4))

spiralCorners =  map (1+) . scanl1 (+) . map diffN $ [1..]

isPrime' x = if isPrime x
             then 1
             else 0

findPrimes = map (\x -> (isPrime' x, x)) 

countPrimes = tail . scanl (\s p -> s + fst p) 0 . findPrimes

firstNumWhenLowPercentage = head . dropWhile (\(x,y) -> 100 * x >=  10 * y)

main = do
  let primeCount =  countPrimes spiralCorners
  print . (1+) . (\x -> div x 2) . snd . firstNumWhenLowPercentage . drop 1  $ zip primeCount [1..]
