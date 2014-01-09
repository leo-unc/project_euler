module Main(main) where

import qualified Data.Set as Set
import Math.NumberTheory.Primes(divisorSum)

limit = 28123 :: Integer

isAbundant :: Integer -> Bool
isAbundant n = (divisorSum n) > 2 * n

abundantNumbersList :: [Integer]
abundantNumbersList = filter isAbundant [1..limit]

abSums = [x + y | x <- abundantNumbersList, y <- abundantNumbersList]

main = do
  (putStrLn . show ) ((div (limit*(limit+1)) 2) - (Set.fold (+) 0  (Set.fromList (filter (<limit) abSums))))