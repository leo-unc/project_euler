-- Library of number-theoretic functions.

module NumberLib 
    (
     divisibleBy
    , triangleNumber
    , squareNumber 
    , pentagonalNumber
    , hexagonalNumber
    , heptagonalNumber
    , octagonalNumber
    , factorial
    , binomial) where

import Data.List
import Data.Char (digitToInt)

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy n x = (n `mod` x == 0)


triangleNumber :: Integral a => a -> a
triangleNumber n = n * (n+1) `div` 2

squareNumber :: Integral a => a -> a
squareNumber n = n^2

pentagonalNumber :: Integral a => a -> a
pentagonalNumber n = n * (3 * n - 1) `div` 2

hexagonalNumber :: Integral a => a -> a
hexagonalNumber n = n * (2 * n - 1)

heptagonalNumber :: Integral a => a -> a
heptagonalNumber n = n * (5 * n - 3) `div` 2

octagonalNumber :: Integral a => a -> a
octagonalNumber n = n * (3 * n - 2)

-- Returns list of carthesian products of two lists using function f.
carthesian_two f xs ys = [f x y| x <- xs, y<- ys]

-- Given a list of lists L1,L2,...LN returns a list of lists with
-- elements in L1xL2xL3xLN
carthesian_lists xs = foldr (carthesian_two (:)) (map (\t->t:[]) (last xs)) (init xs)

-- Factorial of a number
factorial :: Integral a => a -> a
factorial a = if a > 2
              then product [2..a]
              else if a == 0
                   then 1
                   else a

-- Binomial coefficient (n k)
binomial :: Integral a => a -> a -> a
binomial n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))