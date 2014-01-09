module Main (main) where


import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.Digits (digits,unDigits)

generateSelections = filter (not.null) . generateSelectionsStarting 0
  where generateSelectionsStarting x l = if x == l-1
                                         then [[l-1], []]
                                         else (map (x:) (generateSelectionsStarting (x+1) l)) ++ (generateSelectionsStarting (x+1) l)

replaceDigitsAtPositionsWith [] xs d = xs
replaceDigitsAtPositionsWith (p:ps) xs d = 
  let (f, _:ys) = splitAt p xs
  in replaceDigitsAtPositionsWith ps (f ++ (d:ys)) d 

extractPrimesFromPattern x ps = filter isPrime . map (unDigits 10) . filter ((0/=) . head) . map (replaceDigitsAtPositionsWith ps (digits 10 x)) $ [0..9]

testNumber x = filter ((8==) . length) . map (extractPrimesFromPattern x) $ (generateSelections . length . digits 10 $ x)

main = do
  print . head . dropWhile null . map testNumber $ primes
  