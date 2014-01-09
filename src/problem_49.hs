module Main(main) where

import Data.Digits (digits,unDigits)
import Data.Traversable (traverse)
import Data.List (nub,sort,mapAccumL,mapAccumR,intersect)
import Math.NumberTheory.Primes.Sieve (primes)
import qualified Data.Map.Lazy as Map

candList = dropWhile (<1000) . takeWhile (<9998) $ primes

addValueToMap m x = (Map.insertWith (++) (fst x) [snd x] m, x)

keyFromNumber = concatMap show . sort . digits 10

filterSeqLength = ((3<=) . length)

findArithmeticSubsequence [] = []
findArithmeticSubsequence (x:[]) = []
findArithmeticSubsequence (x:y:[]) = [] 
findArithmeticSubsequence xs = let x = head xs
                               in let ds = map (+(negate x)) . tail $ xs
                                  in let r = intersect  (map ((x+) . (2*)) ds) xs
                                     in if not $ null r
                                        then [x:(fst . properFraction . (/2) $ fromIntegral (x+head r)):head r:[]]
                                        else findArithmeticSubsequence $ tail xs

main = do
  print . concatMap findArithmeticSubsequence . filter filterSeqLength  . Map.elems . fst . mapAccumR addValueToMap Map.empty . map (\x -> (keyFromNumber x, x)) $ candList
