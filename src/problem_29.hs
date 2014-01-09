module Main(main) where

import qualified Data.Set as Set

main = do
  (putStrLn . show . Set.size . Set.fromList) [x^y | x<-[2..100], y<-[2..100]]