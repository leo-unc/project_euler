module Main(main) where

import Data.List(nub,nubBy)
import Data.Tuple.Utils


lst = [(x,y,x*y) | x <- [2..9], y <- [1345..9876]] ++ [(x,y,x*y) | x <- [12..98], y <- [134..987]]

isPandigital x = let l = show x
                 in (length l == 9) && (all (/='0') l) && ((length $ nub l) == 9)

toNumber (x,y,z) = (read $ (show x ++ show y ++ show z))::Integer

main = do
  let t = nubBy (\x y-> (thd3 . snd) x == (thd3 . snd) y) $ filter (isPandigital.fst) $ zip (map toNumber lst) lst
  putStrLn $ show $ sum $ map (thd3 . snd) t
  putStrLn $ show t