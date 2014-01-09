module Main() where

import NumberLib

sum_digits_to_x x n = sum (map (^x) (digits n))

main = do
       let a = [2..(6*9^5)]
       (putStrLn . show . sum . (map (fst)) . (filter (\x->fst x == snd x))) (zip a (map (sum_digits_to_x 5) a))