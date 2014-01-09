module Main() where

import System

readRow = (map (\x->(read x)::Int)) . words

readMatrix = (map readRow) . lines

rows = length

cols = length . head

take_elem m (i, j) = if i>= 0 && j >= 0 && i < rows m && j < cols m
                     then Just ((m !! i) !! j)
                     else Nothing

-- Creates lists of pairs corresponding to 4 elements in different
-- directions
index_collection (i, j) = let l = [0..3]
                          in [[(i+k, j)   | k <- l], [(i, j+k)  | k <- l],
                              [(i+k, j+k) | k <- l], [(i+k, j-k)| k <- l]]


all_indices m = [(i,j) | i <-[0..((rows m) - 1)], j <- [0..((cols m)-1)]]

find_sum_for_elements m s = foldr step 1 (map (take_elem m) s)
    where step x acc = case x of
                Just r -> r * acc
                Nothing -> acc

compute_max_sequence_at m = maximum . (map (find_sum_for_elements m)) . index_collection

total_max m = maximum (map (compute_max_sequence_at m) (all_indices m))

main = do
  [f] <- getArgs
  s <- readFile f
  (putStrLn . show . total_max . readMatrix) s