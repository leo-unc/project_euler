
import Data.Set (fromList, member)
import NumberLib

run_combine :: Int -> [Int] -> [[Int]]
run_combine k xs = if length xs >= k
                   then (take k xs) : (run_combine k (tail xs))
                   else []

run_combine' :: [Int] -> Int -> [[Int]]
run_combine' xs k = run_combine k xs

-- For each element in ks and list X returns series of k of elements of X 
-- e.g. series_of [2..4] [1..5] -> [[[1,2],[2,3],[3, 4],[4,5]],
--                                   [[1,2,3], [2, 3, 4], [
series_of :: [Int] -> [Int] -> [[[Int]]]
series_of ks xs = map (\x -> run_combine x xs) ks

-- As above except that sums are calculated
-- series_of [2..4] [1..10] -> [[3,5,7,9],
--                              [6, 9, ...]
sum_series_of :: [Int] -> [Int] -> [[Int]]
sum_series_of ks = (map (map sum)) . (series_of ks)

answer a b = map last (filter (not . null) (answer' a b))
             where answer' a b = let p = primes a
                                 in let ps = fromList p
                                    in map (filter (\x -> member x ps)) (map (takeWhile (\x-> x < a)) (sum_series_of [21..b] p))
            