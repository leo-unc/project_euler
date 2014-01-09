-- Problem 12

import Data.List

divisors n = foldr step [] [1..n]
    where step x xs = if (mod n x == 0)
                      then x:xs
                      else xs

triangle_num n = n*(n+1) `div` 2

carthesian f xs ys = map (\t -> f (fst t) (snd t)) [(x, y) | x <- xs, y <- ys]

divisors_length n = if odd n 
                    then ((length.divisors) n)*((length.divisors) ((n+1) `div` 2))
                    else ((length.divisors) (n `div` 2)) * ((length.divisors) (n+1))


divisors_of_triangle n = nub (carthesian (*) as bs)
    where as = if odd n
               then divisors ((n+1) `div` 2)
               else divisors (n `div` 2)
          bs = if odd n
               then divisors n
               else divisors (n+1)
                         