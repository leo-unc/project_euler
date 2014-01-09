
import NumberLib

sum_of_factorial_of_digits = sum . (map factorial) . digits

wonderful_numbers a = let l = [3..a]
                      in let k = zip l (map sum_of_factorial_of_digits l) 
                         in filter (\x -> fst x == snd x) k