
filterMultiplesOf x xs = filter (\n -> (mod n x) /= 0) xs

sieve (x:xs) = [x] ++ (sieve (filterMultiplesOf x xs))
sieve [] = []

eratosthenesSieve n = sieve [2..n]