module Main(main) where


-- Fib generator of fibbonaci numbers
fib :: [Integer] 
fib = 1 : (fib' 1 1)
    where fib' a b = a : (fib' (a+b)  a)
                       
main = do
  (putStrLn . show) (head (dropWhile (\x-> fst x <10^999) (zip fib [1..])))