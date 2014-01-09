module Main(main) where

import IO(stdin,hGetContents)

findMaxPath = maximum . (foldl step [])
    where step acc x = if length x == 1
                       then x
                       else let l1 = zipWith (+) acc (init x)
                                l2 = zipWith (+) acc (tail x)
                            in [head l1] ++ (zipWith max (tail l1) (init l2)) ++ [last l2]

rInt :: String -> Integer
rInt = read

parseTriangle :: String -> [[Integer]]
parseTriangle =  parseLines . lines
    where parseLines = map parseLine
              where parseLine = (map rInt) . words

main = do
  f <- hGetContents IO.stdin
  (putStrLn . show . findMaxPath . parseTriangle) f