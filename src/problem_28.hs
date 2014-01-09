module Main(main) where

spiralCorners = 1 : spiralCorners' 1 1
    where spiralCorners' l k = let f = map (l+) (map ((2*k)*) [1..4])
                               in f ++ (spiralCorners' (last f) (k+1))

main = do
  (putStrLn . show . sum . (take (1+4*500))) spiralCorners