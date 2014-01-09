module Main() where

import System

readRow x = (read x)::Integer

main = do
  [f] <- getArgs
  s <- readFile f
  (putStrLn . (take 10) . show . sum . (map readRow) . lines) s