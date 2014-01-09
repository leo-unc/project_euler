module Main(main) where

import Char(ord)
import Data.List(sort)
import IO(hGetContents,stdin)
import Text.CSV
import Text.ParserCombinators.Parsec.Error(ParseError)

myOrd a = (ord a) - 64

computeScore = sum . (map myOrd)

processData:: Either ParseError CSV -> Int
processData (Right rows) = processNameList (head rows)
processData _ = undefined

processNameList :: [String] -> Int
processNameList l = let s = sort l
                        k = [1..length l]
                    in sum (zipWith (*) k (map computeScore s))

main = do
  s <- hGetContents IO.stdin
  (putStrLn . show . processData . (parseCSV "")) s