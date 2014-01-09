module Main where

import System.Environment(getArgs)
import System.IO(readFile)
import qualified Data.Text as T
import Data.Ord(comparing,Ordering)
import Data.List(maximumBy, elemIndex)

processFile :: String -> [(Integer,Integer)]
processFile = map processLine . filter (not . null) . lines
  where processLine = toIntegerPair . (T.split (==',')) . T.pack
        toIntegerPair (a:b:_) = (toInt a, toInt b) 
        toIntegerPair _ = error "Bad input"
        toInt = (\x->read x::Integer) . T.unpack

findMax :: [(Integer, Integer)] -> Maybe Int
findMax ps = fmap (+1) $ elemIndex maxElement ps
  where maxElement = maximumBy (comparing pairToDouble) ps
        pairToDouble (a, b) = (fromInteger b::Double) * (log (fromInteger a::Double)) 

main = do
  fname <- fmap head getArgs
  lpairs <- fmap processFile $ readFile fname
  print $ findMax lpairs