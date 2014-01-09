module Main (main) where

import Data.Char (ord)
import Data.Text (split,unpack,pack)

letterScore x = ord x - 65 +1

wordScore = sum . map letterScore

isTriangle :: Int -> Bool
isTriangle x = let t = div (-1 + (floor $ realToFrac $ (sqrt $ fromIntegral (1 + 8 * x)))) 2
               in t * (t+1) == 2 * x

unquote = filter (/='"')

toWordList = map unquote . map unpack . split (==',') . pack

countTriangleWords = show . length . filter isTriangle . map wordScore . toWordList

main = do
  interact countTriangleWords