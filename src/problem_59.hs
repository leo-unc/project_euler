module Main(main) where

import Data.Bits (xor)
import Data.Char (ord,chr)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Set as Set

parseEncodedText :: T.Text -> [Int]
parseEncodedText =  map (\x -> (read $ T.unpack x)::Int) . T.split (==',')

decryptXORTextWithKey :: [Int] -> [Int] -> [Int]
decryptXORTextWithKey encryptedText secretKey = zipWith xor encryptedText (cycle secretKey)

intTextToString :: [Int] -> String
intTextToString = map chr

textGoodness :: Set.Set String -> String -> Int
textGoodness dictionary = Set.size . Set.intersection dictionary . Set.fromList . words

loadDictionary :: String -> IO (Set.Set String)
loadDictionary = fmap (Set.fromList . toWordList) . readFile
                 where 
                   unquote = filter (/='"')
                   toWordList = map (unquote . T.unpack . T.toLower) . T.split (==',') . T.pack

possibleKeys = [(ord a):(ord b):(ord c):[] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']] 

decodeText dictionary encodedText =
  snd . head . sortBy (comparing (negate.fst)) . map (\x -> (textGoodness dictionary x, x)) . map (intTextToString . (decryptXORTextWithKey encodedText)) $ possibleKeys

main = do
  englishWords <- loadDictionary "words.txt"
  encodedText <- fmap (parseEncodedText . T.pack) $ readFile "problem_59_cipher.txt"
  let decodedText = decodeText englishWords encodedText
  print . sum . map ord $ decodedText    