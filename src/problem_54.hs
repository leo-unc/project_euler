module Main (main) where

import Data.List (elemIndex,group,sortBy)
import Data.Char (digitToInt)
import Data.Ord (comparing)

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten  | Jack | Queen | King | Ace
               deriving (Show, Eq, Enum, Ord)
          
data Suit = Spades | Hearts | Diamonds | Clubs
          deriving (Show, Enum, Eq)

suitFromString s = case last s of
  'H' -> Hearts
  'S' -> Spades
  'D' -> Diamonds
  'C' -> Clubs
  
data Card = Card {
    card_value :: CardValue
  , suit :: Suit
  } deriving (Show, Eq)
             
data HandRank = HighCard | OnePair | TwoPairs | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush
              deriving (Show, Eq, Ord, Enum)
             
instance Ord Card where
  compare x y = compare (card_value x) (card_value y)

constructValue (c1:_) = if c1 `elem` ['2'..'9']
                        then case c1 of 
                          '2' -> Two
                          '3' -> Three
                          '4' -> Four
                          '5' -> Five
                          '6' -> Six
                          '7' -> Seven
                          '8' -> Eight
                          '9' -> Nine
                        else case c1 of          
                          'T' -> Ten
                          'J' -> Jack
                          'Q' -> Queen
                          'K' -> King
                          'A' -> Ace
                             
constructCard ss = Card { 
    card_value = (constructValue ss)
  , suit = (suitFromString ss) 
  }
                   
constructHand = sortBy (comparing card_value ) . map constructCard

constructPlayerHands ss = (constructHand . take 5 $ words ss, constructHand . drop 5 $ words ss)

data HandValue = HandValue { 
    rank :: HandRank
  , values :: [(Int, CardValue)]
  } deriving (Show, Eq)
             

instance Ord HandValue where
  compare x y = let rc = compare (rank x) (rank y)
                in if rc == EQ
                   then compare (values x) (values y)
                   else rc
                 
sameSuit ((Card _ st):cs) = and . map (st==) . map suit $ cs

-- Produces counts of values.
countValues ((Card v s):cs) = 
  map (\x -> (length x, head x)) . group . map card_value $ ((Card v s):cs) 

compareValueCounts (a, b) (x, y) =
  case compare a x of
    LT -> LT
    GT -> GT
    EQ -> compare b y

reverseSortedOrderCounts = reverse . sortBy compareValueCounts . countValues

isNext a b = a /= b && b == succ a

consecutiveValues (a:b:c:d:e:[]) = 
  let (a':b':c':d':e':[]) = map card_value [a,b,c,d,e]
  in isNext a' b' && isNext b' c' && isNext c' d' && isNext d' e'


handValue hand = let rsoc = reverseSortedOrderCounts hand
                 in if sameSuit hand
                    then if consecutiveValues hand
                         then case last hand of 
                           Card Ace _ -> HandValue {
                             rank = RoyalFlush, values = rsoc
                             }
                           Card t _ -> HandValue {
                             rank = StraightFlush, values = rsoc
                             }
                         else HandValue {
                           rank = Flush, values = rsoc
                           }
                    else if consecutiveValues hand
                         then HandValue {
                           rank = Straight, values = rsoc
                           }
                         else rankRest hand
                   
                              
rankRest hand = let rsoc = reverseSortedOrderCounts hand 
                in case fst (head rsoc) of
                  4 -> HandValue {
                    rank = FourKind
                    , values = rsoc
                    }
                  3 -> if fst (head . drop 1 $ rsoc) == 2
                       then HandValue {
                         rank = FullHouse, values = rsoc
                         }
                       else HandValue {
                         rank = ThreeKind, values = rsoc
                         }
                  2 -> if fst (head . drop 1 $ rsoc) == 2
                       then HandValue {
                         rank = TwoPairs, values = rsoc 
                         }
                       else HandValue {
                         rank = OnePair, values = rsoc
                         }
                  1 -> HandValue { rank = HighCard, values = rsoc }
                         
main = do
  value <- fmap (length . filter (\(x , y) -> handValue x > handValue y) . map constructPlayerHands . lines) (readFile "poker.txt")
  print value 
                         
  