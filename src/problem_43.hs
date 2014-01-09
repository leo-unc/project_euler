module Main (main) where

import Data.Digits (digits,unDigits)
import Data.List (nub,(\\))
import Data.Maybe

isPandigital x = let d = digits 10 x
                 in if length d <= 1 || length d > 9
                    then False
                    else all id $ zipWith (==) [0..9] d

hasUniqueDigits x = let d = digits 10 x
                    in nub d == d
                       
generatorBase b = map (*b) [1..]            

lastTwoDigits x = let l = digits 10 x
                  in if length l > 2
                     then drop ((length l) - 2) l
                     else l

matchLastTwoDigits l2 = (l2==) . lastTwoDigits

data PartialState = 
  PartialState {
    usedDigits :: [Int]
    } deriving (Show) 

partialStateFromNumber x = let ds = digits 10 x
                           in if length ds < 2
                              then Nothing
                              else if length ds == 2
                                   then Just $ PartialState (0:ds)
                                   else Just $ PartialState ds
                                        
createPartialState s x = let ds = digits 10 x
                         in if length ds < 2
                            then Nothing
                            else let r1 = 0:(usedDigits s)
                                     r2 = (head ds) : (usedDigits s)
                                 in if length ds == 2
                                    then if nub r1 == r1 
                                         then Just $ PartialState r1
                                         else Nothing
                                    else if nub r2 == r2
                                         then Just $ PartialState r2
                                         else Nothing

getTripletsBase = filter (>10) . takeWhile (<=987) . generatorBase


expandState b s = let l2 = take 2 . usedDigits $ s
                  in let cand = filter (\y -> hasUniqueDigits y && matchLastTwoDigits l2 y) . getTripletsBase $ b
                     in map fromJust . filter isJust . map (createPartialState s) $ cand

seeds = map fromJust . filter isJust . map partialStateFromNumber . filter hasUniqueDigits . getTripletsBase $ 17                      

--seeds = [fromJust $ partialStateFromNumber 289]

convertToNumber x = let unused_digits = (\\) [0..9] $ usedDigits x
                    in if length unused_digits /= 1
                       then error ("PartialState " ++ (show x) ++ "Unused digits : " ++ (show unused_digits))
                       else unDigits 10 $ (head unused_digits):(usedDigits x)
                            
expandStates b = concatMap (expandState b)

main = do
  let l = map convertToNumber . expandStates 2 . expandStates 3 . expandStates 5 . expandStates 7 . expandStates 11 . expandStates 13 $ seeds
  print $ sum l