module Main where

-- | Solution for Amicable chains problem (problem 95)

import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust,fromJust)
import Math.NumberTheory.Primes(divisorSum)
import Math.NumberTheory.Primes.Factorisation(factorise')
import Data.Ord (comparing)
import Data.Function(on)
import Data.List(maximumBy)

class Amicable a where
  isAmicable :: a -> Bool

properDivisorSum :: Maybe Int -> Maybe Int
properDivisorSum (Nothing) = Nothing
properDivisorSum (Just 0) = Nothing
properDivisorSum (Just x) = Just $ ((fromIntegral (divisorSum int_x - int_x))::Int)
  where int_x = fromIntegral x::Integer

data SingleChain = SingleChain {
  firstElement :: Int,
  chain::[Int],
  seen::IntSet.IntSet,
  getIsAmicable :: Bool
  } | EmptyChain | BadChain deriving (Show)
                          
instance Amicable SingleChain where
  isAmicable = getIsAmicable
  
isSeen :: SingleChain -> Int -> Bool
isSeen EmptyChain _ = False
isSeen c x = IntSet.member x (seen c)

-- Recursive step of adding a number to digit-factorial chain.
chainStep :: SingleChain -> [Int] -> SingleChain

chainStep c [] = c

chainStep EmptyChain (n:ns) = 
  chainStep (SingleChain {
    firstElement = n,
    chain = [n],
    seen = IntSet.singleton n,
    getIsAmicable = False
    }) ns

chainStep c (n:ns) =
  if isSeen c n
  then if n /= firstElement c
       then c   
       else c {
         getIsAmicable = True
         }
  else if n > 1000000 
       then BadChain
       else chainStep (c {
                          chain = n:(chain c),
                          seen = IntSet.insert n (seen c)
                         }) ns

computeProperDivisorChain :: Int -> SingleChain
computeProperDivisorChain =
  (chainStep EmptyChain) . (map fromJust) . (takeWhile isJust) . (iterate properDivisorSum) . Just

data ChainLength = ChainLength {
  getChainLength :: Int,
  getIsAmicableLength :: Bool
} deriving (Show)

instance Amicable ChainLength where
  isAmicable = getIsAmicableLength

-- This map stores properDivisorSum chain lengths for  N
newtype DivisorChain = DivisorChain {
  knownChains :: IntMap.IntMap ChainLength
} deriving (Show)

emptyChains = DivisorChain {
  knownChains = IntMap.empty
}

updateWithNonPeriodicChain kchs new_chain = 
  IntMap.union kchs (IntMap.fromList pair_ts)
  where npch = reverse $ chain new_chain
        ts = zipWith (,) npch [(length npch),(length npch -1)..1]
        not_known_ts = takeWhile (\e->not $ IntMap.member (fst e) kchs) ts
        pair_ts = map (\x -> (fst x, ChainLength {
                                  getChainLength = snd x,
                                  getIsAmicableLength = isAmicable new_chain
                                  })) not_known_ts
   
computeChainState ch x = 
  case new_ch of
    BadChain -> ch
    _ -> ch {
      knownChains = updateWithNonPeriodicChain kchs new_ch
      }
    where kchs = knownChains ch
          new_ch = computeProperDivisorChain x

endNum  = 100000
main = do
  let q = foldl computeChainState emptyChains [1..endNum]
  let l = filter (isAmicable . snd) . filter (\x-> fst x < endNum) . IntMap.toList $ knownChains q
  let m = maximumBy (comparing (getChainLength . snd)) l
  print $ m
  --let q = computeProperDivisorChain 6
  --print $ q