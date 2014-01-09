module Main where

import Data.Monoid
import Data.Digits (digits)
import NumberLib (factorial)
import qualified Data.IntSet as IntSet (IntSet, fromList, member, insert)
import qualified Data.IntMap as IntMap 
import Data.List(tails)

sumFactorialDigits = sum . map factorial . digits 10

endNum=1000000

data SingleChain = SingleChain {
  chain::[Int],
  seen::IntSet.IntSet 
  } deriving (Show)

isSeen :: SingleChain -> Int -> Bool
isSeen c x = IntSet.member x (seen c)

emptyChain = SingleChain {
  chain = [],
  seen = IntSet.fromList []
  }
             
-- Recursive step of adding a number to digit-factorial chain.            
chainStep :: SingleChain -> [Int] -> SingleChain
chainStep c (n:ns) = 
  if isSeen c n
  then c
  else  chainStep (c {
    chain = n:(chain c),
    seen = IntSet.insert n (seen c)
    }) ns

-- Compute a digit-factorial chain for a single number.
computeNonPeriodicChain x =
  let p = iterate sumFactorialDigits x
  in chainStep emptyChain p

-- This maps starting number to the length of non-periodic digit-factorial 
-- chain.
data FactChains = FactChains {
  knownChains :: IntMap.IntMap Int
  } deriving (Show)

emptyChains = FactChains {
  knownChains = IntMap.empty                        
  }
                  
updateWithNonPeriodicChain kchs new_chain = 
  let ts = zipWith (,) new_chain [(length new_chain),(length new_chain -1)..1]
  in let not_known_ts = takeWhile (\e->not $ IntMap.member (fst e) kchs) ts
     in IntMap.union kchs (IntMap.fromList not_known_ts)
   
computeChainState ch x = 
  let kchs = knownChains ch
      npch = reverse . chain . computeNonPeriodicChain $ x
  in ch {
    knownChains = updateWithNonPeriodicChain kchs npch
    }

main = do
  let p = foldl computeChainState emptyChains [1..(endNum-1)]
  let k = IntMap.foldr mappend mempty $ IntMap.map (\l->if l == 60 then Sum 1 else Sum 0) (knownChains p)
  print $ show (getSum k)
  
