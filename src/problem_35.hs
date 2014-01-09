module Main(main) where

import qualified Math.NumberTheory.Primes.Sieve as Sieve
import Data.Maybe
import Data.List(nub)
import qualified Data.Set as DS

data Rotation = Rotation { getNumber :: Integer
                         , getStringRepresentation::String
                         } deriving (Eq, Show)
                                    
createRotationFromString x = let z = ((read x)::Integer)
                             in createRotation z
                                
createRotation x = Rotation x (show x)

tentativeNumberOfRotations = length . getStringRepresentation

nextRotation Nothing = Nothing
nextRotation (Just x) = if getNumber x >= 0 && getNumber x <=9 
                        then Just x
                        else let s = getStringRepresentation x
                             in let s' = tail s ++ [head s]
                                in let x' = createRotationFromString s'
                                   in if tentativeNumberOfRotations x' == tentativeNumberOfRotations x
                                      then Just x'
                                      else Nothing

generateRotations a = let r = createRotation a
                      in let l = take (tentativeNumberOfRotations r) $ iterate nextRotation $ Just r
                         in if any isNothing l
                            then []
                            else nub $ map (getNumber . fromJust) l
  
removeRotationsFromSet prime_set a = 
  let rs = generateRotations a
  in if null rs
     then ([], DS.delete a prime_set)
     else let d = DS.intersection prime_set $ DS.fromList rs
          in if DS.size d == length rs
             then (rs, DS.difference prime_set $ DS.fromList rs)
             else ([], DS.delete a prime_set)

-- primes, set of prime numbers, result is list of circular primes
examineRotations prime_set = 
  let mk = DS.minView prime_set
  in case mk of
    Nothing -> []
    Just (a, prime_set') -> let (rotations_of_a, s) = removeRotationsFromSet prime_set a 
          in rotations_of_a ++ examineRotations s
      
main = do
  print . length . examineRotations . DS.fromList . takeWhile (<1000000) $ Sieve.primes