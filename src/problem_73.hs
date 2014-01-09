module Main where

import Data.Ord (comparing)
import Data.Ratio
import Data.List(minimumBy)

mergeLists xs ys =
  mergeLists' 1 0 xs ys
  where mergeLists' 1 0 (x:xs) ys = x:(mergeLists' 0 1 xs ys)
        mergeLists' 0 1 xs (y:ys) = y:(mergeLists' 1 0 xs ys)
    
isNormalForm a b = 
  (a > 0) && (b > 0) && (a >= b)
  
augmentBase (p,q) a b = 
  let xs = [p + b * i | i <- (mergeLists [0..] [-1,-2..])]
      ys = [q - a * i | i <- (mergeLists [0..] [-1,-2..])]
  in zip xs ys

flipPair (a,b) = (b,a)

negateFirst (a,b) = (-a,b)

negateSecond (a,b) = (a,-b)


-- Solves linear diophantine equation of form
-- Does not check whether the solution exists.
-- a x + b y = c
solveDiophantine :: Integer -> Integer -> Integer -> [(Integer,Integer)]
solveDiophantine a b c  
  | isNormalForm a b = solveDiophantine' a b c
  | a < 0 = map negateFirst $ solveDiophantine (-a) b c
  | b < 0 = map negateSecond $ solveDiophantine a (-b) c
  | a < b = map flipPair $ solveDiophantine b a c
  | otherwise = undefined
  where solveDiophantine' a b c = 
          let (x,y) = solveSimpleDiophantine a b
          in augmentBase (c*x, c*y) a b

-- Solves base diophantine equation 
-- a x + b y = 1 where a > 0 and b > 0 and a > b
solveSimpleDiophantine a 1 = (0, 1)
solveSimpleDiophantine a b = (y, x - k * y)
  where k = a `div` b
        (x, y) = solveSimpleDiophantine b (a `rem` b)
  
-- Represents step of enumerating reduced proper fractions
data Step = Step {
  a :: Integer,
  b :: Integer,
  c :: Integer,
  d :: Integer,
  count :: Integer
  } deriving (Show)
            
-- StartStep for enumerating proper fractions.
startStep = Step {
  a = 0,
  b = 1,
  c = 1,
  d = maxDenom,
  count = 2    
}

fractionFromStep s = (c s, d s)

filterSolutions m = 
  filter (\t -> numerator t >=1 && denominator t >= 1 && numerator t <= denominator t && denominator t <= maxDenom) . takeWhile (\t -> abs(denominator t) <= maxDenom) . filter (>m) . map (\(x,y)->(x % y))
                                                                   
                                                                   
findApplicableSolution c d ss = 
  let good_solutions = filterSolutions (c % d)  ss
  in minimumBy (comparing (\t-> abs(t - c % d))) good_solutions
  
solveWithDiophantine' a b c d =
  solveDiophantine d (-c) (-d * a + c * b)

nextProperFraction Step{a=0,b=1,c=1,d=d,count=cs} = 
  Step {
    a = 1, b = d, c = 1, d=d-1, count=cs+1
    } 
      
nextProperFraction s1 = 
  s1 {
    a = c s1,
    b = d s1,
    c = numerator f,
    d = denominator f,
    count = 1 + count s1
  }   
  where f = findApplicableSolution (c s1) (d s1) sol
        sol = solveWithDiophantine' (a s1) (b s1) (c s1) (d s1)

maxDenom = 12000

problem_72_start_step = 
  Step {
    a = 3999, 
    b = 11998, 
    c = 1,
    d = 3,
    count = 1
  }
  
main = do
  -- problem_71
  let r = iterate nextProperFraction problem_72_start_step
  let k = dropWhile (\t->c t % d t < (1 % 2)) r
  print $ count (head k) - 2