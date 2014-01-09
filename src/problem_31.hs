module Main(main) where 

-- An arrangement is a collection of coins of different denominations.
kAllDenominations = [1, 2, 5, 10, 20, 50, 100, 200]

findArrangements _ [] = []

findArrangements val (d:ds) =
  let counts = [0..(div val $ d)]
  in  concat . map (findArrangements' val d ds) $ counts
  where findArrangements' val d ds c = 
          let newval = val - d*c
          in if newval == 0
             then [[(d, c)]]
             else map ((d, c) :) (findArrangements (val - d*c) ds)
          
findArrangementCount _ [] = 0                  

findArrangementCount val (d:ds) =
  let k = (div val d)
  in if k == 0
     then 0
     else  sum $ map (findArrangements' val d ds) [0..k]
          where findArrangements' val d ds c = 
                  let newval = val - d*c
                  in if newval == 0
                     then 1
                     else (findArrangementCount newval ds)

          
main = do
  print $ findArrangementCount 200 [1, 2, 5, 10, 20, 50, 100, 200]