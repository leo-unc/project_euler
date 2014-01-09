module Main (main,pentagonalNumber,isPentagonal,testPentagonal,runTestFor) where

pentagonalNumber n = div (n*(3*n - 1)) 2
                                
isPentagonal x = (snd . properFraction $ (sqrt (24 * (fromIntegral x) + 1) + 1 ) / 6 ) == 0
                     

testPentagonal n i = (isPentagonal (n + i) && isPentagonal (n - i), n - i)

runTestFor k = let n = pentagonalNumber k 
               in  filter fst . map (testPentagonal n) . map pentagonalNumber $ [k-1,k-2..1] 

main = do
  print . head . (concatMap runTestFor) $ [2..]