{-
   Problem 8:
   Given a 1000 digit string of integers, find the 13 consecutive
   integers with largest product.

   The 1000 digit string is stored in ../data/prob_0008.txt

   Solved: 2021-02-23
-}

import Data.Char (digitToInt)
import Data.List (tails)

-- Modified this: https://stackoverflow.com/a/24410352
rollingProd :: Num a => Int -> [a] -> [a]
rollingProd n xs
    = map product                          -- multiplicative roll-up
    . zipWith (flip const) (drop (n-1) xs) -- drop the last n-1
    . map (take n)                         -- truncate long windows
    . tails                                -- make arbitrarily long windows
    $ xs

getInpt :: IO [Int]
getInpt = do
    src <- readFile "../data/prob_0008.txt"
    let digits = concatMap (map digitToInt) (lines src)
    return digits

solveProb :: IO Int
solveProb = do
    inpt <- getInpt
    let mx = maximum $ rollingProd 13 inpt
    return mx

main :: IO ()
main = do
    rslt <- solveProb
    putStrLn $ "Solution to Prob 8: " ++ show rslt
