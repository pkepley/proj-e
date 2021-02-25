{-
    Problem 4:
    Find the largest palindromic number that is a product
    of two three digit numbers.

    Solved: 2021-02-23
-}

import Data.Char
import Data.List

isPalindrome :: Integer -> Bool
isPalindrome n =
    let nStr = show n in (nStr == reverse nStr)

solveProb :: Integer
solveProb =
    maximum [mn | n <- [999, 998..100],
                  m <- [n-1, n-2..100],
                  let mn = m*n, isPalindrome mn]

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 4: " ++ show solveProb
