{-
    Problem 10:
    Compute the sum of all primes below 2 million

    Solved: 2021-02-24
-}

-- https://hackage.haskell.org/package/primes
import Data.Numbers.Primes (primes)

solveProb :: Integer
solveProb =
    sum $ takeWhile (< maxN) primes
  where
    maxN = 2 * 10^6

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 11: " ++ show solveProb
