{-
    Problem 10:
    Compute the sum of all primes below 2 million

    Solved: 2021-02-24
-}

-- https://hackage.haskell.org/package/primes
import Data.Numbers.Primes (primes)

solveProb =
  let maxN = 2 * 10^6
  in sum $ takeWhile (< maxN) primes

main = do
  putStrLn $ "Solution to Prob 11: " ++ show solveProb
