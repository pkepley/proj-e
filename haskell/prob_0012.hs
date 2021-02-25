{-
    Problem 12:
    Find the first triangular number with d(T_N) > 500

    Solved: 2014-08-21
-}

-- https://hackage.haskell.org/package/primes
import Data.Numbers.Primes (primeFactors)
import Data.List (nub)

count x = length . filter (x==)

factorization n =
  [(p, count p ps) | let ps = primeFactors n, p <- nub ps]

nDivisors n =
  product [e + 1 | (p, e) <- factorization n]

solveProb =
  head $ [m | n <- [1..], let m = n * (n+1) `div` 2, nDivisors m > 500]

main = do
  putStrLn $ "Solution to Prob 12: " ++ show solveProb
