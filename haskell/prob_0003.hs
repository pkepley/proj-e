{-
    Problem 3:
    Compute the largest prime factor of 600851475143

    Solved: 2021-02-24
-}

-- https://hackage.haskell.org/package/primes
import Data.Numbers.Primes (primeFactors)

solveProb :: Integer
solveProb = maximum (primeFactors 600851475143)

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 3: " ++ show solveProb
