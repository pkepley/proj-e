{-
   problem 7:
   What is the 10,0001st prime number?

   Solved: 2021-02-24
-}


-- https://hackage.haskell.org/package/primes
import Data.Numbers.Primes (primes)

-- zero based indexing, subtract 1!
solveProb :: Integer
solveProb = primes !! (10001 - 1)

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 7: " ++ show solveProb
