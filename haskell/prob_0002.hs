{-
    Problem 2:
    Find the sum of the terms in the Fibonacci sequence (F_n)
    that  satisfy
        1) F_n < 4 * 10^6
        2) F_n is even

   Solved: 2021-02-24
-}

-- https://wiki.haskell.org/The_Fibonacci_sequence
fibs = scanl (+) 0 (1:fibs)

solveProb =
  let maxFib = 4 * 10^6
  in sum [n | n <- takeWhile (< maxFib) fibs, even n]

main = do
  putStrLn $ "Solution to Prob 2: " ++ show solveProb
