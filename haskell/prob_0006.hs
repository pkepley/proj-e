{-
    Problem 6:

    Find the difference between the sum of the squares of the first
    one hundred natural numbers and the square of the sum.

    Solved: 2021-02-23
-}


solveProb =  
  let n = 100
      sumOfSquares = sum $ map (^ 2) [1..n]
      squareOfSum = sum [1..n] ^ 2
  in squareOfSum - sumOfSquares

main = do
    putStrLn $ "Solution to Prob 6: " ++ show solveProb
