{-
   Problem 5:
   2520 is the smallest number that can be divided by each of the numbers
   from 1 to 10 without any remainder.

   What is the smallest positive number that is evenly divisible by all
   of the numbers from 1 to 20?

   Solved: 2021-02-23
-}


listLCM :: [Integer] -> Integer
listLCM = foldl lcm 1

solveProb :: Integer
solveProb = listLCM [1..20]

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 5: " ++ show solveProb
