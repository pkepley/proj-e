{-
    Problem 9:
    Find the unique Pythagorean triple satisfying a + b + c = 1000

    Solved: 2021-02-23
-}


solveProb :: Integer
solveProb =
    head  [a * b * c | a <- [1..1000], b <- [1..(1000 - a)],
                     let c = 1000 - a - b, a*a + b*b == c*c]

main :: IO ()
main = do
    putStrLn $ "Solution to Prob 9: " ++ show solveProb
