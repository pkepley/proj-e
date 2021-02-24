{-
    Prob 13:

    Work out the first ten digits of the sum of the following
    one-hundred 50-digit numbers.

    Solved: 2021-02-23
-}

getInpt = do
    src <- readFile "../data/prob_0013.txt"
    let ns = map read (lines src) :: [Integer]
    return ns

solveProb = do
    ns <- getInpt
    let first10sumNs = read . take 10 $ show $ sum ns :: Integer
    return first10sumNs

main = do
    rslt <- solveProb
    putStrLn $ "Solution to Prob 13: " ++ show rslt
