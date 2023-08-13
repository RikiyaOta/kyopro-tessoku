judge :: [Int] -> IO ()
judge [] = putStrLn "No"
judge _ = putStrLn "Yes"

main = do
    [n, x] <- map read . words <$> getLine :: IO [Int]
    numList <- map read . words <$> getLine :: IO [Int]
    judge $ filter (== x) numList