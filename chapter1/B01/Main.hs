main = do
    [a, b] <- map read . words <$> getLine
    putStrLn $ show $ a + b