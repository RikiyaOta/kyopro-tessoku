import qualified Data.ByteString.Char8 as BS

bitNum :: Char -> Int -> Int
bitNum '0' order = 0
bitNum '1' order = 2^order

toDecimal :: String -> Int -> Int
toDecimal "" acc = acc
toDecimal (x:xs) acc = toDecimal xs acc + bitNum x (length xs)

main = do
    binaryStr <- BS.unpack <$> BS.getLine
    putStr $ show $ toDecimal binaryStr 0