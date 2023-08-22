import Data.Maybe
import qualified Data.ByteString.Char8 as BS
 
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine

search :: Int -> Int -> [Int] -> Int
search target n [] = -1
search target n (x:xs) = if target == x then n - length xs else search target n xs

main = do
    [n, x] <- getInts
    xs <- getInts
    print $ search x n xs
