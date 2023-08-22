import Data.Maybe
import qualified Data.ByteString.Char8 as BS
 
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine

binarySearch :: Int -> Int -> [Int] -> Int
binarySearch target offset [] = offset
binarySearch target offset [x] = if target == x then offset + 1 else -1
binarySearch target offset xs =
    let (left, right) = splitAt (length xs `div` 2) xs
        in case compare target (head right) of
            EQ -> offset + length left + 1
            LT -> binarySearch target offset left
            GT -> binarySearch target (offset + length left) right

main = do
    [n, x] <- getInts
    xs <- getInts
    print $ binarySearch x 0 xs
