import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as BS
 
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInt :: IO Int
getInt = readInt <$> BS.getLine
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine

-- binary search like に解く
-- やっぱり List だとスピード出ない。Array によるインデックスアクセスにして試してみたい。
-- 機能的には上手く動いてくれています。
binarySearch' :: Int -> Int -> [Int] -> Int
binarySearch' target acc [] = acc
binarySearch' target acc [x] = if x < target then acc + 1 else acc
binarySearch' target acc xs =
    let (left, right) = splitAt (length xs `div` 2) xs
        in case compare target (head right) of
            EQ -> acc + length left
            LT -> binarySearch' target acc left
            GT -> binarySearch' target (acc + length left) right

main = do
    n <- getInt
    xs <- getInts
    q <- getInt
    let xs' = sort xs
        in replicateM_ q $ do
            x <- getInt
            print $ binarySearch' x 0 xs'