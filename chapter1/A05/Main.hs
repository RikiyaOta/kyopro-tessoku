import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
 
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine

main = do
    [n, k] <- getInts
    let baseList = [1..n]
        combinations = [(n1,n2) | n1 <- baseList, n2 <- baseList]
        -- POINT: 3重ループをしなくても、2つ数字が決まれば確定するので、O(N^2)まで落とせる。
        accumulator acc (n1, n2) = acc + (if 1 <= k - n1 - n2 && k - n1 - n2 <= n then 1 else 0)
        in print $ foldl accumulator 0 combinations
