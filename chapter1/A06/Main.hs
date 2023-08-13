import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Array.Unboxed as UArray

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine

main = do
    [n, q] <- getInts
    visitorCounts <- getInts
    let acc = UArray.listArray (0,n) $ scanl (+) 0 visitorCounts :: UArray.Array Int Int
    replicateM_ q $ do
        [l, r] <- getInts
        print $ (acc UArray.! r) - (acc UArray.! (l-1))