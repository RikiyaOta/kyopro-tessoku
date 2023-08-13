import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Array.Unboxed as UArray
 
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words
getInt :: IO Int
getInt = readInt <$> BS.getLine
getInts :: IO [Int]
getInts = readIntList <$> BS.getLine


main = do
    n <- getInt
    lotteryResultList <- getInts -- [0 or 1]
    q <- getInt
    let acc = UArray.listArray (0, n) $ scanl (+) 0 lotteryResultList :: UArray.Array Int Int
    replicateM_ q $ do
        [l, r] <- getInts
        let total = r - l + 1
            winCounts = (acc UArray.! r) - (acc UArray.! (l-1))
            loseCounts = total - winCounts
            in putStrLn $ case compare winCounts loseCounts of
                LT -> "lose"
                EQ -> "draw"
                GT -> "win"