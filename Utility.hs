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
getIntNLists :: Int -> IO [[Int]]
getIntNLists n = fmap readIntList <$> replicateM n BS.getLine 
getIntMatrix :: IO [[Int]]
getIntMatrix = fmap readIntList . BS.lines <$> BS.getContents
getCharNLists :: Int -> IO [[Char]]
getCharNLists n = fmap BS.unpack <$> replicateM n BS.getLine