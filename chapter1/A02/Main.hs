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
    [n, x] <- getInts
    numList <- getInts
    if x `elem` numList then putStrLn "Yes" else putStrLn "No"