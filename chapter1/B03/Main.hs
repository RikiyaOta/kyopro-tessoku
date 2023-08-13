import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Maybe

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
  priceList <- getInts
  let priceListWithIndex = zip [1 .. n] priceList :: [(Int, Int)]
      result = [True | (i, price1) <- priceListWithIndex, (j, price2) <- priceListWithIndex, i < j, (k, price3) <- priceListWithIndex, j < k, price1 + price2 + price3 == 1000]
   in if result /= [] then putStrLn "Yes" else putStrLn "No"