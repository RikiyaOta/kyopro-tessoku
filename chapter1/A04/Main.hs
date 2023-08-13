import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt

getInt :: IO Int
getInt = readInt <$> BS.getLine

toBinaryStr :: Int -> Int -> String -> String
toBinaryStr 0 0 acc = acc ++ "0"
toBinaryStr 1 0 acc = acc ++ "1"
toBinaryStr n order acc
    | n >= base = toBinaryStr (n - base) (order - 1) (acc ++ "1")
    | otherwise = toBinaryStr n (order - 1) (acc ++ "0")
    where base = 2^order

{-
 注意：N は 1 以上 1000 以下の整数である。
 注意：2^9 = 512, 2^10 = 1024
-}

maxOrder = 9

main = do
    n <- getInt
    putStrLn $ toBinaryStr n maxOrder ""