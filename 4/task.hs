
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.MD5 as MD5

s2bs :: String -> BS.ByteString
s2bs = BSU.fromString

int2bs :: Int -> BS.ByteString
int2bs = BSU.fromString . show

findLowestIntcode :: String -> (BS.ByteString, Int)
findLowestIntcode prefix =
  head $ filter ((== s2bs prefix) .
                 BS.take (length prefix) . fst)
  [(B16.encode (MD5.hash (BS.concat [s2bs "ckczppom", int2bs x])), x)
  | x <- [0..]]

main :: IO ()
main = do
  print $ findLowestIntcode "00000"
  print $ findLowestIntcode "000000"
