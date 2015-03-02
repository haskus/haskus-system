import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import qualified ViperVM.Format.Compression.GZip as GZip
import Data.Foldable (forM_)

main :: IO ()
main = do
   args <- getArgs

   case args of
      [s] -> do
         bs <- BS.readFile s
         let ms = GZip.decompress bs
         forM_ ms $ \m -> do
            putStrLn $ "File: " ++ GZip.memberName m
         
      _ -> do
         putStrLn "Invalid parameters"
