import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import qualified ViperVM.Format.Compression.GZip as GZip
import Data.Foldable (forM_)
import Control.Monad (when)

main :: IO ()
main = do
   args <- getArgs

   case args of
      [s] -> do
         bs <- BS.readFile s
         let ms = GZip.decompress bs
         forM_ ms $ \m -> do
            let fname = GZip.memberName m
            putStrLn $ "File: " ++ fname
            when (fname /= "") $ do
               BS.writeFile fname (GZip.memberContent m)
         
      _ -> do
         putStrLn "Invalid parameters"
