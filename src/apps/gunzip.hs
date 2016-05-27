import Data.Foldable (forM_)
import Control.Monad (when)
import Options.Applicative
import qualified Data.List as List
import System.FilePath (replaceExtension)

import qualified ViperVM.Format.Compression.GZip as GZip
import ViperVM.Format.Binary.Buffer
import qualified ViperVM.Format.Text as Text

main :: IO ()
main = do
   opts <- getOptions

   bs <- bufferReadFile (optpath opts)
   let ms = GZip.decompress bs
   forM_ ms $ \m -> do
      let fname = case (Text.unpack (GZip.memberName m), optpath opts) of
                     ("",p) | ".tgz" `List.isSuffixOf` p -> replaceExtension p ".tar"
                     (s,_)                          -> s
      putStrLn $ "File: " ++ fname
      when (fname /= "") $ do
         bufferWriteFile fname (GZip.memberContent m)
         


data Options = Options
   { optpath    :: String
   }

options :: Parser Options
options = Options
  <$> argument str (
        metavar "PATH"
     <> help "Path to gzipped file"
     )
  

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Unzip a gzip archive"
     <> header "GUnzip" )
