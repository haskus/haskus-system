import qualified Data.ByteString.Lazy as BS
import qualified ViperVM.Format.Compression.GZip as GZip
import Data.Foldable (forM_)
import Control.Monad (when)
import Options.Applicative
import qualified Data.List as List
import System.FilePath (replaceExtension)

main :: IO ()
main = do
   opts <- getOptions

   bs <- BS.readFile (optpath opts)
   let ms = GZip.decompress bs
   forM_ ms $ \m -> do
      let fname = case (GZip.memberName m, optpath opts) of
                     ("",p) | ".tgz" `List.isSuffixOf` p -> replaceExtension p ".tar"
                     (s,_)                          -> s
      putStrLn $ "File: " ++ fname
      when (fname /= "") $ do
         BS.writeFile fname (GZip.memberContent m)
         


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
