module Main where

import Options.Applicative
import System.FilePath (replaceExtension)

import Haskus.Utils.List (isSuffixOf)
import Haskus.Utils.Flow (forM_,when)
import qualified Haskus.Format.Compression.GZip as GZip
import Haskus.Binary.Buffer
import qualified Haskus.Utils.Text as Text

main :: IO ()
main = do
   opts <- getOptions

   bs <- bufferReadFile (optpath opts)
   let ms = GZip.decompress bs
   forM_ ms $ \m -> do
      let fname = case (Text.unpack (GZip.memberName m), optpath opts) of
                     ("",p) | ".tgz" `isSuffixOf` p -> replaceExtension p ".tar"
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
