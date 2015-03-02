import qualified Data.ByteString.Lazy as BS
import ViperVM.Format.Compression.Algorithms.Huffman

import Text.Printf

main :: IO ()
main = do
   putStrLn "Enter the text to compress"
   xs <- getLine

   let tree = makeTree xs
   putStrLn "Coding:"
   putStrLn (show (buildCodingString tree))

   let 
      wbs = toBinary True tree xs
      r = fromIntegral (length xs) / fromIntegral (BS.length wbs) :: Float

   putStrLn $ printf "Writing file (compression ratio: %.2f%%)" r
   BS.writeFile "out.huff" wbs

   putStrLn "Reading back:"
   bs <- BS.readFile "out.huff"
   putStrLn (fromBinaryLen True tree (length xs) bs)
