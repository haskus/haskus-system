import qualified Data.ByteString as BS
import ViperVM.Format.Compression.Algorithms.Huffman

import Text.Printf

main :: IO ()
main = do
   putStrLn "Enter the text to compress"
   xs <- getLine

   let 
      tree     = computeHuffmanTreeFromFoldable xs
      binTable = buildCodingTable binaryEncoder tree
   putStrLn "Coding:"
   putStrLn (show binTable)

   let 
      wbs = toBinary binTable xs
      r   = fromIntegral (length xs) / fromIntegral (BS.length wbs) :: Float

   putStrLn $ printf "Writing file (compression ratio: %.2f%%)" r
   BS.writeFile "out.huff" wbs

   putStrLn "Reading back:"
   bs <- BS.readFile "out.huff"
   putStrLn (fromBinaryLen True tree (length xs) bs)
