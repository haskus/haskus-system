module Main where

import Text.Printf

import Haskus.Format.Compression.Algorithms.Huffman
import Haskus.Binary.Buffer

main :: IO ()
main = do
   putStrLn "Enter the text to compress"
   xs <- getLine

   let 
      tree     = computeHuffmanTreeFromFoldable xs
      binTable = buildCodingTable binaryEncoder tree
   putStrLn "Coding:"
   print binTable

   let 
      wbs = toBinary binTable xs
      r   = fromIntegral (length xs) / fromIntegral (bufferSize wbs) :: Float

   putStrLn $ printf "Writing file (compression ratio: %.2f%%)" r
   bufferWriteFile "out.huff" wbs

   putStrLn "Reading back:"
   bs <- bufferReadFile "out.huff"
   putStrLn (fromBinaryLen True tree (length xs) bs)
