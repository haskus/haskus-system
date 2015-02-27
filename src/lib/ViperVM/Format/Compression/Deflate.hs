{-# LANGUAGE MultiWayIf #-}

-- | Implement DEFLATE (de)compression algorithm
--
-- http://www.ietf.org/rfc/rfc1951.txt
module ViperVM.Format.Compression.Deflate
   ( makeCodes
   , getBlock
   )
where

import qualified Data.Map as Map
import Data.List (foldl')
import Data.Word
import Data.Bits (shiftL, xor, (.|.), (.&.), testBit)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import qualified Data.ByteString as BS


-- | Compute Huffman codes from a list of code lengths
--
-- Deflate algorithm uses Huffman coding with some additional rules:
--    For two symbols a and b:
--       1) if  codelength(a) == codelength(b) then
--             if code(a) < code(b) then
--                a < b
--             else
--                a > b
--
--       2) if codelength(a) < codelength(b) then
--             code(a) < code(b)
--
--    where:
--       * code(x) is the value of the coding of x
--       * codelength(x) is the length of the coding of x (i.e. the number of
--       bits)
--
-- These properties allow the Huffman encoding to be provided with only a
-- sequence of code lengths.
makeCodes :: [Word] -> [Word64]
makeCodes es = codes
   where
      -- number of codes for each length
      counts = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty es

      -- first code for each length
      initCodes = Map.fromList ([0..maximum es] `zip` cds)
         where
            cds = reverse $ foldl' f [] [0..maximum es]
            f [] _      = [0] -- first code is 0
            f (c:cs) sz = code:c:cs
               where
                  code = (c + Map.findWithDefault 0 (sz-1) counts) `shiftL` 1

      -- values of each code
      codes = reverse . fst $ foldl' f ([],initCodes) es
         where
            f (cs,cds) sz = (c:cs,cds')
               where
                  (Just c, cds') = Map.insertLookupWithKey (const (+)) sz 1 cds


-- 
-- Compressed data are split in blocks. Blocks are *not* byte aligned.
--
-- Header of a block:
--    1st bit: if set, indicate final block
--    2-3 bits: data compression
--       00 - No compression
--       01 - Compressed with fixed Huffman codes
--       10 - Compressed with dynamic Huffman codes
--       11 - Reserved (error)

data Compression
   = NoCompression
   | FixedHuffman
   | DynamicHuffman
   deriving (Show,Eq,Enum)

-- | Read block header and returns (isFinal,compression)
getBlockHeader :: BitGet (Bool,Compression)
getBlockHeader = block $ (,)
   <$> fmap (/=0)                   (word8 1)
   <*> fmap (toEnum . fromIntegral) (word8 2)

-- | Return a decompressed block
getBlock :: BitGet (BS.ByteString,Bool)
getBlock = do
   (isFinal,comp) <- getBlockHeader
   content <- case comp of

      -- Block without compression
      NoCompression -> do
         alignByte
         -- Two bytes: len and nlen
         -- * nlen is the one's complement of len
         -- * len is the number of raw bytes that follow
         (len,nlen) <- block $ (,) <$> word8 8 <*> word8 8
         when (len /= nlen `xor` 0xFF) $
            error "Invalid uncompressed block length"
         -- Read raw data
         getByteString (fromIntegral len)

      --FixedHuffman -> do
      --DynamicHuffman -> do

   return (content,isFinal)


-- | Return the encoded length
getLength :: Int -> BitGet Word16
getLength code = case code of
   x | x <= 260 -> return (fromIntegral code - 254)

   x | x <= 284 -> do
      -- In the RFC, the length is given in a table. I figured out a formula to
      -- compute it instead. It uses the formula to compute the sum of a
      -- geometric sequence (common ratio = 2).
      --
      --       4*(1-2^n)/(1-2) + r*(2^n) + e + 7
      --
      -- It simplifies to: (4+r)*2^n + e + 3
      --
      let 
         (n,r) = (code-261) `divMod` 4
         r'    = fromIntegral r
      e <- getWord16be n
      return ((4+r') * 2^n + e + 3)

   285 -> return 258
   _   -> error "Invalid length code"


-- | Return the encoded distance
getDistance :: Int -> BitGet Word32
getDistance code = case code of
   x | x <= 1 -> return (fromIntegral code + 1)

   _ -> do
      -- The magic formula is very similar to the one in 'getLength'
      let 
         (n,r) = (code-2) `divMod` 2
         r'    = fromIntegral r
      e <- getWord32be n
      return ((2+r') * 2^n + e + 1)


-- | Put the code
putFixedCode :: Int -> BitPut ()
putFixedCode code
   | code <= 143 = do -- A
         -- 8 bit code, starting from 00110000 to 10111111
         let c = fromIntegral code + 48
         putWord8 8 c

   | code <= 255 = do -- B
         -- 9 bit code, starting from 110010000 to 111111111
         let c = fromIntegral (code-144) + 400
         putWord16be 9 c

   | code <= 279 = do -- C
         -- 7 bit code, starting from 0000000 to 0010111
         let c = fromIntegral (code-256)
         putWord8 7 c

   | code <= 287 = do -- D
         -- 8 bit code, starting from 11000000 to 11000111
         let c = fromIntegral (code-280) + 192
         putWord8 8 c

   | otherwise = error "Invalid code"


-- | Get the code
getFixedCode :: BitGet Int
getFixedCode = do
   b <- getWord8 4

      -- D
   if | b  == 0xC -> do
            b2 <- getWord8 4
            let r = fromIntegral $ ((b `shiftL` 4) .|. b2)
            return (r - 192 + 280)

      -- B
      | b .&. 0xC == 0xC -> do
            b2 <- getWord16be 5
            let b' = fromIntegral b
            let r  = fromIntegral $ ((b' `shiftL` 5) .|. b2)
            return (r - 400 + 144)

      -- C
      | (b .&. 0xC == 0) && (testBit b 0 /= testBit b 1) -> do
            b2 <- getWord8 3
            let r = fromIntegral $ ((b `shiftL` 3) .|. b2)
            return (r + 256)

      -- A
      | otherwise -> do
            b2 <- getWord8 4
            let r = fromIntegral $ ((b `shiftL` 4) .|. b2)
            return (r - 48)
