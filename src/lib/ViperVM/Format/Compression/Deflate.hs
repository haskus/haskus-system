{-# LANGUAGE MultiWayIf, LambdaCase #-}

-- | Implement DEFLATE (de)compression algorithm
--
-- http://www.ietf.org/rfc/rfc1951.txt
module ViperVM.Format.Compression.Deflate
   ( decompress
   )
where

import Data.List (sortBy)
import Data.Word
import Data.Maybe (fromJust)
import Data.Bits (shiftL, xor, (.|.), (.&.), testBit)
import Control.Monad (when,replicateM)
import Control.Applicative ((<$>), (<*>))
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Sequence ((><), Seq, (|>))
import Data.Foldable (toList)
import Data.Ord(comparing)

import ViperVM.Format.Compression.Huffman

-- 
-- Compressed data are split in blocks. Blocks are *not* byte aligned.
--

-- | Decompress all blocks
decompress :: BitGet (Seq Word8)
decompress = rec Seq.empty
   where
      rec s = getBlock s >>= \case
         (s',True)  -> return s' -- Final block
         (s',False) -> rec s'



-- | Decompress a block and indicate if it is the last one
--
-- Blocks may require data from previous blocks to be decompressed, hence you
-- must pass these data as the first parameter.
getBlock :: Seq Word8 -> BitGet (Seq Word8,Bool)
getBlock s = do
   (isFinal,comp) <- getBlockHeader

   content <- case comp of
      NoCompression  -> (s ><) <$> getRawBlock
      FixedHuffman   -> decodeBlock getFixedToken s
      DynamicHuffman -> do
         g <- getDynamicToken
         decodeBlock g s

   return (content,isFinal)



-- | Block compression method
data Compression
   = NoCompression
   | FixedHuffman
   | DynamicHuffman
   deriving (Show,Eq,Enum)


-- | Read block header and returns (isFinal,compression)
--
-- If compression type == 3 (not supposed to happen), then fromEnum will fail
getBlockHeader :: BitGet (Bool,Compression)
getBlockHeader = block $ (,)
   <$> fmap (/=0)                   (word8 1)
   <*> fmap (toEnum . fromIntegral) (word8 2)


-- | Read an uncompressed block
getRawBlock :: BitGet (Seq Word8)
getRawBlock = do
   -- align on the next byte boundary
   alignByte
   -- Two bytes: len and nlen
   -- * nlen is the one's complement of len
   -- * len is the number of raw bytes that follow
   (len,nlen) <- block $ (,) <$> word8 8 <*> word8 8
   when (len /= nlen `xor` 0xFF) $
      error "Invalid uncompressed block length"
   -- Read raw data
   bs <- getByteString (fromIntegral len)
   return (Seq.fromList (BS.unpack bs))

-- | A block is a sequence of tokens
data Token a
   = Literal a       -- ^ Literal value (will be copied as-is)
   | EndOfBlock      -- ^ End of block marker
   | Copy Int Int    -- ^ Copy dist len: move backwards dist and copy len bytes
   deriving (Show,Eq)

-- | Decode a block using the getToken method provided
decodeBlock :: BitGet (Token Word8) -> Seq Word8 -> BitGet (Seq Word8)
decodeBlock getToken s = getToken >>= \case
   EndOfBlock    -> return s
   Literal x     -> decodeBlock getToken (s |> x)
   Copy dist len -> decodeBlock getToken (s >< w)
      where
         pre = Seq.length s - fromIntegral dist
         ss = cycle (drop pre (toList s))
         w  = Seq.fromList (take len ss) 

-- | Create a getToken getter
makeGetToken :: BitGet Int -> BitGet Int -> BitGet (Token Word8)
makeGetToken getCode getDist = do
   code <- getCode
   case code of
      256            -> return EndOfBlock
      _ | code < 256 -> return (Literal (fromIntegral code))
      _ -> do
         len  <- getFixedLength code
         dist <- getDist
         return (Copy dist len)

-- | Return the next token with fixed Huffman compression
getFixedToken :: BitGet (Token Word8)
getFixedToken = makeGetToken getFixedCode getFixedDistance


-- | Create the getToken method with dynamic Huffman compression
--
-- It reads tables at the beginning of the block that contain the Huffman codes
-- to use to decode tokens.
getDynamicToken :: BitGet (BitGet (Token Word8))
getDynamicToken = do
   (lits,dist) <- getTables
   let
      getCode = makeBitGetFromCodes ([0..285] `zip` lits)
      getDist = makeBitGetFromCodes ([0..29] `zip` dist)
   return $ makeGetToken getCode getDist

-- | Read tables for dynamic Huffman compression
--
-- Tables are encoded using a Run-Length Encoding. The RLE tokens are encoded
-- using a dynamic Huffman compression. Hence, we first read the table to
-- decode RLE tokens and build a "table decoder" BitGet instance.
getTables :: BitGet ([Word8],[Word8])
getTables = do
   -- Get the number of entries in each table
   nlits  <- (+257) <$> getWord16be 5  -- # of literal/length codes [257..286]
   ndist  <- (+1)   <$> getWord8 5     -- # of distance codes [1..32]
   nclen  <- (+4)   <$> getWord8 4     -- # of RLE code lengths [4..19]
   -- Get the table decoder
   getTable <- getTableDecoder (fromIntegral nclen)
   -- Decode literal/length table
   lits <- getTable (fromIntegral nlits)
   -- Decode distance table
   dist <- getTable (fromIntegral ndist)
   return (lits,dist)


-- | Run-length encoding
data RLE
   = Value Word8        -- ^ A value [1..15]
   | Repeat2bits        -- ^ 2 extra bits indicating repetition (3-6 times)
   | Repeat3bits        -- ^ 3 extra bits indicating repetition (3-10 times)
   | Repeat7bits        -- ^ 7 extra bits indicating repetition (11-138 times)
   deriving (Show,Eq,Ord)

-- | Read the table to build the RLE decoder. Return a RLE-encoded table decoder.
getTableDecoder :: Int -> BitGet (Int -> BitGet [Word8])
getTableDecoder nclen = do
   -- Get 3-bit lengths
   clens <- replicateM nclen (getWord8 3)
   -- The first RLE tokens are the most used ones. The last ones may be missing
   -- in the table. This way, instead of wasting 3 bits to indicate that a RLE
   -- token is not used in the block, the nclen can be shortened to remove the
   -- trailing codes
   let 
      cl = [Repeat2bits, Repeat3bits, Repeat7bits, Value 0,
            Value 8, Value 7,  Value 9, Value 6,  Value 10,
            Value 5, Value 11, Value 4, Value 12, Value 3,
            Value 3, Value 13, Value 2, Value 14, Value 1,
            Value 15]
      
      g = makeBitGetFromCodes (cl `zip` clens)

   return (makeRLEDecoder g)

-- | Create a decoder for RLE-encoded tables
--
-- The table decoder uses the Huffman tree to decode the RLE code.
-- Then it decodes the RLE code and returns the decoded values.
makeRLEDecoder :: BitGet RLE -> Int -> BitGet [Word8]
makeRLEDecoder get = rec []
   where
      rec xs 0 = return (reverse xs)
      rec xs n = get >>= \case
         Value x     -> rec (x:xs) (n-1)
         Repeat2bits -> do
            rep <- fromIntegral . (+3) <$> getWord8 2
            rec (replicate rep (head xs) ++ xs) (n-1)
         Repeat3bits -> do
            rep <- fromIntegral . (+3) <$> getWord8 3
            rec (replicate rep (head xs) ++ xs) (n-1)
         Repeat7bits -> do
            rep <- fromIntegral . (+11) <$> getWord8 7
            rec (replicate rep (head xs) ++ xs) (n-1)



-- | Read the length for the Copy token with the fixed Huffman compression
getFixedLength :: Int -> BitGet Int
getFixedLength code = case code of
   x | x <= 260 -> return (fromIntegral code - 254)

   -- In the RFC, the length is given in a table. I figured out a formula to
   -- compute it instead. It uses the formula to compute the sum of a
   -- geometric sequence (common ratio = 2).
   --
   --       4*(1-2^n)/(1-2) + r*(2^n) + e + 7
   --
   -- It simplifies to: (4+r)*2^n + e + 3
   x | x <= 284 -> f <$> getWord16be n
         where 
            (n,r) = (code-261) `divMod` 4
            r'    = fromIntegral r
            f e   = fromIntegral $ (4+r') * 2^n + e + 3

   285 -> return 258
   _   -> error "Invalid length code"


-- | Read the distance for the Copy token with the fixed Huffman compression
getFixedDistance :: BitGet Int
getFixedDistance = getWord8 5 >>= \case
   x | x <= 1 -> return (fromIntegral x + 1)

      -- The magic formula is very similar to the one in 'getFixedLength'
   x -> f <$> getWord32be n
         where 
            (n,r) = (fromIntegral x-2) `divMod` 2
            r'    = fromIntegral r
            f e   = fromIntegral $ (2+r') * 2^n + e + 1


-- | Put the token code with the fixed Huffman compression
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


-- | Get the token code with the fixed Huffman compression
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

-- | Compute Huffman codes from a list of code lengths with given (unchecked)
-- properties.
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
-- sequence of code lengths. A null code length indicates an element that
-- cannot be encoded.
makeHuffmanCodes :: (Ord b,Num b) => [(a,b)] -> [(Code,a)]
makeHuffmanCodes = rec emptyCode [] . msort
   where
      -- Sort by code length, then by index in the stream
      csort ((_,b),i) = (b,i)
      msort = fmap fst . sortBy (comparing csort) . (`zip` ([0..] :: [Int]))

      -- Encode each symbol, recursively
      rec _ xs [] = xs
      rec c xs ys@((v,l):ls)
         -- Skip symbols with length == 0
         | l == 0    = rec c ((emptyCode,v):xs) ls
         -- Assign current code if the code length matches
         | l == cl   = rec (codeAdd 1 c) ((c,v):xs) ls
         -- Otherwise, increase the current code length, prefixed with the
         -- current code
         | l < cl    = rec (codeShiftL 1 c) xs ys
         -- Shouldn't occur, except for negative code lengths...
         | otherwise = error "Invalid length"
         where
            cl = fromIntegral (codeLength c)

-- | Create a Huffman code getter from a list of codes
makeBitGetFromCodes :: (Ord b, Num b) => [(a,b)] -> BitGet a
makeBitGetFromCodes = fmap fromJust . makeBitGet True . buildTreeFromCodes . makeHuffmanCodes
