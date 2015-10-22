-- | Variable length encodings
module ViperVM.Format.Binary.VariableLength
   ( getULEB128
   , putULEB128
   , getSLEB128
   , putSLEB128
   , getLEB128BS
   )
where

import Data.Int
import Data.Word
import Data.Bits
import Data.ByteString (ByteString)

import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.BitPut
import ViperVM.Format.Binary.BitOrder

-- Unsigned Little Endian Base 128 (ULEB128)
-- The word is splitted in chunks of 7 bits, starting from least significant
-- bits. Each chunk is put in a Word8. The highest bit indicates if there is a
-- following byte (0 false, 1 true)

-- | Get an unsigned word in Little Endian Base 128
getULEB128 :: (Integral a, Bits a) => Get a
getULEB128 = do
   a <- getWord8
   let w = fromIntegral (a .&. 0x7f)
   if not (testBit a 7)
      then return w
      else do
         b <- getULEB128
         return $ (b `shiftL` 7) .|. w

-- | Put an unsigned word in Little Endian Base 128
putULEB128 :: (Integral a, Bits a) => a -> Put
putULEB128 = rec True
   where
      rec first x = case (first,x) of
         (True,0)  -> putWord8 0
         (False,0) -> return ()
         _         -> do
            let 
               r = x `shiftR` 7
               w = x .&. 0x7f
               w' = if r == 0 then w else setBit w 7
            putWord8 (fromIntegral w')
            rec False r

-- | Get a signed int in Little Endian Base 128
getSLEB128 :: (Integral a, Bits a) => Get a
getSLEB128 = do
   let toInt8 :: Word8 -> Int8
       toInt8 = fromIntegral
   a <- getWord8
   if not (testBit a 7)
      then return . fromIntegral . toInt8 $ (a .&. 0x7f) .|. ((a .&. 0x40) `shiftL` 1)
      else do
         b <- getSLEB128
         return $ (b `shiftL` 7) .|. (fromIntegral (a .&. 0x7f))

-- | Put a signed int in Little Endian Base 128
putSLEB128 :: (Integral a, Bits a) => a -> Put
putSLEB128 a = rec a
   where
      ext = if a >= 0 then 0 else complement 0
      rec x =  do
         let 
            r = x `shiftR` 7
            w = x .&. 0x7f
         if r /= ext
            then do
               putWord8 (fromIntegral w .|. 0x80)
               rec r
            else if (testBit w 6 && a < 0) || (not (testBit w 6) && a >= 0)
               then putWord8 (fromIntegral w)   -- no need for sign byte
               else do
                  putWord8 (fromIntegral w .|. 0x80)
                  putWord8 (fromIntegral ext .&. 0x7f)   -- sign byte


-- | Get a bytestring containing a decoded LEB128 string
getLEB128BS :: BitOrder -> Get ByteString
getLEB128BS bo = rec (newBitPutState bo)
   where
      rec state = do
         w      <- getWord8
         let state2 = putBits 7 w state
         case testBit w 7 of
            True  -> rec state2
            False -> return (getBitPutBS state2)

