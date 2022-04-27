{-# LANGUAGE BangPatterns #-}

module Haskus.Arch.X86_64.ISA.Encoder
  ( EncodedInsn
  , emptyInsn
  , getSize
  , putByte
  , showBytes
  )
where

import Data.Word
import Data.Bits

-- | Encoded instruction
--
-- Instructions are 15-byte long at most, so we can store them in two 64-bit
-- words. We use one byte to store the actual size in bytes.
--
-- Structure:
--  x7 x6 x5 x4 x3 x2 x1 x0
--  sz xE xD xC xB xA x9 x8
--
-- sz is the instruction size in bytes
--
-- On little-endian architectures, we can write the bytes directly with some
-- masks depending on the size.
--
data EncodedInsn = EncodedInsn
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word64

emptyInsn :: EncodedInsn
emptyInsn = EncodedInsn 0 0

-- | Size of the instruction in bytes
getSize :: EncodedInsn -> Word8
getSize (EncodedInsn _ b) = fromIntegral (b `unsafeShiftR` 56)

-- | Set the instruction size field
setSize :: Word8 -> EncodedInsn -> EncodedInsn
setSize w (EncodedInsn a b) = EncodedInsn a ((b .&. 0x00FFFFFFFFFFFFFFFF)
                                             .|. (fromIntegral w `unsafeShiftL` 56))

-- | Put a byte. Unset bytes are assumed to be 0
putByte :: Word8 -> EncodedInsn -> EncodedInsn
putByte w i@(EncodedInsn a b) =
  let !w' = fromIntegral w
  in case getSize i of
      0  -> setSize 1  (EncodedInsn w' 0)
      1  -> setSize 2  (EncodedInsn ((w' `unsafeShiftL` 8 ) .|. a) 0)
      2  -> setSize 3  (EncodedInsn ((w' `unsafeShiftL` 16) .|. a) 0)
      3  -> setSize 4  (EncodedInsn ((w' `unsafeShiftL` 24) .|. a) 0)
      4  -> setSize 5  (EncodedInsn ((w' `unsafeShiftL` 32) .|. a) 0)
      5  -> setSize 6  (EncodedInsn ((w' `unsafeShiftL` 40) .|. a) 0)
      6  -> setSize 7  (EncodedInsn ((w' `unsafeShiftL` 48) .|. a) 0)
      7  -> setSize 8  (EncodedInsn ((w' `unsafeShiftL` 56) .|. a) 0)
      8  -> setSize 9  (EncodedInsn a w')
      9  -> setSize 10 (EncodedInsn a ((w' `unsafeShiftL` 8) .|. b))
      10 -> setSize 11 (EncodedInsn a ((w' `unsafeShiftL` 16) .|. b))
      11 -> setSize 12 (EncodedInsn a ((w' `unsafeShiftL` 24) .|. b))
      12 -> setSize 13 (EncodedInsn a ((w' `unsafeShiftL` 32) .|. b))
      13 -> setSize 14 (EncodedInsn a ((w' `unsafeShiftL` 40) .|. b))
      14 -> setSize 15 (EncodedInsn a ((w' `unsafeShiftL` 48) .|. b))
      n  -> error $ "putByte: current size too big (" ++ show n ++ ")"

showBytes :: EncodedInsn -> [Word8]
showBytes i@(EncodedInsn a b) = fmap fromIntegral shown
  where
    shown = take (fromIntegral (getSize i)) show_all
    show_all =
      [ a .&. 0xFF
      , (a `unsafeShiftR` 8) .&. 0xFF
      , (a `unsafeShiftR` 16) .&. 0xFF
      , (a `unsafeShiftR` 24) .&. 0xFF
      , (a `unsafeShiftR` 32) .&. 0xFF
      , (a `unsafeShiftR` 40) .&. 0xFF
      , (a `unsafeShiftR` 48) .&. 0xFF
      , (a `unsafeShiftR` 56) .&. 0xFF
      , b .&. 0xFF
      , (b `unsafeShiftR` 8) .&. 0xFF
      , (b `unsafeShiftR` 16) .&. 0xFF
      , (b `unsafeShiftR` 24) .&. 0xFF
      , (b `unsafeShiftR` 32) .&. 0xFF
      , (b `unsafeShiftR` 40) .&. 0xFF
      , (b `unsafeShiftR` 48) .&. 0xFF
      ]
      
