module Haskus.Arch.X86_64.ISA.Output
  ( Output (..)
  , Put (..)
  , putW16
  , putW32
  , putW64
  , putI8
  , putI16
  , putI32
  , putI64
  , putAsciiZ
  , putSeq
  )
where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

class Monad m => Output m where
  -- | Output the given byte
  putW8 :: Word8 -> m ()

  -- | Return the number of bytes written
  getPos :: m Word

-- | Output a 16-bit word
putW16 :: Output m => Word16 -> m ()
putW16 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))

-- | Output a 32-bit word
putW32 :: Output m => Word32 -> m ()
putW32 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))
  putW8 (fromIntegral (v `unsafeShiftR` 16))
  putW8 (fromIntegral (v `unsafeShiftR` 24))

-- | Output a 64-bit word
putW64 :: Output m => Word64 -> m ()
putW64 v = do
  putW32 (fromIntegral v)
  putW32 (fromIntegral (v `unsafeShiftR` 32))

-- | Output a 8-bit int
putI8 :: Output m => Int8 -> m ()
putI8 v = putW8 (fromIntegral v)

-- | Output a 16-bit int
putI16 :: Output m => Int16 -> m ()
putI16 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))

-- | Output a 32-bit int
putI32 :: Output m => Int32 -> m ()
putI32 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))
  putW8 (fromIntegral (v `unsafeShiftR` 16))
  putW8 (fromIntegral (v `unsafeShiftR` 24))

-- | Output a 64-bit int
putI64 :: Output m => Int64 -> m ()
putI64 v = do
  putW32 (fromIntegral v)
  putW32 (fromIntegral (v `unsafeShiftR` 32))

-- | Output the given values in sequence
putSeq :: (Output m, Put a) => [a] -> m ()
putSeq xs = mapM_ put xs

-- | Output the string in ASCII with a 0-ending character
putAsciiZ :: Output m => [Char] -> m ()
putAsciiZ s = do
  mapM_ (putW8 . fromIntegral . ord) s
  putW8 0

class Put a where
  put :: Output m => a -> m ()

instance Put Word8  where put = putW8
instance Put Word16 where put = putW16
instance Put Word32 where put = putW32
instance Put Word64 where put = putW64
instance Put Int8   where put = putI8
instance Put Int16  where put = putI16
instance Put Int32  where put = putI32
instance Put Int64  where put = putI64
