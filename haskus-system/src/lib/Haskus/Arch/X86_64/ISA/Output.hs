module Haskus.Arch.X86_64.ISA.Output
  ( Output (..)
  , Location
  , putW16
  , putW32
  , putW64
  , putI8
  , putI16
  , putI32
  , putI64
  , putAsciiZ
  )
where

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

type Location = Word64

class Monad m => Output m where
  -- | Output the given byte
  putW8 :: Word8 -> m ()

  -- | Return current location (number of bytes already written)
  getLoc :: m Location

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

-- | Output the string in ASCII with a 0-ending character
putAsciiZ :: Output m => [Char] -> m ()
putAsciiZ s = do
  mapM_ (putW8 . fromIntegral . ord) s
  putW8 0
