{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Buffer builder
module ViperVM.Format.Binary.BufferBuilder
   ( BufferBuilder
   , emptyBufferBuilder
   , toBufferList
   , toBuffer
   , fromBuffer
   , fromWord8
   )
where

import qualified Data.ByteString.Builder as B
import Data.Word

import ViperVM.Format.Binary.Buffer
import qualified ViperVM.Format.Binary.BufferList as BL

-- | Buffer builder
newtype BufferBuilder = BufferBuilder B.Builder

deriving instance Monoid BufferBuilder

-- | Empty buffer builder
emptyBufferBuilder :: BufferBuilder
emptyBufferBuilder = BufferBuilder mempty

-- | Create a Builder denoting the same sequence of bytes as a strict
-- ByteString. The Builder inserts large ByteStrings directly, but copies small
-- ones to ensure that the generated chunks are large on average.
fromBuffer :: Buffer -> BufferBuilder
fromBuffer (Buffer bs) = BufferBuilder (B.byteString bs)

-- | Encode a single unsigned byte as-is.
fromWord8 :: Word8 -> BufferBuilder
fromWord8 w = BufferBuilder (B.word8 w)

-- | Execute a Builder and return the generated chunks as a BufferList. The work
-- is performed lazily, i.e., only when a chunk of the BufferList is forced.
toBufferList :: BufferBuilder -> BL.BufferList
toBufferList (BufferBuilder b) = BL.BufferList (B.toLazyByteString b)

-- | Execute a Builder and return the generated chunks as a Buffer.
toBuffer :: BufferBuilder -> Buffer
toBuffer = BL.toBuffer . toBufferList
