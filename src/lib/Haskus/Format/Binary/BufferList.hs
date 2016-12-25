-- | Buffer list
--
-- BufferList is a lazy ByteString
module Haskus.Format.Binary.BufferList
   ( BufferList (..)
   , toBuffer
   , toBufferList
   , toLazyByteString
   )
where

import qualified Data.ByteString.Lazy as LBS

import Haskus.Format.Binary.Buffer

-- | BufferList
newtype BufferList = BufferList LBS.ByteString

-- | Convert to a buffer
toBuffer :: BufferList -> Buffer
toBuffer (BufferList b) = Buffer (LBS.toStrict b)

-- | Convert from a buffer
toBufferList :: Buffer -> BufferList
toBufferList (Buffer b) = BufferList (LBS.fromStrict b)

-- | Convert to a lazy ByteString
toLazyByteString :: BufferList -> LBS.ByteString
toLazyByteString (BufferList b) = b
