-- | Buffer list
--
-- BufferList is a lazy ByteString
module ViperVM.Format.Binary.BufferList
   ( BufferList (..)
   , toBuffer
   )
where

import qualified Data.ByteString.Lazy as LBS

import ViperVM.Format.Binary.Buffer

-- | BufferList
newtype BufferList = BufferList LBS.ByteString

-- | Convert to a buffer
toBuffer :: BufferList -> Buffer
toBuffer (BufferList b) = Buffer (LBS.toStrict b)
