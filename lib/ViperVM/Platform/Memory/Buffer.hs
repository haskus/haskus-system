-- | Buffer management
module ViperVM.Platform.Memory.Buffer (
   Buffer(..)
) where

import Data.Word (Word64)

import ViperVM.Platform.Drivers

-- | Allocated memory area
data Buffer = Buffer {
   bufferSize :: Word64,
   bufferPeer :: BufferPeer
} deriving (Eq)
