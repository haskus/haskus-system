-- | Buffer
module ViperVM.Platform.Memory.Buffer
   ( Buffer(..)
   )
where

import Data.Word (Word64)
import Data.Ord (comparing)

import ViperVM.Platform.Drivers

-- | Allocated memory area
data Buffer = Buffer
   { bufferSize :: Word64
   , bufferPeer :: BufferPeer
   } deriving (Eq)

instance Ord Buffer where
   compare = comparing bufferPeer
