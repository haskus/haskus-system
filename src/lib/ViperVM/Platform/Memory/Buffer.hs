-- | Buffer
module ViperVM.Platform.Memory.Buffer
   ( Buffer(..)
   , bufferUID
   )
where

import Data.Word (Word64)
import Data.Ord (comparing)

import qualified ViperVM.Platform.Drivers as Peer

-- | Allocated memory area
data Buffer = Buffer
   { bufferSize :: Word64
   , bufferPeer :: Peer.BufferPeer
   } deriving (Eq)

instance Ord Buffer where
   compare = comparing bufferPeer

bufferUID :: Buffer -> String
bufferUID = Peer.bufferUID . bufferPeer
