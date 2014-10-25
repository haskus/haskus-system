{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A /buffer/ is the most basic region of contiguous memory cells. It can be
-- virtual or it can correspond to a physical memory space.
-- 
-- We can have buffers of the following kinds:
--   * Flat (non-virtual) memory: a contiguous region of the memory
--   * Virtual memory: a contiguous region in the virtual memory address space
--   (e.g. a malloc-ed memory region in POSIX systems)
--   * Hard disk: a set of contiguous segments
--   * File system: a file

module ViperVM.Platform.Memory.Buffer
   ( Buffer(..)
   , BufferID
   , bufferUID
   )
where

import Data.Word (Word64)
import Data.Ord (comparing)
import Data.Hashable

import qualified ViperVM.Platform.Drivers as Peer

-- | A buffer
data Buffer = Buffer
   { bufferSize :: Word64           -- ^ Size of the buffer
   , bufferPeer :: Peer.BufferPeer  -- ^ Buffer peer
   } deriving (Eq)

instance Ord Buffer where
   compare = comparing bufferPeer

-- | Buffer uniue identifier
newtype BufferID = BufferID String deriving (Eq,Ord,Show,Read,Hashable)

-- | Return a unique identifier for the buffer
bufferUID :: Buffer -> BufferID
bufferUID = BufferID . Peer.bufferUID . bufferPeer

instance Hashable Buffer where
   hashWithSalt salt m = hashWithSalt salt (bufferUID m)
