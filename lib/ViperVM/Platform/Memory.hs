-- | Memory related module
module ViperVM.Platform.Memory (
   memoryEndianness, memorySize,
   allocateBuffer, releaseBuffer
) where

import Data.Word (Word64)
import qualified Data.Set as Set
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Control.Applicative ((<$>))

import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import ViperVM.Platform.Topology
import ViperVM.Platform.Memory.Buffer

-- | Indicate the endianness of a memory
memoryEndianness :: Memory -> Endianness
memoryEndianness mem = case memoryPeer mem of
   Peer.OpenCLMemory m -> OpenCL.clMemEndianness m
   Peer.HostMemory m -> Host.hostMemEndianness m

-- | Return total memory size
memorySize :: Memory -> Word64
memorySize mem = case memoryPeer mem of
   Peer.OpenCLMemory m -> OpenCL.clMemSize m
   Peer.HostMemory m -> Host.hostMemSize m

-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: Word64 -> Memory -> IO (Either AllocError MemoryBuffer)
allocateBuffer sz mem = do
   b <- fmap (Buffer sz) <$> Peer.allocateBuffer sz (memoryPeer mem)

   forM_ b $ \b' ->
      -- Add allocated buffer to memory buffer list
      atomically $ modifyTVar (memoryBuffers mem) (Set.insert b')

   return (MemoryBuffer mem <$> b)

-- | Release a buffer
releaseBuffer :: MemoryBuffer -> IO ()
releaseBuffer (MemoryBuffer mem buf) = do
   -- Remove buffer from memory buffer list
   atomically $ modifyTVar (memoryBuffers mem) (Set.delete buf)

   Peer.releaseBuffer (memoryPeer mem) (bufferPeer buf)
