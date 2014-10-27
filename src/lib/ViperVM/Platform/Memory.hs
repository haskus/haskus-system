-- | Memory related module
module ViperVM.Platform.Memory
   ( memoryEndianness
   , memorySize
   , memoryBufferCount
   , memoryBufferAllocate
   , memoryBufferRelease
   )
where

import Data.Word (Word64)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Control.Applicative ((<$>))

import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import qualified ViperVM.STM.TSet as TSet
import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import ViperVM.Platform.Types (Memory(..), Buffer(..))

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

-- | Get the number of buffers allocated in the given memory
memoryBufferCount :: Memory -> STM Int
memoryBufferCount mem = TSet.size (memoryBuffers mem)

-- | Allocate a buffer of the given size in the memory 
memoryBufferAllocate :: Word64 -> Memory -> IO (Either AllocError Buffer)
memoryBufferAllocate sz mem = do
   b <- fmap (Buffer mem sz) <$> Peer.allocateBuffer sz (memoryPeer mem)

   forM_ b $ \b' ->
      -- Add allocated buffer to memory buffer list
      atomically $ TSet.insert b' (memoryBuffers mem)

   return b


-- | Release a buffer
memoryBufferRelease :: Buffer -> IO ()
memoryBufferRelease b = do
   let mem = bufferMemory b

   -- Remove buffer from memory buffer list
   atomically $ TSet.delete b (memoryBuffers mem)

   Peer.releaseBuffer (memoryPeer mem) (bufferPeer b)
