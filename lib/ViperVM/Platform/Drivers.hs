module ViperVM.Platform.Drivers (
   MemoryPeer(..), BufferPeer(..),
   ProcPeer(..), NetworkPeer(..),
   allocateBuffer, releaseBuffer
) where

import Control.Applicative ((<$>))
import Data.Word (Word64)

import ViperVM.Arch.Common.Errors
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

-- | Backend specific memory fields
data MemoryPeer = 
     HostMemory Host.Memory
   | OpenCLMemory OpenCL.Memory
   deriving (Eq,Ord)


-- | Backend specific buffer fields
data BufferPeer = 
     HostBuffer Host.Buffer
   | OpenCLBuffer OpenCL.Buffer
   deriving (Eq,Ord)


-- | Backend specific processor fields
data ProcPeer =
     HostProc Host.Proc
   | OpenCLProc OpenCL.Proc
   deriving (Eq,Ord)

data NetworkPeer =
     OpenCLNetwork OpenCL.Network
   deriving (Eq,Ord)


data Driver m b = Driver {
   driverAllocateBuffer :: Word64 -> m -> IO (Either AllocError b),
   driverReleaseBuffer :: m -> b -> IO ()
}

openclDriver :: Driver OpenCL.Memory OpenCL.Buffer
openclDriver = Driver
   OpenCL.allocateBuffer
   OpenCL.releaseBuffer

hostDriver :: Driver Host.Memory Host.Buffer
hostDriver = Driver
   Host.allocateBuffer
   Host.releaseBuffer


-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: Word64 -> MemoryPeer -> IO (Either AllocError BufferPeer)
allocateBuffer size (HostMemory m) = fmap HostBuffer <$> driverAllocateBuffer hostDriver size m
allocateBuffer size (OpenCLMemory m) = fmap OpenCLBuffer <$> driverAllocateBuffer openclDriver size m

-- | Release a buffer
releaseBuffer :: MemoryPeer -> BufferPeer -> IO ()
releaseBuffer (HostMemory m) (HostBuffer b) = driverReleaseBuffer hostDriver m b
releaseBuffer (OpenCLMemory m) (OpenCLBuffer b) = driverReleaseBuffer openclDriver m b
releaseBuffer _ _ = error "Trying to release a buffer from an invalid memory"
