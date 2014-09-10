module ViperVM.Platform.Drivers
   ( MemoryPeer(..)
   , BufferPeer(..)
   , ProcPeer(..)
   , NetworkPeer(..)
   , allocateBuffer
   , releaseBuffer
   , transferRegion
   , bufferUID
   )
where

import Control.Applicative ((<$>))
import Data.Word (Word64)

import ViperVM.Arch.Common.Errors
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Memory.Region
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


data Driver mem buf = Driver 
   { driverAllocateBuffer :: Word64 -> mem -> IO (Either AllocError buf)
   , driverReleaseBuffer :: mem -> buf -> IO ()
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

-- | Transfer a region
transferRegion :: NetworkPeer -> (BufferPeer,Region) -> (BufferPeer,Region) -> IO TransferResult
transferRegion net (srcBuffer,srcRegion) (dstBuffer,dstRegion) =

   case (net,srcBuffer,dstBuffer) of

      (OpenCLNetwork net', OpenCLBuffer buf, HostBuffer hbuf) ->
         OpenCL.transferDeviceToHost net' (buf,srcRegion) (Host.hostBufferPtr hbuf, dstRegion)

      (OpenCLNetwork net', HostBuffer hbuf, OpenCLBuffer buf) ->
         OpenCL.transferHostToDevice net' (Host.hostBufferPtr hbuf, srcRegion) (buf,dstRegion) 


      _ -> return (TransferError ErrTransferInvalid)

-- | Buffer unique identifier
bufferUID :: BufferPeer -> String
bufferUID buf = case buf of
   OpenCLBuffer b -> OpenCL.clBufferUID b
   HostBuffer b -> Host.hostBufferUID b
