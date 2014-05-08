-- | Buffer management
module ViperVM.Platform.Memory.Buffer (
   allocateBuffer, allocateBufferFromRegion, releaseBuffer
) where

import Control.Applicative ((<$>))
import Data.Traversable (traverse)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar)
import Data.List (delete)

import qualified ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.Arch.Posix.Malloc as Posix
import ViperVM.Arch.Common.Errors

import ViperVM.Platform.Topology
import ViperVM.Platform.Memory.Region (regionCover1D, Region(..), Shape(..))

--------------------------------------------------------
-- Generic
--------------------------------------------------------

-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: BufferSize -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = allocPeer size mem >>= traverse wrapStore
   where
      allocPeer = case memoryPeer mem of
         HostMemory {}   -> allocatePosix
         OpenCLMemory {} -> allocateOpenCL

      wrapStore peer = do
         let buf = Buffer mem size peer
         -- Add allocated buffer to memory buffer list
         atomically $ modifyTVar (memoryBuffers mem) ((:) buf)
         return buf

-- | Allocate a buffer able to contain the given region
allocateBufferFromRegion :: Region -> Memory -> IO (Either AllocError Buffer)
allocateBufferFromRegion reg = allocateBuffer bs
   where
      (Region off (Shape1D sz)) = regionCover1D reg
      bs = off + sz

-- | Release a buffer
releaseBuffer :: Buffer -> IO ()
releaseBuffer buf = do
   atomically $ do
      -- Remove buffer from memory buffer list
      let bufsVar = memoryBuffers (bufferMemory buf)
      bufs <- readTVar bufsVar
      writeTVar bufsVar (delete buf bufs)

   case bufferPeer buf of
      HostBuffer ptr -> Posix.free ptr
      CUDABuffer     -> undefined
      OpenCLBuffer _ _ mem -> CL.release mem
      DiskBuffer     -> undefined

-- | Allocate a buffer in host memory
allocatePosix :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocatePosix size _ = fmap HostBuffer <$> Posix.malloc (fromIntegral size)

-- | Allocate a buffer in OpenCL memory
allocateOpenCL :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocateOpenCL size mem = do
   let 
      peer = memoryPeer mem
      ctx = clMemContext peer
      dev = clMemDevice peer
      flags = []
   buf <- CL.createBuffer dev ctx flags (fromIntegral size)
   
   return $ case buf of
      Right m -> Right (OpenCLBuffer dev ctx m)
      Left _ -> Left AllocUnknownError -- FIXME: return appropriate error
