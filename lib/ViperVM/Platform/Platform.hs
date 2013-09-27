module ViperVM.Platform.Platform where

import Foreign.Ptr
import Control.Concurrent.STM
import Data.Word
import Foreign.Marshal.Alloc

type ID = Int

data Memory = Memory {
	memoryId :: ID,
	memoryPeer :: MemoryPeer,
	memoryBuffers :: TVar [Buffer]
}

data MemoryPeer =
	  HostMemory
	| CUDAMemory
	| OpenCLMemory
	| DiskMemory FilePath

data Buffer = Buffer {
	bufferMemory :: Memory,
   bufferSize :: BufferSize,
	bufferPeer :: BufferPeer
}

data BufferPeer = 
	  HostBuffer (Ptr ())
	| CUDABuffer
	| OpenCLBuffer
	| DiskBuffer FilePath

type BufferSize = Word64

data Result a b = Success a | Error b

data AllocError = 
     ErrAllocOutOfMemory
   | ErrAllocUnknown

-- | Allocate a buffer of the given size in the memory 
allocate :: Word64 -> Memory -> IO (Result Buffer AllocError)
allocate size mem = do

   bufPeer <- case (memoryPeer mem) of
      HostMemory -> allocateHost size mem
      CUDAMemory -> undefined
      OpenCLMemory -> undefined
      DiskMemory _ -> undefined

   case bufPeer of
      Error err -> return (Error err)
      Success peer -> do
         let buf = Buffer mem size peer
         atomically $ do
            bufs <- readTVar (memoryBuffers mem)
            writeTVar (memoryBuffers mem) (buf:bufs)
         return (Success buf)

-- | Allocate a buffer in host memory
allocateHost :: Word64 -> Memory -> IO (Result BufferPeer AllocError)
allocateHost size _ = do
   ptr <- mallocBytes (fromIntegral size)
   return $ if ptr == nullPtr 
      then Error ErrAllocUnknown --TODO: detect out of memory
      else Success (HostBuffer ptr)
