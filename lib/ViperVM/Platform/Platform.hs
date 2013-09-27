{-# LANGUAGE ForeignFunctionInterface #-}
module ViperVM.Platform.Platform (
   Memory(..), Buffer(..), Result(..),
   MemoryPeer(..), BufferPeer(..), AllocError(..),
   allocate, release
) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.C.Types

type ID = Int

data Memory = Memory {
   memoryId :: ID,
   memoryPeer :: MemoryPeer,
   memoryBuffers :: TVar [Buffer]
}

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

data MemoryPeer =
     HostMemory
   | CUDAMemory
   | OpenCLMemory
   | DiskMemory

data Buffer = Buffer {
   bufferMemory :: Memory,
   bufferSize :: BufferSize,
   bufferPeer :: BufferPeer
} deriving (Eq)

data BufferPeer = 
     HostBuffer (Ptr ())
   | CUDABuffer
   | OpenCLBuffer
   | DiskBuffer
   deriving (Eq)

type BufferSize = Word64

data Result a b = Success a | Error b

onSuccess :: Result a b -> (a -> IO c) -> IO (Result c b)
onSuccess (Success a) f = Success <$> f a
onSuccess (Error b) _ = return (Error b)

data AllocError = 
     ErrAllocOutOfMemory
   | ErrAllocUnknown

-- | Allocate a buffer of the given size in the memory 
allocate :: Word64 -> Memory -> IO (Result Buffer AllocError)
allocate size mem = do

   bufPeer <- case (memoryPeer mem) of
      HostMemory   -> allocateHost size mem
      CUDAMemory   -> undefined
      OpenCLMemory -> undefined
      DiskMemory   -> undefined

   onSuccess bufPeer $ \peer -> do
      let buf = Buffer mem size peer
      atomically $ do
         bufs <- readTVar (memoryBuffers mem)
         writeTVar (memoryBuffers mem) (buf:bufs)
      return buf

foreign import ccall unsafe "stdlib.h malloc"  malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()

-- | Allocate a buffer in host memory
allocateHost :: Word64 -> Memory -> IO (Result BufferPeer AllocError)
allocateHost size _ = do
   ptr <- malloc (fromIntegral size)
   return $ if ptr == nullPtr
      then Error ErrAllocOutOfMemory
      else Success (HostBuffer ptr)

-- | Release a buffer
release :: Buffer -> IO ()
release buf = do
   atomically $ do
      let bufsVar = memoryBuffers (bufferMemory buf)
      bufs <- readTVar bufsVar
      writeTVar bufsVar (delete buf bufs)

   case bufferPeer buf of
      HostBuffer ptr -> free ptr
      CUDABuffer     -> undefined
      OpenCLBuffer   -> undefined
      DiskBuffer     -> undefined
