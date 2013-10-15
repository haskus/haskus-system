{-# LANGUAGE ForeignFunctionInterface #-}
module ViperVM.Platform.Platform (
   Memory(..), Buffer(..),
   MemoryPeer(..), BufferPeer(..), AllocError(..),
   allocate, release
) where

import Control.Applicative ( (<$>), pure )
import Control.Concurrent.STM
import Data.Traversable
import Data.Foldable
import Data.Monoid (mempty)
import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.C.Types

import ViperVM.Platform.OpenCL

-- Should be in base: http://haskell.1045720.n5.nabble.com/Proposal-Add-the-missing-instances-for-Traversable-Either-b-and-Traversable-b-td5715398.html
instance Foldable (Either e) where
   foldMap f (Right m) = f m
   foldMap _ (Left _) = mempty

instance Traversable (Either e) where
   traverse _ (Left e) = pure (Left e)
   traverse f (Right x) = Right <$> f x

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

data AllocError = 
     ErrAllocOutOfMemory
   | ErrAllocUnknown

-- | Allocate a buffer of the given size in the memory 
allocate :: BufferSize -> Memory -> IO (Either AllocError Buffer)
allocate size mem = allocPeer >>= traverse wrapStore
   where
      allocPeer = case memoryPeer mem of
         HostMemory   -> allocateHost size mem
         CUDAMemory   -> undefined
         OpenCLMemory -> undefined
         DiskMemory   -> undefined

      wrapStore peer = do
         let buf = Buffer mem size peer
         atomically $ modifyTVar (memoryBuffers mem) ((:) buf)
         return buf

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

--------------------------------------------------------
-- Host
--------------------------------------------------------

foreign import ccall unsafe "stdlib.h malloc"  malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()

-- | Allocate a buffer in host memory
allocateHost :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocateHost size _ = do
   ptr <- malloc (fromIntegral size)
   return $ if ptr == nullPtr
      then Left ErrAllocOutOfMemory
      else Right (HostBuffer ptr)

--------------------------------------------------------
-- OpenCL
--------------------------------------------------------

