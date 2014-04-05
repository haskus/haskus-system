{-# LANGUAGE ForeignFunctionInterface #-}

-- | Buffer allocation
module ViperVM.Platform.AllocFree (
   allocateBuffer, allocateBufferFromRegion, releaseBuffer,
   allocateHost, free,
   allocateOpenCL,
) where

import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.Types (CSize(..))
import Control.Applicative ((<$>), pure)
import Data.Monoid (mempty)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar)
import Data.List (delete)

import qualified ViperVM.Arch.OpenCL as CL
import ViperVM.Platform.Types
import ViperVM.MMU.Region (regionCover, Region(..))

-- Should be in base: http://haskell.1045720.n5.nabble.com/Proposal-Add-the-missing-instances-for-Traversable-Either-b-and-Traversable-b-td5715398.html
instance Foldable (Either e) where
   foldMap f (Right m) = f m
   foldMap _ (Left _) = mempty

instance Traversable (Either e) where
   traverse _ (Left e) = pure (Left e)
   traverse f (Right x) = Right <$> f x


--------------------------------------------------------
-- Generic
--------------------------------------------------------

-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: BufferSize -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = allocPeer size mem >>= traverse wrapStore
   where
      allocPeer = case memoryPeer mem of
         HostMemory {}   -> allocateHost
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
      (Region1D off sz) = regionCover reg
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
      HostBuffer ptr -> free ptr
      CUDABuffer     -> undefined
      OpenCLBuffer _ _ mem -> CL.release mem
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
      Left _ -> Left ErrAllocUnknown -- FIXME: return appropriate error
