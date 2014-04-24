-- | Buffer management
module ViperVM.Platform.Buffer (
   allocateBuffer, allocateBufferFromRegion, releaseBuffer
) where

import Control.Applicative ((<$>), pure)
import Data.Monoid (mempty)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar)
import Data.List (delete)

import qualified ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.Arch.Posix.All as Posix
import ViperVM.Arch.Common.Errors

import ViperVM.Platform.Types
import ViperVM.MMU.Region (regionCover, Region(..), Shape(..))

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
      (Region off (Shape1D sz)) = regionCover reg
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
