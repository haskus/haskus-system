{-# LANGUAGE ForeignFunctionInterface, TupleSections #-}

-- | Abstract platform
module ViperVM.Platform.Platform (
   Platform(..), PlatformConfig(..),
   Memory(..), MemoryPeer(..),
   Buffer(..), BufferPeer(..), AllocError(..),
   Region(..),
   Link(..), LinkPeer(..),
   TransferError(..),
   loadPlatform,
   allocateBuffer, releaseBuffer,
   transferRegion
) where

import Control.Applicative ( (<$>), pure )
import Control.Monad (forM)
import Control.Concurrent.STM
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty)
import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.C.Types

import qualified ViperVM.Platform.OpenCL as CL

data PlatformConfig = PlatformConfig {
   libraryOpenCL :: String
}

data Platform = Platform {
   platformMemories :: [Memory],
   -- OpenCL specific
   platformOpenCLLibrary :: CL.Library,
   platformOpenCLPlatforms :: [CL.Platform]
}

-- | Load the platform
loadPlatform :: PlatformConfig -> IO Platform
loadPlatform config = do
   -- Load OpenCL devices
   clLib <- CL.loadOpenCL (libraryOpenCL config)
   clPlatforms <- CL.getPlatforms clLib
   clDevices <- concat <$> forM clPlatforms (\pf -> map (pf,) <$> CL.getPlatformDevices clLib pf)
   clContexts <- forM clDevices (\(pf,dev) -> CL.createContext clLib pf [dev])
   clMemories <- forM (clContexts `zip` clDevices) $ \(ctx,(_,dev)) -> case ctx of
      Right ctx' -> wrapMemoryPeer 0 (OpenCLMemory clLib dev ctx')
      Left err -> error ("Invalid context: " ++ show err)

   -- TODO: load other devices (CUDA, CPU...)
   let cpuMemories = []

   -- Assign valid IDs to memories
   let
      setMemoryId m i = m {memoryId = i}
      allMemories = cpuMemories ++ clMemories
      memories = zipWith setMemoryId allMemories [0..]

   return Platform {
      platformMemories = memories,
      platformOpenCLLibrary = clLib,
      platformOpenCLPlatforms = clPlatforms
   }



-- Should be in base: http://haskell.1045720.n5.nabble.com/Proposal-Add-the-missing-instances-for-Traversable-Either-b-and-Traversable-b-td5715398.html
instance Foldable (Either e) where
   foldMap f (Right m) = f m
   foldMap _ (Left _) = mempty

instance Traversable (Either e) where
   traverse _ (Left e) = pure (Left e)
   traverse f (Right x) = Right <$> f x

-- | Unique identifier
type ID = Int

-- | Memory
data Memory = Memory {
   memoryId :: ID,
   memoryPeer :: MemoryPeer,
   memoryBuffers :: TVar [Buffer]
}

wrapMemoryPeer :: ID -> MemoryPeer -> IO Memory
wrapMemoryPeer ident peer = Memory ident peer <$> newTVarIO []

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

-- | Backend specific memory fields
data MemoryPeer =
     HostMemory
   | CUDAMemory
   | OpenCLMemory CL.Library CL.Device CL.Context
   | DiskMemory

-- | Memory buffer
data Buffer = Buffer {
   bufferMemory :: Memory,
   bufferSize :: BufferSize,
   bufferPeer :: BufferPeer
} deriving (Eq)

-- | Backend specific buffer fields
data BufferPeer = 
     HostBuffer (Ptr ())
   | CUDABuffer
   | OpenCLBuffer CL.Library CL.Device CL.Context CL.Mem
   | DiskBuffer
   deriving (Eq)

type BufferSize = Word64

type RegionOffset = CSize
type RegionSize = CSize
type RegionStride = Word

-- | A region in a buffer
data Region =
     Region1D RegionOffset RegionSize
   | Region2D RegionOffset Word64 RegionSize RegionStride

-- | Buffer allocation error
data AllocError = 
     ErrAllocOutOfMemory
   | ErrAllocUnknown

-- | Region transfer error
data TransferError =
     ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown


-- | A link between two memories
data Link = Link {
   linkId :: ID,
   linkPeer :: LinkPeer
}

instance Eq Link where 
   (==) a b = linkId a == linkId b

data LinkPeer =
     OpenCLLink CL.Library CL.Device CL.CommandQueue

-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: BufferSize -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = allocPeer size mem >>= traverse wrapStore
   where
      allocPeer = case memoryPeer mem of
         HostMemory      -> allocateHost
         CUDAMemory      -> undefined
         OpenCLMemory {} -> allocateOpenCL
         DiskMemory      -> undefined

      wrapStore peer = do
         let buf = Buffer mem size peer
         -- Add allocated buffer to memory buffer list
         atomically $ modifyTVar (memoryBuffers mem) ((:) buf)
         return buf

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
      OpenCLBuffer lib _ _ mem -> CL.releaseBuffer lib mem
      DiskBuffer     -> undefined

-- | Perform a synchronous region transfer
transferRegion :: Link -> Buffer -> Region -> Buffer -> Region -> IO (Maybe TransferError)
transferRegion link bufIn regIn bufOut regOut = do
   let
      bufPeerIn = bufferPeer bufIn
      bufPeerOut = bufferPeer bufOut
      lnkPeer = linkPeer link
      clTransfer _ (Left _) = return (Just ErrTransferUnknown)
      clTransfer lib (Right ev) = CL.waitForEvents lib [ev] >> return Nothing

      -- 1D transfers
      transfer1D off1 off2 sz = case (lnkPeer,bufPeerIn,bufPeerOut) of

         -- OpenCL 1D CL -> Host
         (OpenCLLink _ _ cq, OpenCLBuffer lib _ _ mem, HostBuffer ptr) -> do
            let ptr2 = ptr `plusPtr` (fromIntegral off2) -- TODO: unsafe coercion from CSize to Int
            clTransfer lib =<< CL.enqueueReadBuffer lib cq mem True off1 sz ptr2 []

         -- OpenCL 1D Host -> CL
         (OpenCLLink _ _ cq, HostBuffer ptr, OpenCLBuffer lib _ _ mem) -> do
            let ptr2 = ptr `plusPtr` (fromIntegral off1) -- TODO: unsafe coercion from CSize to Int
            clTransfer lib =<< CL.enqueueWriteBuffer lib cq mem True off2 sz ptr2 []

         _ -> return (Just ErrTransferInvalid)

      -- 2D transfers
      transfer2D = undefined

   -- TODO: check that link interconnects buffers

   case (regIn,regOut) of
      (Region1D off1 sz1, Region1D off2 sz2)
         | sz1 == sz2 -> transfer1D off1 off2 sz1
      (Region2D _ n1 sz1 _, Region2D _ n2 sz2 _)
         | n1 == n2 && sz1 == sz2 -> transfer2D regIn regOut
      _ -> return (Just ErrTransferIncompatibleRegions)


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

allocateOpenCL :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocateOpenCL size mem = do
   let 
      OpenCLMemory lib dev ctx = memoryPeer mem
      flags = []
   buf <- CL.createBuffer lib dev ctx flags (fromIntegral size)
   
   return $ case buf of
      Right m -> Right (OpenCLBuffer lib dev ctx m)
      Left _ -> Left ErrAllocUnknown -- FIXME: return appropriate error
