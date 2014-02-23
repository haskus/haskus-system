{-# LANGUAGE TupleSections #-}

-- | Abstract platform
module ViperVM.Platform.Platform (
   Platform(..), PlatformConfig(..), defaultConfig,
   loadPlatform,
   memoryEndianness, memorySize,
   allocateBuffer, allocateBufferFromRegion, releaseBuffer,
   transferRegion
) where

import Control.Applicative ( (<$>), pure )
import Control.Monad (forM,filterM)
import Control.Concurrent.STM
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty)
import Data.List
import Data.Word (Word64)
import Foreign.Ptr

import qualified ViperVM.Platform.OpenCL as CL
import qualified ViperVM.Platform.CPU as CPU
import ViperVM.Platform.Endianness
import ViperVM.Platform.AllocFree
import ViperVM.Platform.Types
import ViperVM.MMU.Region (regionCover, Region(..))

data PlatformConfig = PlatformConfig {
   libraryOpenCL :: String,
   filterOpenCLDevices :: CL.Device -> IO Bool,
   sysfsPath :: String
}

defaultConfig :: PlatformConfig
defaultConfig = PlatformConfig {
   libraryOpenCL = "libOpenCL.so",
   filterOpenCLDevices = const (return True),
   sysfsPath = "/sys"
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
   clDevices <- concat <$> forM clPlatforms (\pf -> map (pf,) <$> CL.getPlatformDevices pf)
   clDevices' <- filterM (filterOpenCLDevices config . snd) clDevices
   clContexts <- forM clDevices' (\(pf,dev) -> CL.createContext pf [dev])
   clMemories <- forM (clContexts `zip` clDevices') $ \(ctx,(_,dev)) -> case ctx of
      Left err -> error ("Invalid context: " ++ show err)
      Right ctx' -> do
         endianness <- getOpenCLDeviceEndianness dev
         size <- CL.getDeviceGlobalMemSize' dev
         let memPeer = OpenCLMemory {
                  clMemLibrary = clLib,
                  clMemDevice = dev,
                  clMemContext = ctx',
                  clMemEndianness = endianness,
                  clMemSize = size
               }
         wrapMemoryPeer 0 memPeer

   -- | Load host
   numa <- CPU.loadNUMA (sysfsPath config)
   hostEndianness <- getMemoryEndianness
   hostMemories <- forM (CPU.numaNodes numa) $ \node -> do
      let m = CPU.nodeMemory node
      (total,_) <- CPU.nodeMemoryStatus m
      let memPeer = HostMemory {
         hostMemEndianness = hostEndianness,
         hostMemSize = total
      }
      wrapMemoryPeer 0 memPeer

   -- TODO: load other devices (CUDA...)

   -- Assign valid IDs to memories
   let
      setMemoryId m i = m {memoryId = i}
      allMemories = hostMemories ++ clMemories
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

wrapMemoryPeer :: ID -> MemoryPeer -> IO Memory
wrapMemoryPeer ident peer = Memory ident peer <$> newTVarIO []

-- | Indicate the endianness of a memory
memoryEndianness :: Memory -> Endianness
memoryEndianness mem = case memoryPeer mem of
   m@(OpenCLMemory {}) -> clMemEndianness m
   m@(HostMemory {}) -> hostMemEndianness m

-- | Return total memory size
memorySize :: Memory -> Word64
memorySize mem = case memoryPeer mem of
   m@(OpenCLMemory {}) -> clMemSize m
   m@(HostMemory {}) -> hostMemSize m

-- | Allocate a buffer of the given size in the memory 
allocateBuffer :: BufferSize -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = allocPeer size mem >>= traverse wrapStore
   where
      allocPeer = case memoryPeer mem of
         HostMemory {}   -> allocateHost
         CUDAMemory      -> undefined
         OpenCLMemory {} -> allocateOpenCL
         DiskMemory      -> undefined

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
      OpenCLBuffer _ _ mem -> CL.releaseBuffer mem
      DiskBuffer     -> undefined

-- | Perform a synchronous region transfer
transferRegion :: Link -> Buffer -> Region -> Buffer -> Region -> IO (Maybe TransferError)
transferRegion link bufIn regIn bufOut regOut = do
   let
      bufPeerIn = bufferPeer bufIn
      bufPeerOut = bufferPeer bufOut
      lnkPeer = linkPeer link
      clTransfer (Left _) = return (Just ErrTransferUnknown)
      clTransfer (Right ev) = CL.waitForEvents [ev] >> return Nothing

      -- 1D transfers
      transfer1D off1 off2 sz = case (lnkPeer,bufPeerIn,bufPeerOut) of

         -- OpenCL 1D CL -> Host
         (OpenCLLink _ _ cq, OpenCLBuffer _ _ mem, HostBuffer ptr) -> do
            let ptr2 = ptr `plusPtr` fromIntegral off2 -- TODO: unsafe coercion from CSize to Int
            clTransfer =<< CL.enqueueReadBuffer cq mem True off1 sz ptr2 []

         -- OpenCL 1D Host -> CL
         (OpenCLLink _ _ cq, HostBuffer ptr, OpenCLBuffer _ _ mem) -> do
            let ptr2 = ptr `plusPtr` fromIntegral off1 -- TODO: unsafe coercion from CSize to Int
            clTransfer =<< CL.enqueueWriteBuffer cq mem True off2 sz ptr2 []

         _ -> return (Just ErrTransferInvalid)

      -- 2D transfers
      transfer2D = undefined

   -- TODO: check that link interconnects buffers

   case (regIn,regOut) of
      (Region1D off1 sz1, Region1D off2 sz2)
         | sz1 == sz2 -> transfer1D (fromIntegral off1) (fromIntegral off2) (fromIntegral sz1)
      (Region2D _ n1 sz1 _, Region2D _ n2 sz2 _)
         | n1 == n2 && sz1 == sz2 -> transfer2D regIn regOut
      _ -> return (Just ErrTransferIncompatibleRegions)


