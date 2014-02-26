-- | Abstract platform
module ViperVM.Platform.Platform (
   Platform(..), PlatformConfig(..), defaultConfig,
   loadPlatform,
   memoryEndianness, memorySize,
   allocateBuffer, allocateBufferFromRegion, releaseBuffer,
   transferRegion
) where

import Control.Applicative ( (<$>), pure )
import Control.Monad (filterM,void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Control.Concurrent.STM
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap, forM_)
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
   platformProcs :: [Proc],
   -- OpenCL specific
   platformOpenCLPlatforms :: [CL.Platform]
}

-- | State used during platform loading
data LoadState = LoadState {
   currentProcID :: ID,
   currentMemID :: ID,
   currentMemories :: [Memory],
   currentProcs :: [Proc]
}

-- | Initial loading state
initLoadState :: LoadState
initLoadState = LoadState {
   currentProcID = 0,
   currentMemID = 0,
   currentMemories = [],
   currentProcs = []
}

-- | Get new Proc ID
newProcId :: Monad m => StateT LoadState m ID
newProcId = do
   curr <- get
   let x = currentProcID curr
   put (curr { currentProcID = x+1 })
   return x

-- | Get new Mem ID
newMemId :: Monad m => StateT LoadState m ID
newMemId = do
   curr <- get
   let x = currentMemID curr
   put (curr { currentMemID = x+1 })
   return x

-- | Register a new memory
registerMemory :: [Proc] -> MemoryPeer -> StateT LoadState IO Memory
registerMemory procs peer = do
   memId <- newMemId
   mem <- lift $ wrapMemoryPeer memId procs peer
   curr <- get
   put (curr { currentMemories = mem : currentMemories curr})
   return mem

-- | Register a new proc
registerProc :: ProcPeer -> StateT LoadState IO Proc
registerProc peer = do
   procId <- newProcId
   proc <- lift $ wrapProcPeer procId peer
   curr <- get
   put (curr { currentProcs = proc : currentProcs curr})
   return proc



-- | Load OpenCL platform
loadOpenCLPlatform :: PlatformConfig -> StateT LoadState IO [CL.Platform]
loadOpenCLPlatform config = do
   lib <- lift $ CL.loadOpenCL (libraryOpenCL config)
   platforms <- lift $ CL.getPlatforms lib

   forM_ platforms $ \pf -> do
      devices <- lift $ filterM (filterOpenCLDevices config) =<< CL.getPlatformDevices pf
      forM_ devices $ \dev -> do
         context <- lift $ CL.createContext pf [dev]

         case context of
            Left err -> error ("Invalid context: " ++ show err)
            Right ctx' -> do
               endianness <- lift $ getOpenCLDeviceEndianness dev
               size <- lift $ CL.getDeviceGlobalMemSize' dev
               let memPeer = OpenCLMemory {
                        clMemLibrary = lib,
                        clMemDevice = dev,
                        clMemContext = ctx',
                        clMemEndianness = endianness,
                        clMemSize = size
                     }
                   prcPeer = OpenCLProc {
                        clProcDevice = dev,
                        clProcContext = ctx'
                     }
               proc <- registerProc prcPeer
               void (registerMemory [proc] memPeer)

   return platforms


-- | Load host platform
loadHostPlatform :: PlatformConfig -> StateT LoadState IO ()
loadHostPlatform config = do
   numa <- lift $ CPU.loadNUMA (sysfsPath config)
   hostEndianness <- lift $ getMemoryEndianness
   forM_ (CPU.numaNodes numa) $ \node -> do
      let m = CPU.nodeMemory node
      (total,_) <- lift $ CPU.nodeMemoryStatus m
      let memPeer = HostMemory {
            hostMemEndianness = hostEndianness,
            hostMemSize = total
          }
          procs = []
      void (registerMemory procs memPeer)


-- | Load the platform
loadPlatform :: PlatformConfig -> IO Platform
loadPlatform config = evalStateT loadPlatform_ initLoadState
   where
      loadPlatform_ :: StateT LoadState IO Platform
      loadPlatform_ = do
         
         loadHostPlatform config
         clPlatforms <- loadOpenCLPlatform config

         -- TODO: load other devices (CUDA...)
         
         memories <- reverse . currentMemories <$> get
         procs <- reverse . currentProcs <$> get

         return Platform {
            platformMemories = memories,
            platformProcs = procs,
            platformOpenCLPlatforms = clPlatforms
         }



-- Should be in base: http://haskell.1045720.n5.nabble.com/Proposal-Add-the-missing-instances-for-Traversable-Either-b-and-Traversable-b-td5715398.html
instance Foldable (Either e) where
   foldMap f (Right m) = f m
   foldMap _ (Left _) = mempty

instance Traversable (Either e) where
   traverse _ (Left e) = pure (Left e)
   traverse f (Right x) = Right <$> f x

-- | Wrap a memory peer into a memory object
wrapMemoryPeer :: ID -> [Proc] -> MemoryPeer -> IO Memory
wrapMemoryPeer ident procs peer = Memory ident procs peer <$> newTVarIO []

-- | Wrap a proc peer into a proc object
wrapProcPeer :: ID -> ProcPeer -> IO Proc
wrapProcPeer ident peer = return (Proc ident peer)

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
      OpenCLBuffer _ _ mem -> CL.release mem
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


