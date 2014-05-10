-- | Platform loading module
module ViperVM.Platform.Loading (
   loadPlatform
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM,void)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Data.Traversable (forM)

import qualified ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.Arch.GenericHost.All as Generic
import qualified ViperVM.Arch.Linux.Numa as Linux

import ViperVM.Platform.Drivers
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.Platform.Platform
import ViperVM.Platform.Topology
import ViperVM.Platform.Config

type L m a = StateT LoadState m a
type LIO a = L IO a

-- | State used during platform loading
data LoadState = LoadState {
   currentProcID :: ID,
   currentMemID :: ID,
   currentMemories :: [Memory],
   currentNetworks :: [Network],
   currentProcs :: [Proc]
}

-- | Initial loading state
initLoadState :: LoadState
initLoadState = LoadState {
   currentProcID = 0,
   currentMemID = 0,
   currentMemories = [],
   currentNetworks = [],
   currentProcs = []
}

-- | Get new Proc ID
newProcId :: Monad m => L m ID
newProcId = do
   curr <- get
   let x = currentProcID curr
   put (curr { currentProcID = x+1 })
   return x

-- | Get new Mem ID
newMemId :: Monad m => L m ID
newMemId = do
   curr <- get
   let x = currentMemID curr
   put (curr { currentMemID = x+1 })
   return x

-- | Register a new memory
registerMemory :: [Proc] -> MemoryPeer -> LIO Memory
registerMemory procs peer = do
   memId <- newMemId
   mem <- lift $ Memory memId procs peer <$> newTVarIO [] <*> newTVarIO []
   curr <- get
   put (curr { currentMemories = mem : currentMemories curr})
   return mem

-- | Register a new point-to-point link
registerNetwork :: NetworkType -> (Memory -> STM [Memory]) -> NetworkPeer -> LIO Network
registerNetwork typ neighbors peer = do
   let net = Network typ neighbors peer
   curr <- get
   put (curr { currentNetworks = net : currentNetworks curr})
   return net

-- | Register a new proc
registerProc :: ProcPeer -> LIO Proc
registerProc peer = do
   pId <- newProcId
   let proc = Proc pId peer
   curr <- get
   put (curr { currentProcs = proc : currentProcs curr})
   return proc


-- | Load OpenCL platform
loadOpenCLPlatform :: PlatformConfig -> LIO [CL.Platform]
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
               endianness <- lift $ CL.getDeviceEndianness dev
               size <- lift $ CL.getDeviceGlobalMemSize' dev

               -- FIXME: some implementations do not support profiling or out-of-order.
               -- We should avoid crashing here
               let props = [CL.CL_QUEUE_OUT_OF_ORDER, CL.CL_QUEUE_PROFILING]
               linkQueue <- lift $ CL.createCommandQueue' ctx' dev props

               let memPeer = OpenCLMemory $ OpenCL.Memory {
                        OpenCL.clMemLibrary = lib,
                        OpenCL.clMemDevice = dev,
                        OpenCL.clMemContext = ctx',
                        OpenCL.clMemEndianness = endianness,
                        OpenCL.clMemSize = size
                     }
                   prcPeer = OpenCLProc $ OpenCL.Proc {
                        OpenCL.clProcDevice = dev,
                        OpenCL.clProcContext = ctx'
                     }
                   linkPeer = OpenCLNetwork $ OpenCL.Network {
                        OpenCL.clLinkDevice = dev,
                        OpenCL.clLinkContext = ctx',
                        OpenCL.clLinkQueue = linkQueue
                     }
               proc <- registerProc prcPeer
               clMem <- registerMemory [proc] memPeer
               
               -- Add link to every registered host memory
               hostMems <- filter isHostMemory . currentMemories <$> get
               let neighbors m 
                     | m == clMem        = return hostMems
                     | m `elem` hostMems = return [clMem]
                     | otherwise         = return []

               net <- registerNetwork (NetworkPPP FullDuplex) neighbors linkPeer
               forM_ (clMem:hostMems) $ \m -> 
                  lift $ atomically $ modifyTVar (memoryNetworks m) (net:)

   return platforms


-- | Load host platform
loadHostPlatform :: PlatformConfig -> LIO ()
loadHostPlatform config = do
   numa <- lift $ Linux.loadNUMA (sysfsPath config)
   hostEndianness <- lift $ Generic.getMemoryEndianness
   forM_ (Linux.numaNodes numa) $ \node -> do
      let m = Linux.nodeMemory node
      (total,_) <- lift $ Linux.nodeMemoryStatus m
      let memPeer = HostMemory $ Host.Memory {
            Host.hostMemEndianness = hostEndianness,
            Host.hostMemSize = total
          }
          procs = fmap (HostProc . Host.Proc) (Linux.nodeCPUs node)
      procs' <- forM procs registerProc
      void (registerMemory procs' memPeer)


-- | Load the platform
loadPlatform :: PlatformConfig -> IO Platform
loadPlatform config = evalStateT loadPlatform_ initLoadState
   where
      loadPlatform_ :: StateT LoadState IO Platform
      loadPlatform_ = do
         
         loadHostPlatform config
         clPlatforms <- if enableOpenCL config 
            then loadOpenCLPlatform config
            else return []

         -- TODO: load other devices (CUDA...)

         memories <- reverse . currentMemories <$> get
         procs <- reverse . currentProcs <$> get
         networks <- reverse . currentNetworks <$> get

         return Platform {
            platformMemories = memories,
            platformNetworks = networks,
            platformProcs = procs,
            platformOpenCLPlatforms = clPlatforms
         }
