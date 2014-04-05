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

import qualified ViperVM.Arch.OpenCL as CL
import qualified ViperVM.Arch.GenericHost as Generic
import qualified ViperVM.Platform.CPU as CPU
import ViperVM.Platform.Types
import ViperVM.Platform.Config
import ViperVM.Platform.Network
import qualified ViperVM.Platform.Host.SysFS as SysFS

-- | State used during platform loading
data LoadState = LoadState {
   currentProcID :: ID,
   currentMemID :: ID,
   currentNetworkID :: ID,
   currentMemories :: [Memory],
   currentNetworks :: [Network],
   currentProcs :: [Proc]
}

-- | Initial loading state
initLoadState :: LoadState
initLoadState = LoadState {
   currentProcID = 0,
   currentNetworkID = 0,
   currentMemID = 0,
   currentMemories = [],
   currentNetworks = [],
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

-- | Get new network ID
newNetworkId :: Monad m => StateT LoadState m ID
newNetworkId = do
   curr <- get
   let x = currentNetworkID curr
   put (curr { currentNetworkID = x+1 })
   return x

-- | Register a new memory
registerMemory :: [Proc] -> MemoryPeer -> StateT LoadState IO Memory
registerMemory procs peer = do
   memId <- newMemId
   mem <- lift $ Memory memId procs peer <$> newTVarIO [] <*> newTVarIO []
   curr <- get
   put (curr { currentMemories = mem : currentMemories curr})
   return mem

-- | Register a new point-to-point link
registerPPPLink :: Memory -> Memory -> Duplex -> PPPLinkPeer -> StateT LoadState IO Network
registerPPPLink src dst duplex peer = do
   netId <- newNetworkId
   let net = PPPLink netId src dst duplex peer
   curr <- get
   put (curr { currentNetworks = net : currentNetworks curr})
   return net

-- | Register a new proc
registerProc :: ProcPeer -> StateT LoadState IO Proc
registerProc peer = do
   pId <- newProcId
   let proc = Proc pId peer
   curr <- get
   put (curr { currentProcs = proc : currentProcs curr})
   return proc


-- | Associate networks to memories
associateNetworksToMemories :: StateT LoadState IO ()
associateNetworksToMemories = do
   nets <- currentNetworks <$> get

   forM_ nets $ \net ->
      forM_ (networkMemories net) $ \mem ->
         lift $ atomically $ modifyTVar (memoryNetworks mem) (net:)
      


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
               endianness <- lift $ CL.getDeviceEndianness dev
               size <- lift $ CL.getDeviceGlobalMemSize' dev

               -- FIXME: some implementations do not support profiling or out-of-order.
               -- We should avoid crashing here
               let props = [CL.CL_QUEUE_OUT_OF_ORDER, CL.CL_QUEUE_PROFILING]
               linkQueue <- lift $ CL.createCommandQueue' ctx' dev props

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
                   linkPeer = OpenCLLink {
                        clLinkDevice = dev,
                        clLinkContext = ctx',
                        clLinkQueue = linkQueue
                     }
               proc <- registerProc prcPeer
               clMem <- registerMemory [proc] memPeer
               
               -- Add link to every registered host memory
               hostMems <- filter isHostMemory . currentMemories <$> get
               forM_ hostMems $ \hostMem -> registerPPPLink clMem hostMem FullDuplex linkPeer

   return platforms


-- | Load host platform
loadHostPlatform :: PlatformConfig -> StateT LoadState IO ()
loadHostPlatform config = do
   numa <- lift $ CPU.loadNUMA (sysfsPath config)
   hostEndianness <- lift $ Generic.getMemoryEndianness
   forM_ (CPU.numaNodes numa) $ \node -> do
      let m = CPU.nodeMemory node
      (total,_) <- lift $ CPU.nodeMemoryStatus m
      let memPeer = HostMemory {
            hostMemEndianness = hostEndianness,
            hostMemSize = total
          }
          procs = fmap CPUProc (SysFS.toList (CPU.nodeCPUs node))
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

         associateNetworksToMemories
         
         memories <- reverse . currentMemories <$> get
         procs <- reverse . currentProcs <$> get
         networks <- reverse . currentNetworks <$> get

         return Platform {
            platformMemories = memories,
            platformNetworks = networks,
            platformProcs = procs,
            platformOpenCLPlatforms = clPlatforms
         }
