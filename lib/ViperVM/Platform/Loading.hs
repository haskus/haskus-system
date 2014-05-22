-- | Platform loading module
module ViperVM.Platform.Loading (
   loadPlatform
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM,when)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Data.Traversable (forM)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.Arch.GenericHost.All as Generic
import qualified ViperVM.Arch.Linux.Numa as Linux

import ViperVM.Platform.Drivers
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.Platform.Platform
import ViperVM.Platform.Topology
import ViperVM.Platform.Config

-- | Register a new memory
memoryInit :: MemoryPeer -> IO Memory
memoryInit peer = Memory peer 
   <$> newTVarIO Set.empty 
   <*> newTVarIO Set.empty
   <*> newTVarIO Set.empty

-- | Register a new network 
networkInit :: NetworkType -> (Memory -> STM (Set Memory)) -> NetworkPeer -> IO Network
networkInit typ neighbors peer = return (Network typ neighbors peer)

-- | Init a processor
procInit :: ProcPeer -> IO Proc
procInit peer = return (Proc peer)


-- | Load OpenCL platform
loadOpenCLPlatform :: PlatformConfig -> Platform -> IO ()
loadOpenCLPlatform config pf = do
   lib <- CL.loadOpenCL (libraryOpenCL config)
   platforms <- CL.getPlatforms lib

   forM_ platforms $ \clpf -> do
      devices <- filterM (filterOpenCLDevices config) =<< CL.getPlatformDevices clpf
      forM_ devices $ \dev -> do
         context <- CL.createContext clpf [dev]

         case context of
            Left err -> error ("Invalid context: " ++ show err)
            Right ctx' -> do
               endianness <- CL.getDeviceEndianness dev
               size <- CL.getDeviceGlobalMemSize' dev

               -- FIXME: some implementations do not support profiling or out-of-order.
               -- We should avoid crashing here
               let props = [CL.CL_QUEUE_OUT_OF_ORDER, CL.CL_QUEUE_PROFILING]
               linkQueue <- CL.createCommandQueue' ctx' dev props

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
               clMem <- memoryInit memPeer
               proc <- procInit prcPeer

               -- Associate processor to memory
               atomically $ writeTVar (memoryProcs clMem) (Set.singleton proc)
               
               -- Add link to every registered host memory
               hostMems <- atomically $ readTVar (platformHostMemories pf)
               let neighbors m 
                     | m == clMem              = hostMems
                     | m `Set.member` hostMems = Set.singleton clMem
                     | otherwise               = Set.empty
                   neighbors' = return . neighbors

               net <- networkInit (NetworkPPP FullDuplex) neighbors' linkPeer
               atomically $ forM_ (Set.insert clMem hostMems) $ \m -> 
                  modifyTVar (memoryNetworks m) (Set.insert net)



-- | Load host platform
loadHostPlatform :: PlatformConfig -> Platform -> IO ()
loadHostPlatform config pf = do
   numa <- Linux.loadNUMA (sysfsPath config)
   hostEndianness <- Generic.getMemoryEndianness

   mems <- forM (Linux.numaNodes numa) $ \node -> do
      let 
         m = Linux.nodeMemory node
         nid = Linux.nodeId node
      (total,_) <- Linux.nodeMemoryStatus m
      let memPeer = HostMemory $ Host.Memory {
            Host.hostMemNode = nid,
            Host.hostMemEndianness = hostEndianness,
            Host.hostMemSize = total
          }
          procPeers = fmap (HostProc . Host.Proc nid) (Linux.nodeCPUs node)

      mem <- memoryInit memPeer
      procs <- Set.fromList <$> mapM procInit procPeers

      -- Associate processors to memory
      atomically (writeTVar (memoryProcs mem) procs)

      return mem
         
   -- Init platform host memories
   atomically $ writeTVar (platformHostMemories pf) (Set.fromList mems)
         


-- | Load the platform
loadPlatform :: PlatformConfig -> IO Platform
loadPlatform config = do
   
   pf <- Platform <$> newTVarIO Set.empty

   loadHostPlatform config pf
         
   when (enableOpenCL config) $
      loadOpenCLPlatform config pf

   -- TODO: load other devices (CUDA...)

   return pf
