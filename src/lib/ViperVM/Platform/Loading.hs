-- | Platform loading module
module ViperVM.Platform.Loading
   ( loadPlatform
   ) 
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM,when)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Data.Traversable (forM)
import qualified ListT

import qualified ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.Arch.GenericHost.Memory as Generic
import qualified ViperVM.Arch.Linux.Numa as Linux

import ViperVM.Platform.Drivers
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.Platform.Host
import ViperVM.Platform.Types 
   ( Memory(..)
   , Network(..)
   , NetworkType(..)
   , Duplex(..)
   , Proc(..)
   )
import ViperVM.Platform.Config
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet
import qualified ViperVM.STM.TMap as TMap

-- | Init a memory
memoryInit :: MemoryPeer -> IO Memory
memoryInit peer = Memory peer 
   <$> atomically TSet.empty 
   <*> atomically TSet.empty
   <*> atomically TSet.empty

-- | Init a network
networkInit :: NetworkType -> (Memory -> STM (TSet Memory)) -> NetworkPeer -> IO Network
networkInit typ neighbors peer = Network peer typ neighbors <$> atomically TMap.empty

-- | Init a processor
procInit :: ProcPeer -> IO Proc
procInit peer = return (Proc peer)


-- | Load OpenCL platform
loadOpenCLPlatform :: PlatformConfig -> TSet Memory -> IO ()
loadOpenCLPlatform config hostMems = do
   lib <- CL.loadOpenCL (libraryOpenCL config)
   platforms <- CL.getPlatforms lib

   forM_ platforms $ \clpf -> do
      -- Filter OpenCL devices
      let cpuFilter = if enableOpenCLCPUs config
            then const (return True)
            else fmap (notElem CL.CL_DEVICE_TYPE_CPU) . CL.getDeviceType'

      devices <- filterM (filterOpenCLDevices config) =<< filterM cpuFilter =<< CL.getPlatformDevices clpf

      -- Load devices
      forM_ devices $ \dev -> do
         context <- CL.createContext clpf [dev]

         case context of
            Left err -> error ("Invalid context: " ++ show err)
            Right ctx' -> do
               endianness <- CL.getDeviceEndianness dev
               size <- CL.getDeviceGlobalMemSize' dev

               -- some implementations do not support profiling or out-of-order
               supportedProps <- CL.getDeviceQueueProperties' dev
               let
                   props      = [CL.CL_QUEUE_OUT_OF_ORDER , CL.CL_QUEUE_PROFILING]
                   validProps = filter (`elem` supportedProps) props
               linkQueue <- CL.createCommandQueue' ctx' dev validProps

               let memPeer = OpenCLMemory OpenCL.Memory {
                        OpenCL.clMemLibrary = lib,
                        OpenCL.clMemDevice = dev,
                        OpenCL.clMemContext = ctx',
                        OpenCL.clMemEndianness = endianness,
                        OpenCL.clMemSize = size
                     }
                   prcPeer = OpenCLProc OpenCL.Proc {
                        OpenCL.clProcDevice = dev,
                        OpenCL.clProcContext = ctx'
                     }
                   linkPeer = OpenCLNetwork OpenCL.Network {
                        OpenCL.clLinkDevice = dev,
                        OpenCL.clLinkContext = ctx',
                        OpenCL.clLinkQueue = linkQueue
                     }
               clMem <- memoryInit memPeer
               proc <- procInit prcPeer

               -- Associate processor to memory
               atomically $ TSet.insert proc (memoryProcs clMem)
               
               -- Add link to every registered host memory
               let neighbors m =
                     if m == clMem
                        then return hostMems
                        else do
                           tst <- TSet.member m hostMems
                           if tst
                              then TSet.singleton clMem
                              else TSet.empty

               net <- networkInit (NetworkPPP FullDuplex) neighbors linkPeer
               atomically $ do
                  ListT.traverse_ (TSet.insert net . memoryNetworks) (TSet.stream hostMems)
                  TSet.insert net (memoryNetworks clMem)



-- | Load host platform
loadHostPlatform :: PlatformConfig -> TSet Memory -> IO ()
loadHostPlatform config hostMems = do
   numa <- Linux.loadNUMA (sysfsPath config)
   hostEndianness <- Generic.getMemoryEndianness

   mems <- forM (Linux.numaNodes numa) $ \node -> do
      let 
         m = Linux.nodeMemory node
         nid = Linux.nodeId node
      (total,_) <- Linux.nodeMemoryStatus m
      let memPeer = HostMemory Host.Memory {
            Host.hostMemNode = nid,
            Host.hostMemEndianness = hostEndianness,
            Host.hostMemSize = total
          }
          procPeers = fmap (HostProc . Host.Proc nid) (Linux.nodeCPUs node)

      mem <- memoryInit memPeer
      procs <- mapM procInit procPeers

      -- Associate processors to memory
      atomically $ forM_ procs $ \p -> TSet.insert p (memoryProcs mem)

      return mem
         
   -- Init platform host memories
   atomically $ forM_ mems (`TSet.insert` hostMems)
         


-- | Load the platform
loadPlatform :: PlatformConfig -> IO Host
loadPlatform config = do
   
   hostMems <- atomically TSet.empty

   loadHostPlatform config hostMems
         
   when (enableOpenCL config) $
      loadOpenCLPlatform config hostMems

   -- TODO: load other devices (CUDA...)

   return (newHost hostMems)
