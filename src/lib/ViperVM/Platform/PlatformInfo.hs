{-# LANGUAGE RecordWildCards #-}

-- | Module to get info strings from platform objects
module ViperVM.Platform.PlatformInfo (
   memoryInfo, procInfo, networkInfo
) where

import Control.Concurrent.STM (atomically, readTVar)

import ViperVM.Arch.Common.Endianness
import ViperVM.Platform.Topology
import ViperVM.Platform.Drivers
import ViperVM.Platform.Memory (memoryEndianness, memorySize)

import Data.Word (Word64)
import qualified Data.Set as Set
import Text.Printf

-- | Return memory info string
memoryInfo :: Memory -> IO String
memoryInfo mem = do
   buffers <- atomically $ readTVar (memoryBuffers mem)
   let
      str = printf fmt typ sizeGB endian nbuffers
      fmt = "Memory - %s - %.2f GB - %s - %d buffer(s)"
      endian = if memoryEndianness mem == LittleEndian then "Little endian" else "Big endian"
      size = fromIntegral (memorySize mem) :: Double
      sizeGB = size / fromIntegral (1024*1024*1024 :: Word64)
      nbuffers = Set.size buffers
      typ = case memoryPeer mem of
         OpenCLMemory {} -> "OpenCL"
         HostMemory {} -> "Host"
   return str

-- | Return proc info string
procInfo :: Proc -> IO String
procInfo proc = return (printf fmt typ)
   where
      fmt = "Proc - %s"
      typ = case procPeer proc of
         OpenCLProc {} -> "OpenCL"
         HostProc {} -> "CPU"

-- | Return network info string
networkInfo :: Network -> IO String
networkInfo net = return (printf fmt desc)
   where
      fmt = "Network - %s"
      desc :: String
      desc = printf "%s (%s)" drv (show (networkType net))
         where
            drv = case networkPeer net of
               OpenCLNetwork {} -> "OpenCL"
