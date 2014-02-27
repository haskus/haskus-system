{-# LANGUAGE RecordWildCards #-}

-- | Module to get info strings from platform objects
module ViperVM.Platform.PlatformInfo (
   memoryInfo, procInfo, networkInfo
) where

import Control.Concurrent.STM (atomically, readTVar)

import ViperVM.Platform.Types
import ViperVM.Platform.Platform

import Data.Word (Word64)
import Text.Printf

-- | Return memory info string
memoryInfo :: Memory -> IO String
memoryInfo mem = do
   buffers <- atomically $ readTVar (memoryBuffers mem)
   let
      str = printf fmt mid sizeGB typ endian nbuffers
      fmt = "Memory %d - %.2f GB - %s - %s - %d buffer(s)"
      endian = if memoryEndianness mem == LittleEndian then "Little endian" else "Big endian"
      mid = memoryId mem
      size = fromIntegral (memorySize mem) :: Double
      sizeGB = size / fromIntegral (1024*1024*1024 :: Word64)
      nbuffers = length buffers
      typ = case memoryPeer mem of
         OpenCLMemory {} -> "OpenCL"
         HostMemory {} -> "Host"
         CUDAMemory {} -> "CUDA"
         DiskMemory {} -> "Disk"
   return str

-- | Return proc info string
procInfo :: Proc -> IO String
procInfo proc = return (printf fmt pid typ)
   where
      fmt = "Proc %d - %s"
      pid = procId proc
      typ = case procPeer proc of
         OpenCLProc {} -> "OpenCL"
         CPUProc {} -> "CPU"

-- | Return network info string
networkInfo :: Network -> IO String
networkInfo net = return (printf fmt nid desc)
   where
      fmt = "Network %d - %s"
      nid = networkId net
      desc :: String
      desc = case net of
         PPPLink {..} -> printf "%s PPPLink (%d %s %d)" typ src duplex dst where
            src = memoryId pppLinkSource
            dst = memoryId pppLinkTarget
            duplex = case pppLinkDuplex of
               Simplex -> "->"
               HalfDuplex -> "<->"
               FullDuplex -> "<=>"
            typ = case pppLinkPeer of
               OpenCLLink {} -> "OpenCL"
