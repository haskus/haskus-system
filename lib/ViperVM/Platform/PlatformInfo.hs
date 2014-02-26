module ViperVM.Platform.PlatformInfo (
   memoryInfo, procInfo
) where

import Control.Concurrent.STM (atomically, readTVar)

import ViperVM.Platform.Types
import ViperVM.Platform.Platform

import Data.Word (Word64)
import Text.Printf

memoryInfo :: Memory -> IO String
memoryInfo mem = do
   buffers <- atomically $ readTVar (memoryBuffers mem)
   let
      fmt = "Memory %d - %.2f GB - %s - %s - %d buffer(s)"
      str = printf fmt mid sizeGB typ endian nbuffers
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

procInfo :: Proc -> IO String
procInfo proc = do
   let
      fmt = "Proc %d - %s"
      str = printf fmt pid typ
      pid = procId proc
      typ = case procPeer proc of
         OpenCLProc {} -> "OpenCL"
         CPUProc {} -> "CPU"

   return str
