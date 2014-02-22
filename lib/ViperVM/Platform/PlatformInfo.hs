module ViperVM.Platform.PlatformInfo where

import Control.Concurrent.STM (atomically, readTVar)

import ViperVM.Platform.Types
import ViperVM.Platform.Platform

import Data.Word (Word64)
import Text.Printf

memoryInfo :: Memory -> IO String
memoryInfo mem = do
   buffers <- atomically $ readTVar (memoryBuffers mem)
   let
      fmt = "Memory %d (%.2f GB):\n  - %d buffer(s)"
      str = printf fmt mid sizeGB nbuffers
      mid = memoryId mem
      size = fromIntegral (memorySize mem) :: Double
      sizeGB = size / fromIntegral (1024*1024*1024 :: Word64)
      nbuffers = length buffers

   return str
