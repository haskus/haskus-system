-- | Module to get info strings from platform objects
module ViperVM.Platform.PlatformInfo
   ( memoryInfo
   , procInfo
   , networkInfo
   )
where

import ViperVM.Format.Binary.Endianness
import ViperVM.Platform.Types (Memory(..), Proc(..), Network(..))
import ViperVM.Platform.Drivers
import qualified ViperVM.Platform.Drivers.Host as H
import ViperVM.Platform.Memory (memoryEndianness, memorySize)

import Data.Word (Word64)
import Text.Printf

-- | Return memory info string
memoryInfo :: Memory -> String
memoryInfo mem = str
   where
      str = printf fmt typ sizeGB endian
      fmt = "Memory - %s - %.2f GB - %s"
      endian = if memoryEndianness mem == LittleEndian then "Little endian" else "Big endian"
      size = fromIntegral (memorySize mem) :: Double
      sizeGB = size / fromIntegral (1024*1024*1024 :: Word64)
      typ = case memoryPeer mem of
         OpenCLMemory {} -> "OpenCL"
         HostMemory {}   -> "Host"

-- | Return proc info string
procInfo :: Proc -> String
procInfo proc = printf fmt typ
   where
      fmt = "Proc - %s"
      typ = case procPeer proc of
         OpenCLProc {} -> "OpenCL"
         HostProc p    -> "CPU - " ++ H.hostProcModel p

-- | Return network info string
networkInfo :: Network -> String
networkInfo net = printf fmt desc
   where
      fmt = "Network - %s"
      desc :: String
      desc = printf "%s (%s)" drv (show (networkType net))
         where
            drv = case networkPeer net of
               OpenCLNetwork {} -> "OpenCL"
