module ViperVM.Platform.MemoryPeer (
   MemoryPeer(..)
) where

import Data.Word (Word64)

import ViperVM.Arch.Common.Endianness
import qualified ViperVM.Arch.OpenCL.All as CL

-- | Backend specific memory fields
data MemoryPeer =
     HostMemory {
         hostMemSize :: Word64,
         hostMemEndianness :: Endianness
     }
   | OpenCLMemory {
         clMemLibrary :: CL.Library,
         clMemDevice :: CL.Device,
         clMemContext :: CL.Context,
         clMemEndianness :: Endianness,
         clMemSize :: Word64
     }
