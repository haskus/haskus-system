-- | Memory related module
module ViperVM.Platform.Memory (
   memoryEndianness, memorySize
) where

import Data.Word (Word64)

import ViperVM.Arch.Common.Endianness
import ViperVM.Platform.Topology
import ViperVM.Platform.MemoryPeer

-- | Indicate the endianness of a memory
memoryEndianness :: Memory -> Endianness
memoryEndianness mem = case memoryPeer mem of
   m@(OpenCLMemory {}) -> clMemEndianness m
   m@(HostMemory {}) -> hostMemEndianness m

-- | Return total memory size
memorySize :: Memory -> Word64
memorySize mem = case memoryPeer mem of
   m@(OpenCLMemory {}) -> clMemSize m
   m@(HostMemory {}) -> hostMemSize m
