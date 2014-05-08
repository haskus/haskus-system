{-# LANGUAGE RecordWildCards #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Platform(..), ID,
   Memory(..), MemoryPeer(..),
   Buffer(..), BufferPeer(..),
   BufferSize,
   Network(..), PPPLinkPeer(..), Duplex(..),
   TransferError(..),
   Proc(..), ProcPeer(..),
   isHostMemory, networkId
) where

import Control.Concurrent.STM (TVar)
import Foreign.Ptr (Ptr)
import Data.Word (Word,Word64)

import qualified ViperVM.Arch.OpenCL.All as CL
import ViperVM.Arch.Common.Endianness

-- | Platform
data Platform = Platform {
   platformMemories :: [Memory],
   platformNetworks :: [Network],
   platformProcs :: [Proc],
   -- OpenCL specific
   platformOpenCLPlatforms :: [CL.Platform]
}


-- | Unique identifier
type ID = Int

--------------------------------------------------------------
-- Memories
--------------------------------------------------------------

-- | Memory
data Memory = Memory {
   memoryId :: ID,
   memoryProcs :: [Proc],
   memoryPeer :: MemoryPeer,
   memoryBuffers :: TVar [Buffer],
   memoryNetworks :: TVar [Network]
}

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

instance Ord Memory where
   compare a b = compare (memoryId a) (memoryId b)

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

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False


-- | Memory buffer
data Buffer = Buffer {
   bufferMemory :: Memory,
   bufferSize :: BufferSize,
   bufferPeer :: BufferPeer
} deriving (Eq)

-- | Backend specific buffer fields
data BufferPeer = 
     HostBuffer (Ptr ())
   | CUDABuffer
   | OpenCLBuffer CL.Device CL.Context CL.Mem
   | DiskBuffer
   deriving (Eq)

type BufferSize = Word64  -- ^ Size of a buffer in bytes

--------------------------------------------------------------
-- Processors
--------------------------------------------------------------

-- | A processor
data Proc = Proc {
   procId :: ID,
   procPeer :: ProcPeer
}

-- | Backend specific processor fields
data ProcPeer =
     CPUProc {
         cpuIndex :: Word
     }
   | OpenCLProc {
         clProcDevice :: CL.Device,
         clProcContext :: CL.Context
     }

--------------------------------------------------------------
-- Networking / interconnexion networks
--------------------------------------------------------------

-- | Network link direction
data Duplex = Simplex | HalfDuplex | FullDuplex

-- | Networks interconnecting memories
data Network =
   -- | Point-to-point link
   PPPLink {
      pppLinkId :: ID,
      pppLinkSource :: Memory,
      pppLinkTarget :: Memory,
      pppLinkDuplex :: Duplex,
      pppLinkPeer :: PPPLinkPeer
   }

-- | Backend specific fields for point-to-point links
data PPPLinkPeer = 
     OpenCLLink {
         clLinkDevice :: CL.Device,
         clLinkContext :: CL.Context,
         clLinkQueue :: CL.CommandQueue
     }

networkId :: Network -> ID
networkId (PPPLink {..}) = pppLinkId

--------------------------------------------------------------
-- Errors
--------------------------------------------------------------

-- | Region transfer error
data TransferError =
     ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown
   deriving (Show,Eq)
