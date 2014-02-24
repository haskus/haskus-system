-- | Platform types
module ViperVM.Platform.Types (
   ID,
   Endianness(..),
   Memory(..), MemoryPeer(..),
   Buffer(..), BufferPeer(..),
   BufferSize,
   Link(..), LinkPeer(..),
   AllocError(..), TransferError(..),
   Proc(..), ProcPeer(..)
) where

import Control.Concurrent.STM (TVar)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)

import qualified ViperVM.Platform.OpenCL as CL

-- | Memory endianness
data Endianness = LittleEndian | BigEndian deriving (Eq,Show)

-- | Unique identifier
type ID = Int

-- | Memory
data Memory = Memory {
   memoryId :: ID,
   memoryProcs :: [Proc],
   memoryPeer :: MemoryPeer,
   memoryBuffers :: TVar [Buffer]
}

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

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
   | CUDAMemory
   | DiskMemory

-- | A link between two memories
data Link = Link {
   linkId :: ID,
   linkPeer :: LinkPeer
}

instance Eq Link where 
   (==) a b = linkId a == linkId b

-- | Backend specific link
data LinkPeer =
     OpenCLLink CL.Library CL.Device CL.CommandQueue


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

-- | Region transfer error
data TransferError =
     ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown
   deriving (Show,Eq)

-- | Buffer allocation error
data AllocError = 
     ErrAllocOutOfMemory
   | ErrAllocUnknown
   deriving (Show,Eq)


-- | A processor
data Proc = Proc {
   procID :: ID,
   procPeer :: ProcPeer
}

-- | Backend specific processor fields
data ProcPeer =
     CPUProc
   | OpenCLProc {
         clProcDevice :: CL.Device,
         clProcContext :: CL.Context
     }
