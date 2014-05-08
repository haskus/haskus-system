{-# LANGUAGE RecordWildCards #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Memory(..), Proc(..),
   Buffer(..), BufferSize,
   Network(..), Duplex(..),
   isHostMemory,
   ID
) where

import Control.Concurrent.STM (TVar)
import Data.Word (Word64)

import ViperVM.Platform.MemoryPeer
import ViperVM.Platform.ProcPeer
import ViperVM.Platform.NetworkPeer
import ViperVM.Platform.BufferPeer

-- | Memory
data Memory = Memory {
   memoryId :: ID,
   memoryProcs :: [Proc],
   memoryPeer :: MemoryPeer,
   memoryBuffers :: TVar [Buffer],
   memoryNetworks :: TVar [Network]
}


-- | A processor
data Proc = Proc {
   procId :: ID,
   procPeer :: ProcPeer
}


-- | Networks interconnecting memories
data Network =
   -- | Point-to-point link
   PPPLink {
      pppLinkSource :: Memory,
      pppLinkTarget :: Memory,
      pppLinkDuplex :: Duplex,
      pppLinkPeer :: PPPLinkPeer
   }


-- | Memory buffer
data Buffer = Buffer {
   bufferMemory :: Memory,
   bufferSize :: BufferSize,
   bufferPeer :: BufferPeer
} deriving (Eq)


-- | Unique identifier
type ID = Int

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

instance Ord Memory where
   compare a b = compare (memoryId a) (memoryId b)
  

-- | Size of a buffer in bytes
type BufferSize = Word64

-- | Network link direction
data Duplex = Simplex | HalfDuplex | FullDuplex

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False
