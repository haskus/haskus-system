{-# LANGUAGE RecordWildCards #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Memory(..), Proc(..),
   Network(..), Duplex(..), NetworkType(..),
   isHostMemory,
   ID
) where

import Control.Concurrent.STM

import ViperVM.Platform.Memory.Buffer (Buffer)
import ViperVM.Platform.Drivers

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
data Network = Network {
   networkType :: NetworkType,
   networkNeighbors :: Memory -> STM [Memory],
   networkPeer :: NetworkPeer
}

-- | Unique identifier
type ID = Int

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

instance Ord Memory where
   compare a b = compare (memoryId a) (memoryId b)
  

-- | Network link direction
data Duplex = Simplex | HalfDuplex | FullDuplex deriving (Show)

-- | Network type
data NetworkType = NetworkPPP Duplex deriving (Show)

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False
