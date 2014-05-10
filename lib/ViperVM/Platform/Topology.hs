{-# LANGUAGE RecordWildCards #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Memory(..), Proc(..),
   Network(..), Duplex(..),
   isHostMemory,
   networkMemories, memoryNeighbors,
   ID
) where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)

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
data Network = PPPLink {
   pppLinkSource :: Memory,
   pppLinkTarget :: Memory,
   pppLinkDuplex :: Duplex,
   pppLinkPeer :: NetworkPeer
}

-- | Unique identifier
type ID = Int

instance Eq Memory where
   (==) a b = memoryId a == memoryId b

instance Ord Memory where
   compare a b = compare (memoryId a) (memoryId b)
  

-- | Network link direction
data Duplex = Simplex | HalfDuplex | FullDuplex

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False

-- | Retrieve memories interconnected by the network
networkMemories :: Network -> [Memory]
networkMemories net = case net of
   PPPLink {..} -> [pppLinkSource, pppLinkTarget]

-- | Return memories directly reachable through a network
memoryNeighbors :: Memory -> IO (Map Memory [Network])
memoryNeighbors source = do
   nets <- readTVarIO (memoryNetworks source)
   return $ Map.fromListWith (++) [(mem,[net]) | net <- nets, 
                                      mem <- networkMemories net, 
                                      mem /= source]
