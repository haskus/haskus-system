{-# LANGUAGE RecordWildCards #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Memory(..), Proc(..),
   Network(..), Duplex(..), NetworkType(..),
   isHostMemory, memoryNeighbors,
   ID
) where

import Control.Concurrent.STM
import Control.Applicative ((<$>))
import Data.Set (Set)
import Data.Ord (comparing)
import Data.Traversable (traverse)
import qualified Data.Set as Set

import ViperVM.Platform.Memory.Buffer (Buffer)
import ViperVM.Platform.Drivers
import ViperVM.STM.TSet


-- | Memory
data Memory = Memory {
   memoryPeer :: MemoryPeer,
   memoryProcs :: TSet Proc,
   memoryBuffers :: TSet Buffer,
   memoryNetworks :: TSet Network
}

instance Eq Memory where
   (==) a b = memoryPeer a == memoryPeer b

instance Ord Memory where
   compare = comparing memoryPeer


-- | A processor
data Proc = Proc {
   procPeer :: ProcPeer
}

instance Eq Proc where
   (==) a b = procPeer a == procPeer b

instance Ord Proc where
   compare = comparing procPeer


-- | Networks interconnecting memories
data Network = Network {
   networkType :: NetworkType,
   networkNeighbors :: Memory -> STM (Set Memory),
   networkPeer :: NetworkPeer
}

instance Eq Network where
   (==) a b = networkPeer a == networkPeer b

instance Ord Network where
   compare = comparing networkPeer

-- | Unique identifier
type ID = Int

  

-- | Network link direction
data Duplex = Simplex | HalfDuplex | FullDuplex deriving (Show)

-- | Network type
data NetworkType = NetworkPPP Duplex deriving (Show)

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False

memoryNeighbors :: Memory -> STM (Set Memory)
memoryNeighbors mem = do
   nets <- Set.toList <$> readTVar (memoryNetworks mem)
   Set.unions <$> traverse (`networkNeighbors` mem) nets
