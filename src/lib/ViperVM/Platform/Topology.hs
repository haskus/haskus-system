{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

-- | Platform topology
module ViperVM.Platform.Topology (
   Memory(..), Proc(..),
   Network(..), Duplex(..), NetworkType(..),
   BufferData(..), MemoryBuffer(..),
   pattern MemoryBufferData,
   isHostMemory, memoryNeighbors
) where

import Control.Concurrent.STM
import Control.Applicative ((<$>))
import Data.Set (Set)
import Data.Ord (comparing)
import Data.Traversable (traverse)
import qualified Data.Set as Set

import ViperVM.Platform.Memory.Buffer (Buffer)
import ViperVM.Platform.Memory.Data (Data)
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
   networkPeer :: NetworkPeer,
   networkType :: NetworkType,
   networkNeighbors :: Memory -> STM (Set Memory)
}

instance Eq Network where
   (==) a b = networkPeer a == networkPeer b

instance Ord Network where
   compare = comparing networkPeer

-- | A data associated with its buffer
data BufferData = BufferData {
   bufferDataBuffer :: MemoryBuffer,
   bufferDataData :: Data
}

pattern MemoryBufferData a b c = BufferData (MemoryBuffer a b) c

-- | Memory buffer
data MemoryBuffer = MemoryBuffer Memory Buffer deriving (Eq)
  
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
