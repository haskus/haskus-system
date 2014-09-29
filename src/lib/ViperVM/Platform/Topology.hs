{-# LANGUAGE RecordWildCards, PatternSynonyms, TupleSections #-}

-- | Platform topology
module ViperVM.Platform.Topology 
   ( Memory(..)
   , Proc(..)
   , Network(..)
   , Duplex(..)
   , NetworkType(..)
   , BufferData(..)
   , MemoryBuffer(..)
   , pattern MemoryBufferData
   , isHostMemory
   , memoryNeighbors
   , memoryNetNeighbors
   )
where

import Control.Concurrent.STM
import Control.Applicative ((<$>))
import Data.Set (Set)
import Data.Ord (comparing)
import Data.Traversable (traverse,forM)
import qualified Data.Set as Set

import ViperVM.Platform.Memory.Buffer (Buffer)
import ViperVM.Platform.Memory.Data (Data)
import ViperVM.Platform.Drivers
import ViperVM.Platform.NetworkBench
import ViperVM.STM.TSet
import ViperVM.STM.TMap


-- | Memory
data Memory = Memory 
  { memoryPeer       :: MemoryPeer
  , memoryProcs      :: TSet Proc
  , memoryBuffers    :: TSet Buffer
  , memoryNetworks   :: TSet Network
  }

instance Eq Memory where
   (==) a b = memoryPeer a == memoryPeer b

instance Ord Memory where
   compare = comparing memoryPeer


-- | A processor
data Proc = Proc 
  { procPeer :: ProcPeer
  }

instance Eq Proc where
   (==) a b = procPeer a == procPeer b

instance Ord Proc where
   compare = comparing procPeer


-- | Networks interconnecting memories
data Network = Network 
  { networkPeer :: NetworkPeer
  , networkType :: NetworkType
  , networkNeighbors :: Memory -> STM (Set Memory)
  , networkBenchs :: TMap (Memory,Memory) (TSet NetworkBenchResult)
  }

instance Eq Network where
   (==) a b = networkPeer a == networkPeer b

instance Ord Network where
   compare = comparing networkPeer

-- | A data associated with its buffer
data BufferData = BufferData 
   { bufferDataBuffer :: MemoryBuffer
   , bufferDataData :: Data
   }

pattern MemoryBufferData a b c = BufferData (MemoryBuffer a b) c

-- | Memory buffer
data MemoryBuffer = MemoryBuffer 
   { memoryBufferMemory :: Memory
   , memoryBufferBuffer :: Buffer
   } deriving (Eq)
  
-- | Network link direction
data Duplex
   = Simplex 
   | HalfDuplex 
   | FullDuplex 
   deriving (Show)

-- | Network type
data NetworkType = NetworkPPP Duplex deriving (Show)

-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   HostMemory {} -> True
   _ -> False

-- | Memory neighbor memory nodes
memoryNeighbors :: Memory -> STM (Set Memory)
memoryNeighbors mem = do
   nets <- Set.toList <$> readTVar (memoryNetworks mem)
   Set.unions <$> traverse (`networkNeighbors` mem) nets

-- | Memory neighbor memory nodes + interconnecting network
memoryNetNeighbors :: Memory -> STM (Set (Network,Memory))
memoryNetNeighbors mem = do
   nets <- Set.toList <$> readTVar (memoryNetworks mem)
   ss <- forM nets $ \net -> do
      ms <- networkNeighbors net mem
      return . Set.fromList . fmap (net,) . Set.toList $ ms
   return $ Set.unions ss
