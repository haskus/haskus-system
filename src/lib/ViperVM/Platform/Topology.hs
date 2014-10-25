{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Platform topology
module ViperVM.Platform.Topology 
   ( Memory(..)
   , MemoryUID
   , memoryUID
   , Proc(..)
   , Network(..)
   , NetworkUID
   , networkUID
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
import Data.Ord (comparing)
import Data.Traversable (traverse,forM)
import Data.Hashable

import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.Platform.Memory.Buffer (Buffer)
import ViperVM.Platform.Memory.Data (Data)
import ViperVM.Platform.Drivers
import ViperVM.Platform.NetworkBench
import ViperVM.STM.TSet as TSet
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

newtype MemoryUID = MemoryUID String deriving (Read, Show, Eq, Ord, Hashable)

-- | Memory unique identifier (not stable between different program executions)
memoryUID :: Memory -> MemoryUID
memoryUID mem = MemoryUID $ case memoryPeer mem of
   Peer.OpenCLMemory m -> OpenCL.clMemUID m
   Peer.HostMemory m -> Host.hostMemUID m

instance Hashable Memory where
   hashWithSalt salt m = hashWithSalt salt (memoryUID m)


-- | A processor
data Proc = Proc 
  { procPeer :: ProcPeer
  }

instance Eq Proc where
   (==) a b = procPeer a == procPeer b

instance Ord Proc where
   compare = comparing procPeer

newtype ProcUID = ProcUID String deriving (Read, Show, Eq, Ord, Hashable)

-- | Processor unique identifier (not stable between different program executions)
procUID :: Proc -> ProcUID
procUID mem = ProcUID $ case procPeer mem of
   Peer.OpenCLProc m -> OpenCL.clProcUID m
   Peer.HostProc m -> Host.hostProcUID m


instance Hashable Proc where
   hashWithSalt salt p = hashWithSalt salt (procUID p)

-- | Networks interconnecting memories
data Network = Network 
  { networkPeer :: NetworkPeer
  , networkType :: NetworkType
  , networkNeighbors :: Memory -> STM (TSet Memory)
  , networkBenchs :: TMap (Memory,Memory) (TSet NetworkBenchResult)
  }

instance Eq Network where
   (==) a b = networkPeer a == networkPeer b

instance Ord Network where
   compare = comparing networkPeer

newtype NetworkUID = NetworkUID String deriving (Read, Show, Eq, Ord,Hashable)

-- | Network unique identifier (not stable between different program executions)
networkUID :: Network -> NetworkUID
networkUID mem = NetworkUID $ case networkPeer mem of
   Peer.OpenCLNetwork m -> OpenCL.clNetUID m

instance Hashable Network where
   hashWithSalt salt n = hashWithSalt salt (networkUID n)

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
memoryNeighbors :: Memory -> STM (TSet Memory)
memoryNeighbors mem = do
   nets <- TSet.toList (memoryNetworks mem)
   TSet.unions =<< traverse (`networkNeighbors` mem) nets

-- | Memory neighbor memory nodes + interconnecting network
memoryNetNeighbors :: Memory -> STM (TSet (Network,Memory))
memoryNetNeighbors mem = do
   nets <- TSet.toList (memoryNetworks mem)
   ss <- forM nets $ \net -> do
      ms <- networkNeighbors net mem
      TSet.map (net,) ms
   TSet.unions ss
