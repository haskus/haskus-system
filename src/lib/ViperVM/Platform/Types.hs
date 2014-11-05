{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Types used in Platform
module ViperVM.Platform.Types
   ( 
   -- * Memory
     Memory(..)
   , MemoryUID
   , memoryUID
   -- ** Buffer
   , Buffer(..)
   , BufferID
   , bufferUID
   -- ** Data
   , Data(..)
   -- * Processor   
   , Proc(..)
   -- * Network   
   , Network(..)
   , NetworkUID
   , networkUID
   , Duplex(..)
   , NetworkType(..)
   , BenchResult(..)
   , NetworkBenchResult(..)
   -- * Multi-data
   , MultiData (..)
   , MultiData_(..)
   )
where

import Control.Concurrent.STM
import Data.Ord (comparing)
import Data.Hashable
import Data.Word (Word64)

import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL
import qualified ViperVM.Platform.Drivers.Host as Host

import ViperVM.STM.TSet as TSet
import ViperVM.STM.TMap
import ViperVM.STM.TList

import ViperVM.Platform.Memory.Layout

---------------------------------------------------------------
-- MEMORY
---------------------------------------------------------------

-- | Memory
data Memory = Memory 
  { memoryPeer       :: Peer.MemoryPeer
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


---------------------------------------------------------------
-- PROCESSOR
---------------------------------------------------------------

-- | A processor
data Proc = Proc 
  { procPeer :: Peer.ProcPeer
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

---------------------------------------------------------------
-- NETWORK
---------------------------------------------------------------

-- | Networks interconnecting memories
data Network = Network 
  { networkPeer :: Peer.NetworkPeer
  , networkType :: NetworkType
  , networkNeighbors :: Memory -> STM (TSet Memory)
  , networkBenchs :: TMap (Memory,Memory) (TList NetworkBenchResult)
  }

instance Eq Network where
   (==) a b = networkPeer a == networkPeer b

instance Ord Network where
   compare = comparing networkPeer

newtype NetworkUID = NetworkUID String deriving (Read, Show, Eq, Ord,Hashable)

-- | Network link direction
data Duplex
   = Simplex 
   | HalfDuplex 
   | FullDuplex 
   deriving (Show)

-- | Network type
data NetworkType = NetworkPPP Duplex deriving (Show)

-- | Network unique identifier (not stable between different program executions)
networkUID :: Network -> NetworkUID
networkUID mem = NetworkUID $ case networkPeer mem of
   Peer.OpenCLNetwork m -> OpenCL.clNetUID m

instance Hashable Network where
   hashWithSalt salt n = hashWithSalt salt (networkUID n)

-- | Result of a transfer bench
data BenchResult
   = BenchFailed           -- ^ Failed transer
   | BenchSuccess Double   -- ^ Successful transfer with duration
   deriving (Eq,Show,Ord)

-- | Set of transfer bench results for different data sizes
data NetworkBenchResult = NetworkBenchResult
   { netBench1D :: TList (Word64,BenchResult)
   }

---------------------------------------------------------------
-- BUFFER
---------------------------------------------------------------

-- | A buffer
data Buffer = Buffer
   { bufferMemory :: Memory         -- ^ Memory containing the buffer
   , bufferSize :: Word64           -- ^ Size of the buffer
   , bufferPeer :: Peer.BufferPeer  -- ^ Buffer peer
   } deriving (Eq)

instance Ord Buffer where
   compare = comparing bufferPeer

-- | Buffer uniue identifier
newtype BufferID = BufferID String deriving (Eq,Ord,Show,Read,Hashable)

-- | Return a unique identifier for the buffer
bufferUID :: Buffer -> BufferID
bufferUID = BufferID . Peer.bufferUID . bufferPeer

instance Hashable Buffer where
   hashWithSalt salt m = hashWithSalt salt (bufferUID m)


---------------------------------------------------------------
-- DATA
---------------------------------------------------------------

-- | A data physically stored in memory with the given layout
data Data = Data 
   { dataBuffer :: Buffer           -- ^ Buffer containing the data
   , dataOffset :: Word64           -- ^ Offset in the buffer
   , dataLayout :: Layout           -- ^ Layout of the data
   , dataOwner  :: TMVar MultiData  -- ^ Owner node: it can't be an Object,
                                    -- otherwise type parameters would be
                                    -- needed. Existential quantification is
                                    -- used to avoid this.
   }


---------------------------------------------------------------
-- MULTI DATA
---------------------------------------------------------------

class MultiData_ o where
   mdInstances :: o -> STM [Data]
   mdSources :: o -> STM [MultiData]
   mdTargets :: o -> STM [MultiData]

-- | Multi data are used as untyped objects to create graphs of
-- buffer/data/"object"
--
-- This should contain enough information to perform garbage collection and
-- other memory management...
data MultiData = forall o . MultiData_ o => MultiData o
