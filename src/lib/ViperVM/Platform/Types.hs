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
   , procModelHash
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

import ViperVM.Utils.Hash (hashString)
import ViperVM.Utils.STM.TSet as TSet
import ViperVM.Utils.STM.TMap
import ViperVM.Utils.STM.TList

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
procUID proc = ProcUID $ case procPeer proc of
   Peer.OpenCLProc p -> OpenCL.clProcUID p
   Peer.HostProc p -> Host.hostProcUID p

-- | Return a hash for the proc model
procModelHash :: Proc -> String
procModelHash proc = hashString $ case procPeer proc of
   Peer.OpenCLProc p -> OpenCL.clProcModel p
   Peer.HostProc p -> Host.hostProcModel p

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
   mdInstances :: o -> STM [Data]      -- ^ Retrieve data instances
   mdSources :: o -> STM [MultiData]   -- ^ Retrieve multi data that can be used as sources
   mdTargets :: o -> STM [MultiData]   -- ^ Retrieve multi data for which this data can be used as source

-- | Multi data are used as untyped objects to create graphs of
-- buffer/data/"object"
--
-- This should contain enough information to perform garbage collection and
-- other memory management...
data MultiData = forall o . MultiData_ o => MultiData o

instance MultiData_ MultiData where
   mdInstances (MultiData a) = mdInstances a
   mdSources (MultiData a) = mdSources a
   mdTargets (MultiData a) = mdTargets a
