{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Host
   ( Host
   , newHost
   , hostMemories
   , hostMemoriesIO
   , hostProcessors
   , hostProcessorsIO
   , hostNetworks
   , hostNetworksIO
   , allMemoriesFromHost
   , allMemoriesFromHostIO
   , allProcessorsFromHost
   , allProcessorsFromHostIO
   , allNetworksFromHost
   , allNetworksFromHostIO
   , deepFirstMemories
   , breadthFirstMemories
   , traverseHostMemories
   , findMemory
   , findMemoryByUID
   )
where

import ViperVM.Platform.Topology
import ViperVM.Platform.Types
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet
import ViperVM.STM.TGraph (deepFirst,breadthFirst)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Traversable (traverse)
import Control.Concurrent.STM

-- | Host
data Host = Host
   {
   -- | Host memories
   hostMems :: TSet Memory
   }

-- | Create a new Host
newHost :: TSet Memory -> Host
newHost = Host

-- | Helper to make *IO accessors
toIO :: (Host -> STM (TSet a)) -> Host -> IO [a]
toIO f h = atomically $ do
   xs <- f h
   TSet.toList xs

-- | Retrieve host memories
hostMemories :: Host -> STM (TSet Memory)
hostMemories h = 
   -- we do this to support removable memories in the
   -- future: we don't give direct access to the
   -- underlying hostMems
   return (hostMems h)

-- | Retrieve host memories in IO
hostMemoriesIO :: Host -> IO [Memory]
hostMemoriesIO = toIO hostMemories

-- | Retrieve host processors
hostProcessors :: Host -> STM (TSet Proc)
hostProcessors h = do
   mems <- TSet.toList =<< hostMemories h
   TSet.unions (map memoryProcs mems)

-- | Retrieve host processors in IO
hostProcessorsIO :: Host -> IO [Proc]
hostProcessorsIO = toIO hostProcessors

-- | Retrieve host networks
hostNetworks :: Host -> STM (TSet Network)
hostNetworks h = do
   mems <- TSet.toList =<< hostMemories h
   TSet.unions (map memoryNetworks mems)

-- | Retrieve host networks in IO
hostNetworksIO :: Host -> IO [Network]
hostNetworksIO = toIO hostNetworks

-- | Retrieve all memories reachable from the host
allMemoriesFromHost :: Host -> STM (TSet Memory)
allMemoriesFromHost h = do
   accu <- TSet.empty
   let op xs x = TSet.insert x xs >> return xs
   breadthFirstMemories h accu op

-- | Retrieve all memories reachable from the host in IO
allMemoriesFromHostIO :: Host -> IO [Memory]
allMemoriesFromHostIO = toIO allMemoriesFromHost

-- | Retrieve all processors reachable from the host
allProcessorsFromHost :: Host -> STM (TSet Proc)
allProcessorsFromHost h = do
   mems <- TSet.toList =<< allMemoriesFromHost h
   TSet.unions (map memoryProcs mems)

-- | Retrieve all processors reachable from the host in IO
allProcessorsFromHostIO :: Host -> IO [Proc]
allProcessorsFromHostIO = toIO allProcessorsFromHost

-- | Retrieve all networks reachable from the host
allNetworksFromHost :: Host -> STM (TSet Network)
allNetworksFromHost h = do
   mems <- TSet.toList =<< allMemoriesFromHost h
   TSet.unions (map memoryNetworks mems)

-- | Retrieve all networks reachable from the host in IO
allNetworksFromHostIO :: Host -> IO [Network]
allNetworksFromHostIO = toIO allNetworksFromHost

-- | Traverse platform memories
traverseHostMemories :: Host -> (Memory -> STM a) -> STM [a]
traverseHostMemories host f = traverse f =<< TSet.toList =<< hostMemories host

-- | Traverse all memories (deep-first search)
deepFirstMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
deepFirstMemories host ini f = do
      mems <- TSet.toList =<< hostMemories host
      execStateT (deepFirst before after children mems) ini
   where
      before m = do
         v <- get
         v' <- lift (f v m)
         put v'

      after _ = return ()
      children m = lift (TSet.toList =<< memoryNeighbors m)

-- | Traverse all memories (breadth-first search)
breadthFirstMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
breadthFirstMemories host ini f = do
      mems <- TSet.toList =<< hostMemories host
      execStateT (breadthFirst visit children mems) ini
   where
      visit m = do
         v <- get
         v' <- lift (f v m)
         put v'
         return True
      children m = lift (TSet.toList =<< memoryNeighbors m)


-- | Try to find a memory matching the predicate (breadth-first search)
findMemory :: Host -> (Memory -> STM Bool) -> STM (Maybe Memory)
findMemory host f = do
      mems <- TSet.toList =<< hostMemories host
      execStateT (breadthFirst visit children mems) Nothing
   where
      visit m = do
         b <- lift $ f m
         if b
            then do
               put (Just m) -- return found memory (in state)
               return False -- stop the traversal
            else return True

      children m = lift (TSet.toList =<< memoryNeighbors m)

-- | Find a memory by UID
findMemoryByUID :: Host -> MemoryUID -> STM (Maybe Memory)
findMemoryByUID host uid = 
   findMemory host (return . (== uid) . memoryUID)
