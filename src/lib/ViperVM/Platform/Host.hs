{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Host
   ( Host(..)
   , deepFirstMemories
   , breadthFirstMemories
   , traverseHostMemories
   , findMemoryByUID
   )
where

import ViperVM.Platform.Topology
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet
import ViperVM.STM.TGraph (deepFirst,breadthFirst)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Traversable (traverse)
import Control.Concurrent.STM
import Data.Maybe (listToMaybe)

-- | Platform
data Host = Host
   {
   -- | Host memories
   hostMemories :: TSet Memory
   }

-- | Traverse platform memories
traverseHostMemories :: Host -> (Memory -> STM a) -> STM [a]
traverseHostMemories host f = traverse f =<< TSet.toList (hostMemories host)

-- | Traverse all memories (deep-first search)
deepFirstMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
deepFirstMemories host ini f = do
      mems <- TSet.toList (hostMemories host)
      execStateT (deepFirst before after children mems) ini
   where
      before m = do
         v <- get
         v' <- lift (f v m)
         put v'

      after _ = return ()

      children :: Memory -> StateT a STM [Memory]
      children m = lift (TSet.toList =<< memoryNeighbors m)

-- | Traverse all memories (breadth-first search)
breadthFirstMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
breadthFirstMemories host ini f = do
      mems <- TSet.toList (hostMemories host)
      execStateT (breadthFirst visit children mems) ini
   where
      visit m = do
         v <- get
         v' <- lift (f v m)
         put v'

      children :: Memory -> StateT a STM [Memory]
      children m = lift (TSet.toList =<< memoryNeighbors m)


-- | Find a memory by UID
findMemoryByUID :: Host -> MemoryUID -> STM (Maybe Memory)
findMemoryByUID host uid = do
   let extractMem xs x = return (x:xs)
   mems <- breadthFirstMemories host [] extractMem
   return $ listToMaybe [x | x <- mems, memoryUID x == uid]
