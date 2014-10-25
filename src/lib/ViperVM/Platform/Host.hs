{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Host
   ( Host(..)
   , foldMemories
   , traverseHostMemories
   , findMemoryByUID
   )
where

import ViperVM.Platform.Topology
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet

import Data.Traversable (traverse)
import Control.Concurrent.STM
import Data.Maybe (listToMaybe)
import ListT (fold)

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
foldMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
foldMemories host ini f = do
      visited <- TSet.empty
      fold (visit visited) ini (TSet.stream (hostMemories host))
   where
      -- Visit a memory
      visit visited ret m =
         -- check that we haven't already visited the node
         TSet.member m visited >>= \case
            True  -> return ret
            False -> do
               -- Perform action
               r <- f ret m
               -- indicate that the node is visited
               TSet.insert m visited
               -- visit neighbours
               ns <- memoryNeighbors m
               fold (visit visited) r (TSet.stream ns)
               


-- | Find a memory by UID
findMemoryByUID :: Host -> MemoryUID -> STM (Maybe Memory)
findMemoryByUID host uid = do
   let extractMem xs x = return (x:xs)
   mems <- foldMemories host [] extractMem
   return $ listToMaybe [x | x <- mems, memoryUID x == uid]
