module ViperVM.Platform.Host (
   Host(..), foldMemories,
   traverseHostMemories
) where

import ViperVM.Platform.Topology
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet

import Data.Traversable (traverse)
import qualified Data.Set as Set
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Concurrent.STM

-- | Platform
data Host = Host {
   -- | Host memories
   hostMemories :: TSet Memory
}

-- | Traverse platform memories
traverseHostMemories :: Host -> (Memory -> STM a) -> STM [a]
traverseHostMemories host f = traverse f =<< TSet.toList (hostMemories host)

-- | Traverse all memories (breadth-first search)
foldMemories :: Host -> a -> (a -> Memory -> STM a) -> STM a
foldMemories host ini f = go ini Set.empty =<< readTVar (hostMemories host)
   where
      go ret visited toVisit
         | Set.null toVisit = return ret
         | otherwise = do
               let ms = Set.toList toVisit
               r <- foldM f ret ms

               neighbors <- Set.unions <$> mapM memoryNeighbors ms

               let visited' = Set.union visited toVisit
                   toVisit' = Set.difference neighbors visited'
               go r visited' toVisit'
