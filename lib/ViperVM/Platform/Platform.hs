module ViperVM.Platform.Platform (
   Platform(..), foldMemories,
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
data Platform = Platform {
   -- | Host memories
   platformHostMemories :: TSet Memory
}

-- | Traverse platform memories
traverseHostMemories :: Platform -> (Memory -> STM a) -> STM [a]
traverseHostMemories pf f = traverse f =<< TSet.toList (platformHostMemories pf)

-- | Traverse all memories (breadth-first search)
foldMemories :: Platform -> a -> (a -> Memory -> STM a) -> STM a
foldMemories pf ini f = go ini Set.empty =<< readTVar (platformHostMemories pf)
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
