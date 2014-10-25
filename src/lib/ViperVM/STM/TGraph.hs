-- | Transactionnal graph
module ViperVM.STM.TGraph
   ( deepFirst
   )
where

import qualified Data.Set as Set
import Control.Monad (foldM, foldM_)


-- | Deep-first graph traversal
deepFirst :: (Monad m, Ord a, Eq a) => (a -> m ()) -> (a -> m ()) -> (a -> m [a]) -> [a] -> m ()
deepFirst before after children xs = foldM_ go Set.empty xs
   where
      go visited x 
         | Set.member x visited = return visited
         | otherwise = do
            before x
            cs <- children x
            let visited' = Set.insert x visited
            visited'' <- foldM go visited' cs
            after x
            return visited''
