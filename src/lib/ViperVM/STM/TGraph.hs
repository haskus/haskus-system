-- | Transactionnal graph
module ViperVM.STM.TGraph
   ( deepFirst
   , breadthFirst
   )
where

import qualified Data.Set as Set
import Control.Monad (foldM, foldM_, when)


-- | Deep-first graph traversal
--
-- before is executed when the node is entered
-- after is executed when the node is leaved
-- children gets node's children
--
deepFirst :: (Monad m, Ord a, Eq a) => (a -> m ()) -> (a -> m ()) -> (a -> m [a]) -> [a] -> m ()
deepFirst before after children xs = foldM_ go Set.empty xs
   where
      go visited x 
         | Set.member x visited = 
            -- the node is already visited
            return visited

         | otherwise = do
            before x
            cs <- children x
            -- add current node to the visited ones to avoid "loops"
            let visited' = Set.insert x visited
            -- visited "children" nodes
            visited'' <- foldM go visited' cs
            after x
            return visited''

-- | Breadth-first graph traversal
--
-- visit is executed when the node is entered. If False is returned, the traversal ends
-- children gets node's children
--
breadthFirst :: (Monad m, Ord a, Eq a) => (a -> m Bool) -> (a -> m [a]) -> [a] -> m ()
breadthFirst visit children = go Set.empty
   where
      go _ [] = 
         -- there are no more nodes to visit
         return ()

      go visited (x:xs) 
         | Set.member x visited = 
            -- the node is already visited, we skip it
            go visited xs

         | otherwise = do
            b <- visit x
            -- if "visit" returns False, we stop the traversal
            when b $ do
               -- otherwise we add children to the list of nodes to
               -- visit and we continue the traversal
               cs <- children x
               go (Set.insert x visited) (xs ++ cs)
