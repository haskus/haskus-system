-- | Transactionnal graph
module ViperVM.STM.TGraph
   ( deepFirst
   , breadthFirst
   , TNode (..)
   , singleton
   , linkTo
   )
where

import qualified Data.Set as Set
import Control.Monad (foldM, foldM_, when, void)
import Control.Concurrent.STM
import Control.Applicative ((<$>), (<*>))

import ViperVM.STM.TList (TList)
import qualified ViperVM.STM.TList as TList

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


-- | A node contains a value and two lists of incoming/outgoing edges
data TNode a r = TNode
   { nodeValue :: a
   , nodeEdgeIn :: TList (r, TNode a r)
   , nodeEdgeOut :: TList (r, TNode a r)
   }

-- | Create a graph node
singleton :: a -> STM (TNode a r)
singleton v = TNode v <$> TList.empty <*> TList.empty

-- | Link two nodes together
linkTo :: TNode a r -> r -> TNode a r -> STM ()
linkTo src rel dst = do
   void $ TList.append (rel, src) (nodeEdgeIn dst)
   void $ TList.append (rel, dst) (nodeEdgeOut src)

