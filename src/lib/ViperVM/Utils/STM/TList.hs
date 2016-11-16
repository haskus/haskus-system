{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Transactional list
module ViperVM.Utils.STM.TList
   ( TList
   , TNode
   , empty
   , singleton
   , null
   , length
   , first
   , last
   , prev
   , next
   , value
   , deleteAll
   , delete
   , filter
   , find
   , append
   , append_
   , prepend
   , prepend_
   , insertBefore
   , insertAfter
   , toList
   , toReverseList
   , fromList
   , index
   , take
   )
where

import Prelude hiding (null,length,last,filter,take)

import ViperVM.Utils.STM
import ViperVM.Utils.Flow
import ViperVM.Utils.Maybe

-- | A double linked-list
newtype TList a = TList (TNode a)

-- | A node in the list
--
-- Every list has a marker node whose value is Nothing. Its nodePrev links to
-- the last node and its nodeNext links to the first node.
data TNode a = TNode
   { nodeValue :: Maybe a         -- value (Nothing if list marker)
   , nodePrev  :: TVar (TNode a)  -- previous node
   , nodeNext  :: TVar (TNode a)  -- next node
   }

-- | Get value associated with a node
value :: TNode a -> a
value node = case nodeValue node of
   Just v  -> v
   Nothing -> error "TList: empty node value"

-- | Empty node singleton
empty :: STM (TList a)
empty = do
   p <- newTVar undefined
   n <- newTVar undefined
   let node = TNode Nothing p n
   -- initially the marker node refers to itself
   writeTVar p node
   writeTVar n node
   return (TList node)

-- | Remove all the elements of the list (O(1))
deleteAll :: TList a -> STM ()
deleteAll (TList m) = do
   -- make the marker node refer to itself
   writeTVar (nodeNext m) m
   writeTVar (nodePrev m) m

-- | Create a singleton list
singleton :: e -> STM (TList e)
singleton e = do
   m <- empty
   void $ append e m
   return m

-- | Indicate if the list is empty
null :: TList e -> STM Bool
null (TList m) = do
   h <- readTVar (nodeNext m)
   -- if the list is empty, the marker node refers to itself, hence the value of
   -- the nodeNext node is Nothing
   return (isNothing (nodeValue h))

-- | Count the number of elements in the list (0(n))
length :: TList e -> STM Word
length (TList m) = go 0 m
   where
      go !n node = do
         node' <- readTVar (nodeNext node)
         case nodeValue node' of
            Nothing -> return n
            Just _  -> go (n+1) node'

-- | Get the first element if any
first :: TList e -> STM (Maybe (TNode e))
first (TList m) = next m

-- | Get the last element if any
last :: TList e -> STM (Maybe (TNode e))
last (TList m) = prev m

-- | Get the previous element if any
prev :: TNode a -> STM (Maybe (TNode a))
prev n = do
   h <- readTVar (nodePrev n)
   case nodeValue h of
      Nothing -> return Nothing
      Just _  -> return (Just h)

-- | Get the next element if any
next :: TNode a -> STM (Maybe (TNode a))
next n = do
   h <- readTVar (nodeNext n)
   case nodeValue h of
      Nothing -> return Nothing
      Just _  -> return (Just h)


-- | Delete a element of the list
delete :: TNode a -> STM ()
delete n = do
   -- if somehow we delete the marker node, we get a ring-list
   left <- readTVar $ nodePrev n
   right <- readTVar $ nodeNext n
   writeTVar (nodeNext left) right
   writeTVar (nodePrev right) left
   -- Link list node to itself so subsequent 'delete' calls will be harmless.
   writeTVar (nodePrev n) n
   writeTVar (nodeNext n) n

-- | Insert a node between two adjacent nodes.
insertBetween :: a -> TNode a -> TNode a -> STM (TNode a)
insertBetween v left right = do
   n <- TNode (Just v) <$> newTVar left 
                       <*> newTVar right
   writeTVar (nodeNext left)  n
   writeTVar (nodePrev right) n
   return n

-- | Append an element to the list
append :: a -> TList a -> STM (TNode a)
append v (TList m) = insertAfter v m

-- | Append an element to the list
append_ :: a -> TList a -> STM ()
append_ a = void . append a

-- | Prepend an element to the list
prepend :: a -> TList a -> STM (TNode a)
prepend v (TList m) = insertBefore v m

-- | Prepend an element to the list
prepend_ :: a -> TList a -> STM ()
prepend_ a = void . prepend a

-- | Insert an element before another
insertBefore :: a -> TNode a -> STM (TNode a)
insertBefore v n = do
   right <- readTVar $ nodeNext n
   insertBetween v n right

-- | Insert an element after another
insertAfter :: a -> TNode a -> STM (TNode a)
insertAfter v n = do
   left <- readTVar $ nodePrev n
   insertBetween v left n

-- | Convert into a list (O(n))
toList :: TList a -> STM [a]
toList (TList m) = go [] m
   where
      go !xs node = do
         node' <- readTVar (nodePrev node)
         case nodeValue node' of
            Nothing -> return xs
            Just x  -> go (x:xs) node'

-- | Convert into a reversed list (O(n))
toReverseList :: TList a -> STM [a]
toReverseList (TList m) = go [] m
   where
      go !xs node = do
         node' <- readTVar (nodeNext node)
         case nodeValue node' of
            Nothing -> return xs
            Just x  -> go (x:xs) node'

-- | Create from a list
fromList :: [e] -> STM (TList e)
fromList xs = do
   s <- empty
   forM_ xs (`append` s)
   return s

-- | Only keep element matching the criterium
filter :: (e -> STM Bool) -> TList e -> STM ()
filter f (TList m) = go m
   where
      go node = do
         node' <- readTVar (nodeNext node)
         case nodeValue node' of
            Nothing -> return ()
            Just v  -> do
               p <- f v
               if not p
                  then delete node' >> go node
                  else go node'

-- | Find the first node matching the predicate (if any)
find :: (e -> STM Bool) -> TList e -> STM (Maybe (TNode e))
find f (TList m) = go m
   where
      go node = do
         node' <- readTVar (nodeNext node)
         case nodeValue node' of
            Nothing -> return Nothing
            Just v  -> do
               p <- f v
               if p
                  then return (Just node')
                  else go node'

-- | Get the node from its index
index :: Word -> TList e -> STM (Maybe (TNode e))
index n (TList m) = go n m
   where
      go !i node = do
         node' <- readTVar (nodeNext node)
         case nodeValue node' of
            Nothing        -> return Nothing
            Just _
               | i == 0    -> return (Just node')
               | otherwise -> go (i-1) node'

-- | Take (and remove) up to n elements in the list (O(n))
take :: Word -> TList e -> STM [e]
take n l = index n l >>= \case
   -- return the whole list
   Nothing -> do
      r <- toList l
      deleteAll l
      return r

   -- build the list and remove elements at the same time
   Just node -> go [] node
      where
         go !xs node' = do
            case nodeValue node' of
               Nothing -> return xs
               Just x  -> do
                  p <- readTVar (nodePrev node')
                  delete node'
                  go (x:xs) p
