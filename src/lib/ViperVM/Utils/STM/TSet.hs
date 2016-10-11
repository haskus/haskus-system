{-# LANGUAGE ConstraintKinds #-}

-- | STM mutable set
module ViperVM.Utils.STM.TSet
   ( TSet
   , null
   , size
   , member
   , notMember
   , empty
   , singleton
   , insert
   , delete
   , toList
   , fromList
   , elems
   , stream
   , unions
   , map
   )
where

import Prelude hiding (lookup,null,map)

import Control.Concurrent.STM
import qualified STMContainers.Set as SSET
import STMContainers.Set (Element)
import ListT (ListT, fold)
import qualified ListT

import Data.Foldable (forM_)

-- | STM Set
type TSet a = SSET.Set a

-- | Indicate if the set is empty
null :: TSet a -> STM Bool
null = SSET.null

-- | Number of elements in the set
size :: TSet a -> STM Int
size = fold f 0 . SSET.stream
   where 
      f n _ = return (n+1)

-- | Check if an element is in the set
member :: Element e => e -> TSet e -> STM Bool
member = SSET.lookup

-- | Check if an element is not in the set
notMember :: Element e => e -> TSet e -> STM Bool
notMember e s = not <$> member e s

-- | Create an empty set
empty :: STM (TSet e)
empty = SSET.new

-- | Create a set containing a single element
singleton :: Element e => e -> STM (TSet e)
singleton e = do
   m <- empty
   insert e m
   return m

-- | Insert an element in a set
insert :: Element e => e -> TSet e -> STM ()
insert = SSET.insert

-- | Delete an element from a set
delete :: Element e => e -> TSet e -> STM ()
delete = SSET.delete

-- | Convert a set into a list
toList :: TSet e -> STM [e]
toList = ListT.toList . SSET.stream

-- | Create a set from a list
fromList :: Element e => [e] -> STM (TSet e)
fromList xs = do
   s <- empty
   forM_ xs (`insert` s)
   return s

-- | Get the set elements
elems :: TSet e -> STM [e]
elems = toList

-- | Get the set as a ListT stream
stream :: TSet e -> ListT STM e
stream = SSET.stream

-- | Perform a set union
unions :: Element e => [TSet e] -> STM (TSet e)
unions ss = do
   ret <- empty
   forM_ ss $ \s -> 
      ListT.traverse_ (`insert` ret) (stream s)
   return ret

-- | Apply a function to each element in the set
map :: (Element b) => (a -> b) -> TSet a -> STM (TSet b)
map f m = do
   r <- empty
   ListT.traverse_ (\x -> insert (f x) r) (stream m)
   return r

--filter :: (a -> Bool) -> TSet a -> STM ()
--filter f = withTVar (Set.filter f)
