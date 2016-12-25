{-# LANGUAGE ConstraintKinds #-}

-- | STm hashmap
module Haskus.Utils.STM.TMap
   ( TMap
   , Key
   , null
   , size
   , lookup
   , member
   , notMember
   , empty
   , singleton
   , insert
   , fromList
   , delete
   , elems
   , keys
   , (!)
   )
where

import Prelude hiding (lookup,null)

import Haskus.Utils.STM
import qualified STMContainers.Map as SMAP
import STMContainers.Map (Key)
import ListT (fold)
import qualified ListT

import Haskus.Utils.Maybe (fromJust,isJust,isNothing)

-- | STM hashmap
type TMap a b = SMAP.Map a b

-- | Indicate if the map is empty
null :: TMap a b -> STM Bool
null = SMAP.null

-- | Get the number of elements in the map
size :: TMap a b -> STM Int
size = fold f 0 . SMAP.stream
   where 
      f n _ = return (n+1)

-- | Lookup an element in the map from its key
lookup :: Key k => k -> TMap k a -> STM (Maybe a)
lookup = SMAP.lookup

-- | Check if a key is in the map
member :: Key k => k -> TMap k b -> STM Bool
member k m = isJust <$> lookup k m

-- | Check if a key is not in the map
notMember :: Key k => k -> TMap k b -> STM Bool
notMember k m = isNothing <$> lookup k m

-- | Create an empty map
empty :: STM (TMap a b)
empty = SMAP.new

-- | Create a map containing a single element
singleton :: Key k => k -> v -> STM (TMap k v)
singleton k v = do
   m <- empty
   insert k v m
   return m

-- | Insert an element in the map
insert :: Key k => k -> v -> TMap k v -> STM ()
insert k v = SMAP.insert v k

-- | Create a new TMap from a list
fromList :: Key k => [(k,v)] -> STM (TMap k v)
fromList vs = do
   m <- empty
   mapM_ (\(k,v) -> insert k v m) vs
   return m

-- | Delete an element from the map
delete :: Key k => k -> TMap k v -> STM ()
delete = SMAP.delete

-- | Create a list of (key,value)
toList :: TMap a b -> STM [(a,b)]
toList = ListT.toList . SMAP.stream

-- | Get values
elems :: TMap a b -> STM [b]
elems = fmap (fmap snd) . toList

-- | Get keys
keys :: TMap a b -> STM [a]
keys = fmap (fmap fst) . toList

-- | Unsafe lookup in the map
(!) :: Key k => TMap k v -> k -> STM v
(!) m k = fromJust <$> lookup k m
