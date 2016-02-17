{-# LANGUAGE ConstraintKinds #-}
module ViperVM.Utils.STM.TMap
   ( TMap
   , null
   , size
   , lookup
   , member
   , notMember
   , empty
   , singleton
   , insert
   , delete
   , elems
   , keys
   , (!)
   )
where

import Prelude hiding (lookup,null)

import Control.Concurrent.STM
import qualified STMContainers.Map as SMAP
import STMContainers.Map (Key)
import ListT (fold)
import qualified ListT

import Data.Maybe (fromJust,isJust,isNothing)

type TMap a b = SMAP.Map a b

null :: TMap a b -> STM Bool
null = SMAP.null

size :: TMap a b -> STM Int
size = fold f 0 . SMAP.stream
   where 
      f n _ = return (n+1)

lookup :: Key k => k -> TMap k a -> STM (Maybe a)
lookup = SMAP.lookup

member :: Key k => k -> TMap k b -> STM Bool
member k m = isJust <$> lookup k m

notMember :: Key k => k -> TMap k b -> STM Bool
notMember k m = isNothing <$> lookup k m


empty :: STM (TMap a b)
empty = SMAP.new

singleton :: Key k => k -> v -> STM (TMap k v)
singleton k v = do
   m <- empty
   insert k v m
   return m

insert :: Key k => k -> v -> TMap k v -> STM ()
insert k v = SMAP.insert v k

delete :: Key k => k -> TMap k v -> STM ()
delete = SMAP.delete

toList :: TMap a b -> STM [(a,b)]
toList = ListT.toList . SMAP.stream

elems :: TMap a b -> STM [b]
elems = fmap (fmap snd) . toList

keys :: TMap a b -> STM [a]
keys = fmap (fmap fst) . toList

(!) :: Key k => TMap k v -> k -> STM v
(!) m k = fromJust <$> lookup k m
