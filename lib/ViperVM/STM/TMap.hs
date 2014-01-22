module ViperVM.STM.TMap where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import ViperVM.STM.Common
import Control.Applicative ( (<$>) )

type TMap a b = TVar (Map a b)

null :: TMap a b -> STM Bool
null = readTVar >=$> Map.null

size :: TMap a b -> STM Int
size = readTVar >=$> Map.size

member :: Ord a => a -> TMap a b -> STM Bool
member v = readTVar >=$> Map.member v

notMember :: Ord a => a -> TMap a b -> STM Bool
notMember v = readTVar >=$> Map.notMember v

lookup :: Ord k => k -> TMap k a -> STM (Maybe a)
lookup key m = Map.lookup key <$> readTVar m

empty :: STM (TMap a b)
empty = newTVar Map.empty

singleton :: a -> b -> STM (TMap a b)
singleton k v = newTVar (Map.singleton k v)

insert :: Ord a => a -> b -> TMap a b -> STM ()
insert k v = withTVar (Map.insert k v)

insert_ :: Ord a => TMap a b -> a -> b -> STM ()
insert_ m k v = insert k v m

delete :: Ord a => a -> TMap a b -> STM ()
delete v = withTVar (Map.delete v)

filter :: (b -> Bool) -> TMap a b -> STM ()
filter f = withTVar (Map.filter f)

foldr :: (b -> b -> b) -> b -> TMap a b -> STM b
foldr f x = readTVar >=$> Map.foldr f x

foldl :: (a -> b -> a) -> a -> TMap c b -> STM a
foldl f x = readTVar >=$> Map.foldl f x

elems :: TMap a b -> STM [b]
elems = readTVar >=$> Map.elems

keys :: TMap a b -> STM [a]
keys= readTVar >=$> Map.keys

toList :: TMap a b -> STM [(a,b)]
toList = readTVar >=$> Map.toList

(!) :: Ord a => TMap a b -> a -> STM b
(!) m k = (Map.! k) <$> readTVar m
