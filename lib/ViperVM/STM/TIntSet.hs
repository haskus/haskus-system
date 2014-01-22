module ViperVM.STM.TIntSet where

import Control.Concurrent.STM
import Data.IntSet
import qualified Data.IntSet as IntSet
import ViperVM.STM.Common

type TIntSet = TVar IntSet

null :: TIntSet -> STM Bool
null = readTVar >=$> IntSet.null

size :: TIntSet -> STM Int
size = readTVar >=$> IntSet.size

member :: Int -> TIntSet -> STM Bool
member v = readTVar >=$> IntSet.member v

notMember :: Int -> TIntSet -> STM Bool
notMember v = readTVar >=$> IntSet.notMember v

empty :: STM TIntSet
empty = newTVar IntSet.empty

singleton :: Int -> STM TIntSet
singleton v = newTVar (IntSet.singleton v)

insert :: Int -> TIntSet -> STM ()
insert v = withTVar (IntSet.insert v)

delete :: Int -> TIntSet -> STM ()
delete v = withTVar (IntSet.delete v)

filter :: (Int -> Bool) -> TIntSet -> STM ()
filter f = withTVar (IntSet.filter f)

foldr :: (Int -> b -> b) -> b -> TIntSet -> STM b
foldr f x = readTVar >=$> IntSet.foldr f x

foldl :: (a -> Int -> a) -> a -> TIntSet -> STM a
foldl f x = readTVar >=$> IntSet.foldl f x

elems :: TIntSet -> STM [Int]
elems = readTVar >=$> IntSet.elems

toList :: TIntSet -> STM [Int]
toList = readTVar >=$> IntSet.toList
