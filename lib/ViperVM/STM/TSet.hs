module ViperVM.STM.TSet where

import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.Set (Set)
import ViperVM.STM.Common
import Control.Applicative ( (<$>) )

type TSet a = TVar (Set a)

null :: TSet a -> STM Bool
null = readTVar >=$> Set.null

size :: TSet a -> STM Int
size = readTVar >=$> Set.size

member :: Ord a => a -> TSet a -> STM Bool
member v = readTVar >=$> Set.member v

notMember :: Ord a => a -> TSet a -> STM Bool
notMember v = readTVar >=$> Set.notMember v

empty :: STM (TSet a)
empty = newTVar Set.empty

singleton :: a -> STM (TSet a)
singleton v = newTVar (Set.singleton v)

insert :: Ord a => a -> TSet a -> STM ()
insert v = withTVar (Set.insert v)

delete :: Ord a => a -> TSet a -> STM ()
delete v = withTVar (Set.delete v)

filter :: (a -> Bool) -> TSet a -> STM ()
filter f = withTVar (Set.filter f)

foldr :: (a -> b -> b) -> b -> TSet a -> STM b
foldr f x = readTVar >=$> Set.foldr f x

foldl :: (a -> b -> a) -> a -> TSet b -> STM a
foldl f x = readTVar >=$> Set.foldl f x

elems :: TSet a -> STM [a]
elems = readTVar >=$> Set.elems

toList :: TSet a -> STM [a]
toList = readTVar >=$> Set.toList

toSet :: TSet a -> STM (Set a)
toSet = readTVar

pop :: Ord a => TSet a -> STM a
pop xs = do
   x <- head <$> toList xs
   delete x xs
   return x
