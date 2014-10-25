{-# LANGUAGE ConstraintKinds #-}
module ViperVM.STM.TSet
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

type TSet a = SSET.Set a

null :: TSet a -> STM Bool
null = SSET.null

size :: TSet a -> STM Int
size = fold f 0 . SSET.stream
   where 
      f n _ = return (n+1)

member :: Element e => e -> TSet e -> STM Bool
member = SSET.lookup

notMember :: Element e => e -> TSet e -> STM Bool
notMember = fmap (fmap not) . member

empty :: STM (TSet e)
empty = SSET.new

singleton :: Element e => e -> STM (TSet e)
singleton e = do
   m <- empty
   insert e m
   return m

insert :: Element e => e -> TSet e -> STM ()
insert = SSET.insert

delete :: Element e => e -> TSet e -> STM ()
delete = SSET.delete

toList :: TSet e -> STM [e]
toList = ListT.toList . SSET.stream

fromList :: Element e => [e] -> STM (TSet e)
fromList xs = do
   s <- empty
   forM_ xs (`insert` s)
   return s

elems :: TSet e -> STM [e]
elems = toList

stream :: TSet e -> ListT STM e
stream = SSET.stream

unions :: Element e => [TSet e] -> STM (TSet e)
unions ss = do
   ret <- empty
   forM_ ss $ \s -> 
      ListT.traverse_ (`insert` ret) (stream s)
   return ret

map :: (Element a, Element b) => (a -> b) -> TSet a -> STM (TSet b)
map f m = do
   r <- empty
   ListT.traverse_ (\x -> insert (f x) r) (stream m)
   return r

--filter :: (a -> Bool) -> TSet a -> STM ()
--filter f = withTVar (Set.filter f)
