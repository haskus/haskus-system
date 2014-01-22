module ViperVM.STM.TList where

import Control.Concurrent.STM
import qualified Data.List as List
import ViperVM.STM.Common

type TList a = TVar [a]

null :: TList a -> STM Bool
null = readTVar >=$> List.null

empty :: STM (TList a)
empty = newTVar []

pop :: TList a -> STM a
pop v = do
   (x:xs) <- readTVar v
   writeTVar v xs
   return x

push :: TList a -> a -> STM ()
push v x = do
   xs <- readTVar v
   writeTVar v (x:xs)
   
