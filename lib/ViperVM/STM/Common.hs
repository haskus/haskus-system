module ViperVM.STM.Common where

import Control.Concurrent.STM
import Control.Monad

(>=$>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>=$>) a f = a >=> return . f

withTVar :: (a -> a) -> TVar a -> STM ()
withTVar f v = readTVar v >>= writeTVar v . f

useTVar :: TVar a -> (a -> STM a) -> STM ()
useTVar v f = readTVar v >>= f >>= writeTVar v

