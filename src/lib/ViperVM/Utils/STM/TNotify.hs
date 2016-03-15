module ViperVM.Utils.STM.TNotify
   ( TNotify
   , TNotifySource
   , newTNotify
   , waitNotify
   , pollNotify
   , signalNotify
   , signalNotify'
   )
where

import Control.Concurrent.STM
import Control.Monad (void)

newtype TNotify a       = TNotify (TMVar a)
newtype TNotifySource a = TNotifySource (TMVar a)

-- | Create a TNotify and its source
newTNotify :: STM (TNotify a, TNotifySource a)
newTNotify = do
   m <- newEmptyTMVar
   return (TNotify m, TNotifySource m)

-- | Signal a notification
--
-- Return False if it has already been triggered
signalNotify :: a -> TNotifySource a -> STM ()
signalNotify a m = void (signalNotify' a m)

-- | Signal a notification
--
-- Return False if it has already been triggered
signalNotify' :: a -> TNotifySource a -> STM Bool
signalNotify' a (TNotifySource m) = tryPutTMVar m a

-- | Wait for a notification
waitNotify :: TNotify a -> STM a
waitNotify (TNotify m) = readTMVar m

-- | Test if a notification occured
pollNotify :: TNotify a -> STM (Maybe a)
pollNotify (TNotify m) = tryReadTMVar m
