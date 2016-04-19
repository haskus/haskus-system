
-- | Future values
module ViperVM.Utils.STM.Future
   ( Future
   , FutureSource
   , newFuture
   , newFutureIO
   , waitFuture
   , pollFuture
   , pollFutureIO
   , setFuture
   , setFutureIO
   , setFuture'
   )
where

import Control.Concurrent.STM
import Control.Monad (void)

-- | Future value of type a
newtype Future a       = Future (TMVar a)

-- | Setter for a future value
newtype FutureSource a = FutureSource (TMVar a)

-- | Create a Future and its source
newFuture :: STM (Future a, FutureSource a)
newFuture = do
   m <- newEmptyTMVar
   return (Future m, FutureSource m)

-- | `newFuture` in `IO`
newFutureIO :: IO (Future a, FutureSource a)
newFutureIO = atomically newFuture

-- | Set a future
setFuture :: a -> FutureSource a -> STM ()
setFuture a m = void (setFuture' a m)

-- | Set a future in IO
setFutureIO :: a -> FutureSource a -> IO ()
setFutureIO a m = atomically (setFuture a m)

-- | Set a future
--
-- Return False if it has already been set
setFuture' :: a -> FutureSource a -> STM Bool
setFuture' a (FutureSource m) = tryPutTMVar m a

-- | Wait for a future
waitFuture :: Future a -> STM a
waitFuture (Future m) = readTMVar m

-- | Poll a future
pollFuture :: Future a -> STM (Maybe a)
pollFuture (Future m) = tryReadTMVar m

-- | `pollFuture` in `IO`
pollFutureIO :: Future a -> IO (Maybe a)
pollFutureIO = atomically . pollFuture
