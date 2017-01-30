{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utils for Monads
module Haskus.Utils.Monad
   ( MonadInIO (..)
   , module Control.Monad
   , module Control.Monad.IO.Class
   , module Control.Monad.Extra
   , module Control.Monad.Trans.Class
   )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State

class MonadIO m => MonadInIO m where
   -- | Lift with*-like functions into IO (alloca, etc.)
   liftWith :: (forall c. (a -> IO c) -> IO c) -> (a -> m b) -> m b

   -- | Lift with*-like functions into IO (alloca, etc.)
   liftWith2 :: (forall c. (a -> b -> IO c) -> IO c) -> (a -> b -> m e) -> m e

instance MonadInIO IO where
   {-# INLINE liftWith #-}
   liftWith = id

   {-# INLINE liftWith2 #-}
   liftWith2 = id

instance MonadInIO m => MonadInIO (StateT s m) where
   {-# INLINE liftWith #-}
   liftWith wth f =
      StateT $ \s -> do
         liftWith wth (\a -> runStateT (f a) s)

   {-# INLINE liftWith2 #-}
   liftWith2 wth f =
      StateT $ \s ->
         liftWith2 wth (\a b -> runStateT (f a b) s)
