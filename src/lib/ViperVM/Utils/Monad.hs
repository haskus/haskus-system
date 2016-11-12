{-# LANGUAGE Rank2Types #-}

-- | Utils for Monads
module ViperVM.Utils.Monad
   ( MonadInIO (..)
   , module Control.Monad
   , module Control.Monad.IO.Class
   )
where

import Control.Monad.IO.Class
import Control.Monad

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
