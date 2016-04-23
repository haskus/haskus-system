{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | State monad with multiple states (extensible)
--
-- Similar to the multistate package, with the following differences (as of
-- 0.7.0.0):
--    * don't pollute Data.HList.HList
--    * use HArray instead of a HList, for fast indexing
module ViperVM.Utils.MultiState
   ( MStateT
   , MState
   , mSet
   , mGet
   , mTryGet
   , mModify
   , mModify'
   , mWith
   , runMState
   , evalMState
   , execMState
   , liftMStateT
   , (>~:>)
   , (>:>)
   )
where

import Control.Monad.State.Lazy
import Control.Monad.Identity

import ViperVM.Utils.HArray

-- | Multi-state monad transformer
--
-- States are stacked in a heterogeneous array.
type MStateT (s :: [*]) m a = StateT (HArray s) m a

-- | Multi-state
type MState (s :: [*]) a = MStateT s Identity a

-- | Run MState
runMState :: MState s a -> HArray s -> (a,HArray s)
runMState act s = runIdentity (runStateT act s)

-- | Evaluate MState
evalMState :: MState s a -> HArray s -> a
evalMState act s = runIdentity (evalStateT act s)

-- | Execute MState
execMState :: MState s a -> HArray s -> HArray s
execMState act s = runIdentity (execStateT act s)

-- | Set a value in the state
mSet :: (Monad m, HArrayIndexT a s) => a -> MStateT s m ()
mSet = modify' . setHArrayT

-- | Get a value in the state
mGet :: (Monad m, HArrayIndexT a s) => MStateT s m a
mGet = getHArrayT <$> get

-- | Try to get a value in the state
mTryGet :: (Monad m, HArrayTryIndexT a s) => MStateT s m (Maybe a)
mTryGet = tryGetHArrayT <$> get


-- | Modify a value in the state
mModify :: (Monad m, HArrayIndexT a s) => (a -> a) -> MStateT s m ()
mModify f = modify (\s -> setHArrayT (f (getHArrayT s)) s)

-- | Modify a value in the state (strict version)
mModify' :: (Monad m, HArrayIndexT a s) => (a -> a) -> MStateT s m ()
mModify' f = modify' (\s -> setHArrayT (f (getHArrayT s)) s)

-- | Execute an action with an extended state
mWith :: forall s a m b.
   ( Monad m
   ) => a -> MStateT (a ': s) m b -> MStateT s m b
mWith v act = do
   s <- get
   (r,s') <- lift $ runStateT act (prependHArray v s)
   put (tailHArray s')
   return r

-- | Lift a multi-state into an HArray transformer
liftMStateT :: (Monad m) => MStateT xs m x -> HArrayT m xs (x ': xs)
liftMStateT act = HArrayT $ \xs -> do
   (x,xs') <- runStateT act xs
   return (prependHArray x xs')

-- | Compose MStateT
(>~:>) :: (Monad m) => HArrayT m xs ys -> MStateT ys m y -> HArrayT m xs (y ': ys)
(>~:>) f g = f >~:~> liftMStateT g

-- | Compose MStateT
(>:>) :: (Monad m) => MStateT xs m x -> MStateT (x ': xs) m y -> HArrayT m xs (y ': x ': xs)
(>:>) f g = liftMStateT f >~:~> liftMStateT g
