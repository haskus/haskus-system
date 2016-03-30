{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | State monad with multiplate states (extensible)
--
-- Similar to the multistate package, with the following differences (as of
-- 0.7.0.0):
--    * don't pollute Data.HList.HList
--    * use HArray instead of a HList, for fast indexing
module ViperVM.Utils.MultiState
   ( MStateT
   , mSet
   , mGet
   , mModify
   , mModify'
   )
where

import Control.Monad.State.Lazy

import ViperVM.Utils.HArray

type MStateT (s :: [*]) m a = StateT (HArray s) m a

-- | Set a value in the state
mSet :: (Monad m, HArrayIndexT a s) => a -> MStateT s m ()
mSet = modify' . setHArrayT

-- | Get a value in the state
mGet :: (Monad m, HArrayIndexT a s) => MStateT s m a
mGet = getHArrayT <$> get

-- | Modify a value in the state
mModify :: (Monad m, HArrayIndexT a s) => (a -> a) -> MStateT s m ()
mModify f = modify (\s -> setHArrayT (f (getHArrayT s)) s)

-- | Modify a value in the state (strict version)
mModify' :: (Monad m, HArrayIndexT a s) => (a -> a) -> MStateT s m ()
mModify' f = modify' (\s -> setHArrayT (f (getHArrayT s)) s)
