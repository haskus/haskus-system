{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.Utils.HList
   ( HFoldr' (..)
   )
where

import Data.HList.FakePrelude (ApplyAB(..))
import Data.HList.HList

-- | Like HFoldr but only use types, not values!
--
-- It allows us to foldr over the list of types in the union and for each type
-- to retrieve the alignment and the size (from Storable).
class HFoldr' f v (l :: [*]) r where
   hFoldr' :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr' f v '[] v' where
   hFoldr'       _ v _   = v

instance (ApplyAB f (e, r) r', HFoldr' f v l r) => HFoldr' f v (e ': l) r' where
   -- compared to hFoldr, we pass undefined values instead of the values
   -- supposedly in the list (we don't have a real list associated to HList l)
   hFoldr' f v _ = applyAB f (undefined :: e, hFoldr' f v (undefined :: HList l) :: r)

