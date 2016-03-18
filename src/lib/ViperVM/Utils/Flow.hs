{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Control flow
module ViperVM.Utils.Flow
   ( flowSeq
   , flowSeq'
   , flowMatch
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList

import Data.HList.HList
import Data.Proxy
import GHC.TypeLits

-- | Bind two variants in sequence. By convention, we choose the first variant
-- value/type as the valid one.
-- (i.e. generalized EitherT binding with reversed parameters)
flowSeq :: forall x xs m l l2 k .
   ( l2 ~ ReplaceAt 0 (x ': xs) l
   , Monad m
   , k ~ Length l
   , KnownNat k
   )
   => m (Variant (x ': xs)) -> (x -> m (Variant l)) -> m (Variant l2)
flowSeq v f = updateVariantFoldM (Proxy :: Proxy 0) f =<< v



-- | Like `flowSeq` but specialised for `()` (similarly to `>>`)
flowSeq' :: forall xs m l l2 k .
   ( l2 ~ ReplaceAt 0 (() ': xs) l
   , Monad m
   , k ~ Length l
   , KnownNat k
   )
   => m (Variant (() ': xs)) -> m (Variant l) -> m (Variant l2)
flowSeq' v f = updateVariantFoldM (Proxy :: Proxy 0) (const f) =<< v


-- | Match a variant
flowMatch :: forall l t m a l2 is.
   ( Monad m
   , l2 ~ MapMaybe l
   , is ~ Generate 0 (Length l)
   , KnownNat (Length l)
   , HTuple' (MapMaybe l) t
   , HFoldr' GetValue (Variant l, HList '[]) is (Variant l, HList l2)
   ) => m (Variant l) -> (t -> m a) -> m a
flowMatch v f = f . matchVariant =<< v
