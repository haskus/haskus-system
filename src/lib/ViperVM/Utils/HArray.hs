{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Heterogeneous array: like a HList but indexed in O(1)
module ViperVM.Utils.HArray
   ( HArray
   , HArrayIndex
   , HArrayIndexT
   , HArrayTryIndexT
   , emptyHArray
   , getHArrayN
   , setHArrayN
   , getHArrayT
   , setHArrayT
   , tryGetHArrayT
   , appendHArray
   , prependHArray
   , concatHArray
   )
where

import Data.Vector as V
import Data.Proxy
import Unsafe.Coerce
import GHC.TypeLits

import ViperVM.Utils.HList

-- | heterogeneous array
data HArray (types :: [*]) = forall a. HArray (Vector a)

type role HArray representational

-- | Empty array
emptyHArray :: HArray '[]
emptyHArray = HArray V.empty

-- | The type `t` with index `n` is indexable in the array
type HArrayIndex (n :: Nat) t (ts :: [*]) =
   ( KnownNat n
   , t ~ TypeAt n ts
   , KnownNat (Length ts)
   , CmpNat n (Length ts) ~ 'LT
   )

-- | A type `t` is indexable in the array
type HArrayIndexT t (ts :: [*]) =
   ( IsMember t ts ~ 'True
   , HArrayIndex (IndexOf t ts) t ts
   )

-- | A type `t` is maybe indexable in the array
type HArrayTryIndexT t (ts :: [*]) =
   ( HArrayIndex (MaybeIndexOf t ts) t (t ': ts)
   )


-- | Get an element by index
getHArrayN :: forall (n :: Nat) (ts :: [*]) t.
   (HArrayIndex n t ts) => Proxy n -> HArray ts -> t
getHArrayN _ (HArray as) = unsafeCoerce (as ! idx)
   where
      idx = fromIntegral (natVal (Proxy :: Proxy n))

-- | Set an element by index
setHArrayN :: forall (n :: Nat) (ts :: [*]) t.
   (HArrayIndex n t ts) => Proxy n -> t -> HArray ts -> HArray ts
setHArrayN _ a (HArray as) = HArray (as V.// [(idx,unsafeCoerce a)])
   where
      idx = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get an element by type (select the first one with this type)
getHArrayT :: forall t ts.
   (HArrayIndexT t ts) => HArray ts -> t
getHArrayT = getHArrayN (Proxy :: Proxy (IndexOf t ts))

-- | Set an element by type (select the first one with this type)
setHArrayT :: forall t ts.
   (HArrayIndexT t ts) => t -> HArray ts -> HArray ts
setHArrayT = setHArrayN (Proxy :: Proxy (IndexOf t ts))

-- | Get an element by type (select the first one with this type)
tryGetHArrayT :: forall t ts.
   (HArrayTryIndexT t ts) => HArray ts -> Maybe t
tryGetHArrayT as = if n == 0
      then Nothing
      else Just $ getHArrayN (Proxy :: Proxy (MaybeIndexOf t ts)) as'
   where
      n   = natVal (Proxy :: Proxy (MaybeIndexOf t ts))
      as' :: HArray (t ': ts)
      as' = prependHArray undefined as

-- | Append a value to an array (O(n))
appendHArray :: HArray ts -> t -> HArray (Concat ts '[t])
appendHArray (HArray as) a = HArray (V.snoc as (unsafeCoerce a))

-- | Prepend a value to an array (O(n))
prependHArray :: t -> HArray ts -> HArray (t ': ts)
prependHArray a (HArray as) = HArray (V.cons (unsafeCoerce a) as)

-- | Concat arrays
concatHArray :: HArray ts1 -> HArray ts2 -> HArray (Concat ts1 ts2)
concatHArray (HArray as1) (HArray as2) = HArray (V.concat [as1,unsafeCoerce as2])
