{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Variant type
module ViperVM.Utils.Variant
   ( Variant
   , getVariant
   , setVariant
   , updateVariant
   , updateVariantM
   , updateVariantFold
   , updateVariantFoldM
   )
where

import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = forall a . Variant Int a


type family TypeAt (n :: Nat) l where
   TypeAt 0 (x ': xs) = x
   TypeAt n (x ': xs) = TypeAt (n-1) xs

-- | Get the value if it has the indexed type
getVariant :: forall (n :: Nat) l . 
   (KnownNat n)
   => Proxy n -> Variant l -> Maybe (TypeAt n l)
getVariant _ (Variant t a) =
   if t /= fromIntegral (natVal (Proxy :: Proxy n))
      then Nothing
      else Just (unsafeCoerce a) -- we know it is the effective type


-- | Set the value with the given indexed type
setVariant :: forall (n :: Nat) l .
   (KnownNat n)
   => Proxy n -> TypeAt n l -> Variant l
setVariant _ = Variant (fromIntegral (natVal (Proxy :: Proxy n)))


-- | Update a variant value
updateVariant :: forall (n :: Nat) l l2 .
   (KnownNat n)
   => Proxy n -> (TypeAt n l -> TypeAt n l2) -> Variant l -> Variant l2
updateVariant _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing -> Variant t a
      Just x  -> Variant t (f x)

-- | Update a variant value in a Monad
updateVariantM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => Proxy n -> (TypeAt n l -> m (TypeAt n l2)) -> Variant l -> m (Variant l2)
updateVariantM _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> f x

-- | Concat two type lists
type family Concat xs ys where
   Concat '[] '[]      = '[]
   Concat '[] ys       = ys
   Concat (x ': xs) ys = x ': Concat xs ys

type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs

-- | replace l[n] with l2 (folded)
type family ReplaceAt (n :: Nat) l l2 where
   ReplaceAt 0 (x ': xs) ys = Concat ys xs
   ReplaceAt n (x ': xs) ys = x ': ReplaceAt (n-1) xs ys

-- | Update a variant value with a variant and fold the result
updateVariantFold :: forall (n :: Nat) (m :: Nat) l l2 .
   (KnownNat n, KnownNat m, m ~ Length l2)
   => Proxy n -> (TypeAt n l -> Variant l2) -> Variant l -> Variant (ReplaceAt n l l2)
updateVariantFold _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing ->
         -- we need to adapt the tag if new valid tags (from l2) are added before
         if t < n
            then Variant t a
            else Variant (t+nl2-1) a

      Just x  -> case f x of
         Variant t2 a2 -> Variant (t2+n) a2
   where
      n   = fromIntegral (natVal (Proxy :: Proxy n))
      nl2 = fromIntegral (natVal (Proxy :: Proxy m))

-- | Update a variant value with a variant and fold the result
updateVariantFoldM :: forall (n :: Nat) (k :: Nat) m l l2 .
   (KnownNat n, KnownNat k, k ~ Length l2, Monad m)
   => Proxy n -> (TypeAt n l -> m (Variant l2)) -> Variant l -> m (Variant (ReplaceAt n l l2))
updateVariantFoldM _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing ->
         -- we need to adapt the tag if new valid tags (from l2) are added before
         return $ if t < n
            then Variant t a
            else Variant (t+nl2-1) a

      Just x  -> do
         y <- f x
         case y of
            Variant t2 a2 -> return (Variant (t2+n) a2)
   where
      n   = fromIntegral (natVal (Proxy :: Proxy n))
      nl2 = fromIntegral (natVal (Proxy :: Proxy k))
