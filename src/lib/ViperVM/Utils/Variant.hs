{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Variant type
module ViperVM.Utils.Variant
   ( Variant
   , getVariant
   , setVariant
   , updateVariant
   , updateVariantM
   , updateVariantFold
   , updateVariantFoldM
   , ReplaceAt
   , Concat
   , Length
   , MapMaybe
   , GetValue
   , Generate
   , matchVariantHList
   , matchVariant
   , getVariant0
   , getVariant1
   , getVariant2
   , getVariant3
   , getVariant4
   , getVariant5
   , setVariant0
   , setVariant1
   , setVariant2
   , setVariant3
   , setVariant4
   , setVariant5
   , liftEither
   )
where

import Data.HList.FakePrelude (ApplyAB(..))
import Data.HList.HList
import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy

import ViperVM.Utils.HList

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

getVariant0 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 0 l)
getVariant0 = getVariant (Proxy :: Proxy 0)

getVariant1 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 1 l)
getVariant1 = getVariant (Proxy :: Proxy 1)

getVariant2 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 2 l)
getVariant2 = getVariant (Proxy :: Proxy 2)

getVariant3 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 3 l)
getVariant3 = getVariant (Proxy :: Proxy 3)

getVariant4 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 4 l)
getVariant4 = getVariant (Proxy :: Proxy 4)

getVariant5 :: forall (l :: [*]). Variant l -> Maybe (TypeAt 5 l)
getVariant5 = getVariant (Proxy :: Proxy 5)

-- | Set the value with the given indexed type
setVariant :: forall (n :: Nat) l .
   (KnownNat n)
   => Proxy n -> TypeAt n l -> Variant l
setVariant _ = Variant (fromIntegral (natVal (Proxy :: Proxy n)))


setVariant0 :: forall (l :: [*]). TypeAt 0 l -> Variant l
setVariant0 = setVariant (Proxy :: Proxy 0)

setVariant1 :: forall (l :: [*]). TypeAt 1 l -> Variant l
setVariant1 = setVariant (Proxy :: Proxy 1)

setVariant2 :: forall (l :: [*]). TypeAt 2 l -> Variant l
setVariant2 = setVariant (Proxy :: Proxy 2)

setVariant3 :: forall (l :: [*]). TypeAt 3 l -> Variant l
setVariant3 = setVariant (Proxy :: Proxy 3)

setVariant4 :: forall (l :: [*]). TypeAt 4 l -> Variant l
setVariant4 = setVariant (Proxy :: Proxy 4)

setVariant5 :: forall (l :: [*]). TypeAt 5 l -> Variant l
setVariant5 = setVariant (Proxy :: Proxy 5)

-- | Lift an Either into a Variant (reversed order by convention)
liftEither :: Either a b -> Variant '[b,a]
liftEither (Left a)  = setVariant1 a
liftEither (Right b) = setVariant0 b

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

type family MapMaybe l where
   MapMaybe '[]       = '[]
   MapMaybe (x ': xs) = Maybe x ': MapMaybe xs

-- | Generate a list of Nat [n..m-1]
type family Generate (n :: Nat) (m :: Nat) where
   Generate n n = '[]
   Generate n m = Proxy n ': Generate (n+1) m

data GetValue = GetValue

instance forall (n :: Nat) l l2 i r .
   ( i ~ (Variant l, HList l2)                         -- input
   , r ~ (Variant l, HList (Maybe (TypeAt n l) ': l2)) -- result
   , KnownNat n
   ) => ApplyAB GetValue (Proxy n,i) r where
      applyAB _ (_, (v,xs)) = (v, getVariant (Proxy :: Proxy n) v `HCons` xs)

-- | Get variant possible values in a HList of Maybe types
matchVariantHList :: forall l l2 m is . 
   ( m ~ Length l
   , KnownNat m
   , l2 ~ MapMaybe l
   , is ~ Generate 0 m
   , HFoldr' GetValue (Variant l, HList '[]) is (Variant l, HList l2)
   ) => Variant l -> HList (MapMaybe l)
matchVariantHList v = snd res
   where
      res :: (Variant l, HList (MapMaybe l))
      res = hFoldr' GetValue
               ((v, HNil) :: (Variant l, HList '[]))
               (undefined :: HList is)

-- | Get variant possible values in a tuple of Maybe types
matchVariant :: forall l l2 m is t . 
   ( m ~ Length l
   , KnownNat m
   , l2 ~ MapMaybe l
   , is ~ Generate 0 m
   , HFoldr' GetValue (Variant l, HList '[]) is (Variant l, HList l2)
   , HTuple' (MapMaybe l) t
   ) => Variant l -> t
matchVariant = hToTuple' . matchVariantHList
