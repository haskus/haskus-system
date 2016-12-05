{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Typed Variant type (union)
module ViperVM.Utils.Variant
   ( Variant
   , getVariantN
   , setVariantN
   , updateVariantN
   , setVariant
   , getVariant
   , updateVariant
   , updateVariantM
   , updateVariantFold
   , updateVariantFoldM
   , variantToHList
   , variantToTuple
   , liftEither
   , liftEitherM
   , variantRemoveType
   , Member
   , Catchable
   , MaybeCatchable
   , Liftable
   , catchVariant
   , pickVariant
   , headVariant
   , singleVariant
   , appendVariant
   , prependVariant
   , liftVariant
   , toEither
   , ContVariant (..)
   )
where

import Unsafe.Coerce
import GHC.Exts (Any)

import ViperVM.Utils.Monad
import ViperVM.Utils.Types
import ViperVM.Utils.Tuple
import ViperVM.Utils.HList
import ViperVM.Utils.ContFlow
import ViperVM.Utils.Types.List

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = Variant {-# UNPACK #-} !Word Any

-- | Make GHC consider `l` as a representational parameter to make coercions
-- between Variant values unsafe
type role Variant representational

instance Eq (Variant '[]) where
   (==) = error "Empty variant"

instance
   ( Eq (Variant xs)
   , Eq x
   ) => Eq (Variant (x ': xs))
   where
      {-# INLINE (==) #-}
      (==) v1@(Variant t1 _) v2@(Variant t2 _)
         | t1 /= t2  = False
         | otherwise = case (headVariant v1, headVariant v2) of
            (Right a, Right b) -> a == b
            (Left as, Left bs) -> as == bs
            _                  -> False

instance Ord (Variant '[]) where
   compare = error "Empty variant"

instance
   ( Ord (Variant xs)
   , Ord x
   ) => Ord (Variant (x ': xs))
   where
      compare v1 v2 = case (headVariant v1, headVariant v2) of
         (Right a, Right b) -> compare a b
         (Left as, Left bs) -> compare as bs
         (Right _, Left _)  -> LT
         (Left _, Right _)  -> GT

instance Show (Variant '[]) where
   show = error "Empty variant"

instance
   ( Show (Variant xs)
   , Show x
   ) => Show (Variant (x ': xs))
   where
      show v = case headVariant v of
         Right x -> show x
         Left xs -> show xs

-- | Set the value with the given indexed type
setVariantN :: forall (n :: Nat) (l :: [*]).
   ( KnownNat n
   ) => Index n l -> Variant l
{-# INLINE setVariantN #-}
setVariantN a = Variant (natValue @n) (unsafeCoerce a)

-- | Get the value if it has the indexed type
getVariantN :: forall (n :: Nat) (l :: [*]).
   ( KnownNat n
   ) => Variant l -> Maybe (Index n l)
{-# INLINE getVariantN #-}
getVariantN (Variant t a) = do
   guard (t == natValue @n)
   return (unsafeCoerce a) -- we know it is the effective type

-- | Lift an Either into a Variant (reversed order by convention)
liftEither :: Either a b -> Variant '[b,a]
{-# INLINE liftEither #-}
liftEither (Left a)  = setVariantN @1 a
liftEither (Right b) = setVariantN @0 b

-- | Lift an Either into a Variant (reversed order by convention)
liftEitherM :: (Monad m) => m (Either a b) -> m (Variant '[b,a])
liftEitherM = fmap liftEither

-- | Update a variant value
updateVariantN :: forall (n :: Nat) a b l.
   ( KnownNat n
   , a ~ Index n l
   ) => (a -> b) -> Variant l -> Variant (ReplaceN n b l)
{-# INLINE updateVariantN #-}
updateVariantN f v@(Variant t a) =
   case getVariantN @n v of
      Nothing -> Variant t a
      Just x  -> Variant t (unsafeCoerce (f x))

-- | Update a variant value in a Monad
updateVariantM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => (Index n l -> m (Index n l2)) -> Variant l -> m (Variant l2)
{-# INLINE updateVariantM #-}
updateVariantM f v@(Variant t a) =
   case getVariantN @n v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> unsafeCoerce (f x)

-- | Update a variant value with a variant and fold the result
updateVariantFold :: forall (n :: Nat) l l2 .
   ( KnownNat n
   , KnownNat (Length l2)
   ) => (Index n l -> Variant l2) -> Variant l -> Variant (ReplaceAt n l l2)
updateVariantFold f v@(Variant t a) =
   case getVariantN @n v of
      Nothing ->
         -- we need to adapt the tag if new valid tags (from l2) are added before
         if t < n
            then Variant t a
            else Variant (t+nl2-1) a

      Just x  -> case f x of
         Variant t2 a2 -> Variant (t2+n) a2
   where
      n   = natValue @n
      nl2 = natValue @(Length l2)

-- | Update a variant value with a variant and fold the result
updateVariantFoldM :: forall (n :: Nat) m l l2.
   ( KnownNat n
   , KnownNat (Length l2)
   , Monad m
   ) => (Index n l -> m (Variant l2)) -> Variant l -> m (Variant (ReplaceAt n l l2))
updateVariantFoldM f v@(Variant t a) =
   case getVariantN @n v of
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
      n   = natValue @n
      nl2 = natValue @(Length l2)


class VariantToHList xs where
   -- | Convert a variant into a HList of Maybes
   variantToHList :: Variant xs -> HList (MapMaybe xs)

instance VariantToHList '[] where
   variantToHList _ = HNil

instance
   ( VariantToHList xs
   ) => VariantToHList (x ': xs)
   where
      variantToHList v@(Variant t a) =
            getVariantN @0 v `HCons` variantToHList v'
         where
            v' :: Variant xs
            v' = Variant (t-1) a


class VariantRemoveType a xs where
   -- | Remove a type from a variant
   variantRemoveType :: Variant xs -> Either (Variant (Filter a xs)) a

instance VariantRemoveType a '[] where
   variantRemoveType _ = undefined

instance forall a xs n xs' y ys.
      ( VariantRemoveType a xs'
      , n ~ MaybeIndexOf a xs
      , xs' ~ RemoveAt1 n xs
      , Filter a xs' ~ Filter a xs
      , KnownNat n
      , xs ~ (y ': ys)
      ) => VariantRemoveType a (y ': ys)
   where
      {-# INLINE variantRemoveType #-}
      variantRemoveType (Variant t a)
         = case natValue' @n of
            0             -> Left (Variant t a) -- no 'a' left in xs
            n | n-1 == t  -> Right (unsafeCoerce a)
              | n-1 < t   -> variantRemoveType @a @xs' (Variant (t-1) a)
              | otherwise -> Left (Variant t a)

-- | a is catchable in xs
type Catchable a xs =
   ( IsMember a xs ~ 'True
   , VariantRemoveType a xs
   )

-- | a may be catchable in xs
type MaybeCatchable a xs =
   ( VariantRemoveType a xs
   )

-- | Extract a type from a variant. Return either the value of this type or the
-- remaining variant
catchVariant :: forall a xs.
   ( MaybeCatchable a xs
   ) => Variant xs -> Either (Variant (Filter a xs)) a
catchVariant v = variantRemoveType @a v

-- | Pick a variant value
pickVariant :: forall (n :: Nat) l. 
   ( KnownNat n
   ) => Variant l -> Either (Variant (RemoveAt n l)) (Index n l)
{-# INLINE pickVariant #-}
pickVariant v@(Variant t a) = case getVariantN @n v of
   Just x  -> Right x
   Nothing -> Left $ if t > natValue @n
      then Variant (t-1) a
      else Variant t a

-- | Pick the head of a variant value
headVariant :: forall x xs. Variant (x ': xs) -> Either (Variant xs) x
{-# INLINE headVariant #-}
headVariant v@(Variant t a) = case getVariantN @0 v of
   Just x  -> Right x
   Nothing -> Left $ Variant (t-1) a


-- | Get variant possible values in a tuple of Maybe types
variantToTuple :: forall l t.
   ( VariantToHList l
   , HTuple' (MapMaybe l) t
   ) => Variant l -> t
variantToTuple = hToTuple' . variantToHList

-- | Retreive the last v
singleVariant :: Variant '[a] -> a
{-# INLINE singleVariant #-}
singleVariant (Variant _ a) = unsafeCoerce a


-- | Extend a variant by appending other possible values
appendVariant :: forall (ys :: [*]) (xs :: [*]). Variant xs -> Variant (Concat xs ys)
{-# INLINE appendVariant #-}
appendVariant (Variant t a) = Variant t a

-- | Extend a variant by prepending other possible values
prependVariant :: forall (ys :: [*]) (xs :: [*]).
   ( KnownNat (Length ys)
   ) => Variant xs -> Variant (Concat ys xs)
{-# INLINE prependVariant #-}
prependVariant (Variant t a) = Variant (n+t) a
   where
      n = natValue @(Length ys)

-- | Set the first matching type of a Variant
setVariant :: forall a l.
   ( Member a l
   ) => a -> Variant l
{-# INLINE setVariant #-}
setVariant = setVariantN @(IndexOf a l)

-- | Set the first matching type of a Variant
getVariant :: forall a l.
   ( Member a l
   ) => Variant l -> Maybe a
{-# INLINE getVariant #-}
getVariant = getVariantN @(IndexOf a l)

-- | Update a variant value
updateVariant :: forall a b n l.
   ( Member a l
   , n ~ IndexOf a l
   ) => (a -> b) -> Variant l -> Variant (ReplaceN n b l)
{-# INLINE updateVariant #-}
updateVariant f v = updateVariantN @n f v


-- | xs is liftable in ys
type Liftable xs ys =
   ( IsSubset xs ys ~ 'True
   , VariantLift xs ys
   )


class VariantLift xs ys where
   liftVariant' :: Variant xs -> Variant ys

instance VariantLift '[] ys where
   liftVariant' = error "Lifting empty variant"

instance forall x xs ys.
      ( VariantLift xs ys
      , KnownNat (IndexOf x ys)
      ) => VariantLift (x ': xs) ys
   where
      {-# INLINE liftVariant' #-}
      liftVariant' v = case headVariant v of
         Right a  -> Variant (natValue @(IndexOf x ys)) (unsafeCoerce a)
         Left  v' -> liftVariant' v'


-- | Lift a variant into another
--
-- Set values to the first matching type
liftVariant :: forall xs ys.
   ( Liftable xs ys
   ) => Variant xs -> Variant ys
{-# INLINE liftVariant #-}
liftVariant = liftVariant'

-- | Convert a variant of two values in a Either
toEither :: forall a b. Variant '[a,b] -> Either b a
toEither (Variant 0 a) = Right (unsafeCoerce a)
toEither (Variant _ a) = Left (unsafeCoerce a)


class ContVariant xs where
   -- | Convert a variant into a multi-continuation
   variantToCont :: Variant xs -> ContFlow xs r

   -- | Convert a variant into a multi-continuation
   variantToContM :: Monad m => m (Variant xs) -> ContFlow xs (m r)

   -- | Convert a multi-continuation into a Variant
   contToVariant :: ContFlow xs (Variant xs) -> Variant xs

   -- | Convert a multi-continuation into a Variant
   contToVariantM :: Monad m => ContFlow xs (m (Variant xs)) -> m (Variant xs)

instance ContVariant '[a] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant _ a) = ContFlow $ \(Single f) ->
      f (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(Single f) -> do
      Variant _ a <- act
      f (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      Single (setVariantN @0)

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      Single (return . setVariantN @0)

instance ContVariant '[a,b] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         _ -> f2 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         _ -> f2 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      )

instance ContVariant '[a,b,c] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         _ -> f3 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         _ -> f3 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      )

instance ContVariant '[a,b,c,d] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         _ -> f4 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         _ -> f4 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      )

instance ContVariant '[a,b,c,d,e] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         _ -> f5 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         _ -> f5 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      )

instance ContVariant '[a,b,c,d,e,f] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         _ -> f6 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         _ -> f6 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      )

instance ContVariant '[a,b,c,d,e,f,g] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         _ -> f7 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         _ -> f7 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      )

instance ContVariant '[a,b,c,d,e,f,g,h] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         _ -> f8 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         _ -> f8 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      , setVariantN @7
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      , return . setVariantN @7
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         7 -> f8 (unsafeCoerce a)
         _ -> f9 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         7 -> f8 (unsafeCoerce a)
         _ -> f9 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      , setVariantN @7
      , setVariantN @8
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      , return . setVariantN @7
      , return . setVariantN @8
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10) ->
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         _ -> f10 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10) -> do
      Variant t a <- act
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         _ -> f10 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      , setVariantN @7
      , setVariantN @8
      , setVariantN @9
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      , return . setVariantN @7
      , return . setVariantN @8
      , return . setVariantN @9
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j,k] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11) ->
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         9 -> f10 (unsafeCoerce a)
         _ -> f11 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11) -> do
      Variant t a <- act
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         9 -> f10 (unsafeCoerce a)
         _ -> f11 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      , setVariantN @7
      , setVariantN @8
      , setVariantN @9
      , setVariantN @10
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      , return . setVariantN @7
      , return . setVariantN @8
      , return . setVariantN @9
      , return . setVariantN @10
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j,k,l] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12) ->
      case t of
         0  -> f1  (unsafeCoerce a)
         1  -> f2  (unsafeCoerce a)
         2  -> f3  (unsafeCoerce a)
         3  -> f4  (unsafeCoerce a)
         4  -> f5  (unsafeCoerce a)
         5  -> f6  (unsafeCoerce a)
         6  -> f7  (unsafeCoerce a)
         7  -> f8  (unsafeCoerce a)
         8  -> f9  (unsafeCoerce a)
         9  -> f10 (unsafeCoerce a)
         10 -> f11 (unsafeCoerce a)
         _  -> f12 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12) -> do
      Variant t a <- act
      case t of
         0  -> f1  (unsafeCoerce a)
         1  -> f2  (unsafeCoerce a)
         2  -> f3  (unsafeCoerce a)
         3  -> f4  (unsafeCoerce a)
         4  -> f5  (unsafeCoerce a)
         5  -> f6  (unsafeCoerce a)
         6  -> f7  (unsafeCoerce a)
         7  -> f8  (unsafeCoerce a)
         8  -> f9  (unsafeCoerce a)
         9  -> f10 (unsafeCoerce a)
         10 -> f11 (unsafeCoerce a)
         _  -> f12 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( setVariantN @0
      , setVariantN @1
      , setVariantN @2
      , setVariantN @3
      , setVariantN @4
      , setVariantN @5
      , setVariantN @6
      , setVariantN @7
      , setVariantN @8
      , setVariantN @9
      , setVariantN @10
      , setVariantN @11
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . setVariantN @0
      , return . setVariantN @1
      , return . setVariantN @2
      , return . setVariantN @3
      , return . setVariantN @4
      , return . setVariantN @5
      , return . setVariantN @6
      , return . setVariantN @7
      , return . setVariantN @8
      , return . setVariantN @9
      , return . setVariantN @10
      , return . setVariantN @11
      )
