{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
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
   , Member
   , Catchable
   , MaybeCatchable
   , Liftable
   , Matchable
   , MatchableH
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

import ViperVM.Utils.Monad
import ViperVM.Utils.Maybe
import ViperVM.Utils.Types
import ViperVM.Utils.Tuple
import ViperVM.Utils.HList
import ViperVM.Utils.ContFlow
import ViperVM.Utils.Types.List

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = forall a. Variant {-# UNPACK #-} !Word a

-- | Make GHC consider `l` as a representational parameter to make coercions
-- between Variant values unsafe
type role Variant representational

instance Eq (Variant '[]) where
   (==) _ _ = True

instance
   ( Eq (Variant xs)
   , Eq x
   ) => Eq (Variant (x ': xs))
   where
      {-# INLINE (==) #-}
      (==) v1 v2 = case (pickVariant @0 v1, pickVariant @0 v2) of
         (Right a, Right b) -> a == b
         (Left as, Left bs) -> as == bs
         _                  -> False

instance Show (Variant '[]) where
   show _ = "Empty variant"

instance
   ( Show (Variant xs)
   , Show x
   ) => Show (Variant (x ': xs))
   where
      show v = case pickVariant @0 v of
         Right x -> show x
         Left xs -> show xs

-- | Set the value with the given indexed type
setVariantN :: forall (n :: Nat) (l :: [*]).
   ( KnownNat n
   ) => Index n l -> Variant l
{-# INLINE setVariantN #-}
setVariantN = Variant (natValue @n)

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
      Just x  -> Variant t (f x)

-- | Update a variant value in a Monad
updateVariantM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => (Index n l -> m (Index n l2)) -> Variant l -> m (Variant l2)
{-# INLINE updateVariantM #-}
updateVariantM f v@(Variant t a) =
   case getVariantN @n v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> f x

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

data GetValue    = GetValue
data RemoveType  = RemoveType

instance forall (n :: Nat) l l2 i r .
   ( i ~ (Variant l, HList l2)                         -- input
   , r ~ (Variant l, HList (Maybe (Index n l) ': l2)) -- result
   , KnownNat n
   ) => Apply GetValue (Proxy n,i) r where
      apply _ (_, (v,xs)) = (v, getVariantN @n v `HCons` xs)


data Found
   = FoundSame
   | FoundDifferent


instance forall (n :: Nat) l l2 r i a (same :: Nat).
   ( i ~ (Variant l, Word, Maybe Found) -- input
   , r ~ (Variant l, Word, Maybe Found) -- result
   , l2 ~ Filter a l
   , KnownNat n
   , KnownNat same
   ) => Apply RemoveType ((Proxy n,Proxy same),i) r where
      apply _ (_, (v,shift,r)) =
            case r of
               -- we already have a result: we update the shift
               Just _  -> (v, shift + same, r)
               -- we look for a result
               Nothing -> case getVariantN @n v of
                  Nothing -> (v,shift,r)
                  Just _  -> if same == 0
                     then (v, shift, Just FoundDifferent)
                     else (v, shift, Just FoundSame)
         where
            -- if (a /= Index n l), same == 0, else same == 1
            same = natValue @same

-- | a is catchable in xs
type Catchable a xs =
   ( IsMember a xs ~ 'True
   , HFoldr' RemoveType (Variant xs, Word, Maybe Found)
         (Zip (Indexes xs) (MapTest a xs))
         (Variant xs, Word, Maybe Found)
   )

-- | a may be catchable in xs
type MaybeCatchable a xs =
   ( HFoldr' RemoveType (Variant xs, Word, Maybe Found)
         (Zip (Indexes xs) (MapTest a xs))
         (Variant xs, Word, Maybe Found)
   )

-- | Extract a type from a variant. Return either the value of this type or the
-- remaining variant
catchVariant :: forall a l.
   ( MaybeCatchable a l
   ) => Variant l -> Either (Variant (Filter a l)) a
catchVariant v = case res of
      (Variant _ a, _,     Just FoundSame)      -> Right (unsafeCoerce a)
      (Variant t a, shift, Just FoundDifferent) -> Left (Variant (t-shift) a)
      _ -> error "catchVariant error" -- shouldn't happen
   where
      res :: (Variant l, Word, Maybe Found)
      res = hFoldr' RemoveType
               ((v,0,Nothing) :: (Variant l, Word, Maybe Found))
               (undefined :: HList (Zip (Indexes l) (MapTest a l)))

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


-- | Matchable as a HList
type MatchableH l =
   ( HFoldr' GetValue (Variant l, HList '[])
         (Indexes l) (Variant l, HList (MapMaybe l))
   , KnownNat (Length l)
   )

-- | Matchable as a tuple
type Matchable l t =
   ( MatchableH l
   , HTuple' (MapMaybe l) t
   )

-- | Get variant possible values in a HList of Maybe types
variantToHList :: forall l.  (MatchableH l)
   => Variant l -> HList (MapMaybe l)
variantToHList v = snd res
   where
      res :: (Variant l, HList (MapMaybe l))
      res = hFoldr' GetValue
               ((v, HNil) :: (Variant l, HList '[]))
               (undefined :: HList (Indexes l))

-- | Get variant possible values in a tuple of Maybe types
variantToTuple :: forall l t.  (Matchable l t) => Variant l -> t
variantToTuple = hToTuple' . variantToHList

-- | Retreive the last v
singleVariant :: Variant '[a] -> a
singleVariant = fromJust . getVariantN @0


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
   , HFoldr' VariantLift (Variant xs, Maybe (Variant ys))
         (Indexes xs) (Variant xs, Maybe (Variant ys))
   )

data VariantLift = VariantLift


-- | Merge a variant into another
instance forall (n :: Nat) (m :: Nat) xs ys i r x.
   ( x ~ Index n xs
   , IsMember x ys ~ 'True
   , i ~ (Variant xs, Maybe (Variant ys)) -- input
   , r ~ (Variant xs, Maybe (Variant ys)) -- output
   , x ~ Index m ys
   , m ~ IndexOf x ys
   , KnownNat m
   , KnownNat n
   ) => Apply VariantLift (Proxy n,i) r where
      apply _ (_, i) = case i of
         (_, Just _)  -> i
         (v, Nothing) -> case getVariantN @n v of
               Nothing -> (v, Nothing)
               Just a  -> (v, Just (setVariant a))



-- | Lift a variant into another
--
-- Set values to the first correspond type tag
liftVariant :: forall xs ys i r.
   ( IsSubset xs ys ~ 'True
   , i ~ (Variant xs, Maybe (Variant ys))
   , r ~ (Variant xs, Maybe (Variant ys))
   , HFoldr' VariantLift i (Indexes xs) r
   ) => Variant xs -> Variant ys
liftVariant v = s
   where
      res :: r
      res = hFoldr' VariantLift
               ((v,Nothing) :: i)
               (undefined :: HList (Indexes xs))

      Just s = snd res

-- | Convert a variant of two values in a Either
toEither :: forall a b. Variant '[a,b] -> Either b a
toEither (Variant 0 a) = Right (unsafeCoerce a)
toEither (Variant _ a) = Left (unsafeCoerce a)


class ContVariant xs where
   -- | Convert a variant into a multi-continuation
   variantToCont :: Variant xs -> ContFlow xs r

   -- | Convert a multi-continuation into a Variant
   contToVariant :: ContFlow xs (Variant xs) -> Variant xs

   -- | Convert a multi-continuation into a Variant
   contToVariantM :: Monad m => ContFlow xs (m (Variant xs)) -> m (Variant xs)

instance ContVariant '[a] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant _ a) = ContFlow $ \(Single f) ->
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
