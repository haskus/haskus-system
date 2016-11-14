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
   , fusionVariant
   , liftVariant
   , toEither
   )
where

import Unsafe.Coerce

import ViperVM.Utils.Monad
import ViperVM.Utils.Maybe
import ViperVM.Utils.Types
import ViperVM.Utils.HList
import ViperVM.Utils.Types.List

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = forall a . Variant Word a

-- | Make GHC consider `l` as a representational parameter to make coercions
-- between Variant values unsafe
type role Variant representational

-- | Set the value with the given indexed type
setVariantN :: forall (n :: Nat) (l :: [*]). (KnownNat n)
   => TypeAt n l -> Variant l
setVariantN = Variant (natValue' @n)

-- | Get the value if it has the indexed type
getVariantN :: forall (n :: Nat) (l :: [*]). (KnownNat n)
   => Variant l -> Maybe (TypeAt n l)
getVariantN (Variant t a) = do
   guard (t == natValue' @n)
   return (unsafeCoerce a) -- we know it is the effective type

-- | Lift an Either into a Variant (reversed order by convention)
liftEither :: Either a b -> Variant '[b,a]
liftEither (Left a)  = setVariantN @1 a
liftEither (Right b) = setVariantN @0 b

-- | Lift an Either into a Variant (reversed order by convention)
liftEitherM :: (Monad m) => m (Either a b) -> m (Variant '[b,a])
liftEitherM = fmap liftEither

-- | Update a variant value
updateVariantN :: forall (n :: Nat) l l2 .
   ( KnownNat n
   ) => (TypeAt n l -> TypeAt n l2) -> Variant l -> Variant l2
{-# INLINE updateVariantN #-}
updateVariantN f v@(Variant t a) =
   case getVariantN @n v of
      Nothing -> Variant t a
      Just x  -> Variant t (f x)

-- | Update a variant value in a Monad
updateVariantM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => (TypeAt n l -> m (TypeAt n l2)) -> Variant l -> m (Variant l2)
{-# INLINE updateVariantM #-}
updateVariantM f v@(Variant t a) =
   case getVariantN @n v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> f x

-- | Update a variant value with a variant and fold the result
updateVariantFold :: forall (n :: Nat) l l2 .
   ( KnownNat n
   , KnownNat (Length l2)
   ) => Proxy n -> (TypeAt n l -> Variant l2) -> Variant l -> Variant (ReplaceAt n l l2)
updateVariantFold _ f v@(Variant t a) =
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
   ) => Proxy n -> (TypeAt n l -> m (Variant l2)) -> Variant l -> m (Variant (ReplaceAt n l l2))
updateVariantFoldM _ f v@(Variant t a) =
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
data VariantShow = VariantShow

instance forall (n :: Nat) l l2 i r .
   ( i ~ (Variant l, HList l2)                         -- input
   , r ~ (Variant l, HList (Maybe (TypeAt n l) ': l2)) -- result
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
            -- if (a /= TypeAt n l), same == 0, else same == 1
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
catchVariant :: forall l a.
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
pickVariant :: forall n l. 
   ( KnownNat n
   ) => Proxy n -> Variant l -> Either (Variant (RemoveAt n l)) (TypeAt n l)
{-# INLINE pickVariant #-}
pickVariant _ v@(Variant t a) = case getVariantN @n v of
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


-- | Showing a variant value
instance forall (n :: Nat) l i r .
   ( i ~ (Variant l, Maybe String) -- input
   , r ~ (Variant l, Maybe String) -- result
   , Show (TypeAt n l)
   , KnownNat n
   ) => Apply VariantShow (Proxy n,i) r where
      apply _ (_, i) = case i of
         (_, Just _)  -> i
         (v, Nothing) -> case getVariantN @n v of
               Nothing -> (v, Nothing)
               Just a  -> (v, Just s)
                  where
                     s = show a

instance 
   ( HFoldr' VariantShow (Variant l, Maybe String) (Indexes l) (Variant l, Maybe String)
   )
   => Show (Variant l) where
   show v = s
      where
         res :: (Variant l, Maybe String)
         res = hFoldr' VariantShow
                  ((v,Nothing) :: (Variant l, Maybe String))
                  (undefined :: HList (Indexes l))

         Just s = snd res

-- | Extend a variant by appending other possible values
appendVariant :: forall (xs :: [*]) (ys :: [*]). Proxy ys -> Variant xs -> Variant (Concat xs ys)
{-# INLINE appendVariant #-}
appendVariant _ (Variant t a) = Variant t a

-- | Extend a variant by prepending other possible values
prependVariant :: forall (xs :: [*]) (ys :: [*]).
   ( KnownNat (Length ys)
   ) => Proxy ys -> Variant xs -> Variant (Concat ys xs)
{-# INLINE prependVariant #-}
prependVariant _ (Variant t a) = Variant (n+t) a
   where
      n = natValue @(Length ys)

-- | Fusion variant values of the same type
fusionVariant :: Liftable l (Nub l) => Variant l -> Variant (Nub l)
{-# INLINE fusionVariant #-}
fusionVariant = liftVariant

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


-- | xs is liftable in ys
type Liftable xs ys =
   ( IsSubset xs ys ~ 'True
   , HFoldr' VariantLift (Variant xs, Maybe (Variant ys))
         (Indexes xs) (Variant xs, Maybe (Variant ys))
   )

data VariantLift = VariantLift


-- | Merge a variant into another
instance forall (n :: Nat) (m :: Nat) xs ys i r x.
   ( x ~ TypeAt n xs
   , IsMember x ys ~ 'True
   , i ~ (Variant xs, Maybe (Variant ys)) -- input
   , r ~ (Variant xs, Maybe (Variant ys)) -- output
   , x ~ TypeAt m ys
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
toEither v = case catchVariant v1 of
      Right x -> x
      Left _  -> undefined
   where
      v1 :: Variant '[Either b a, Either b a]
      v1 = updateVariantN @0 Right v2
      v2 :: Variant '[a, Either b a]
      v2 = updateVariantN @1 Left v
   
