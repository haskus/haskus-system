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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Typed Variant type (union)
module ViperVM.Utils.Variant
   ( Variant
   , getVariant
   , setVariant
   , setVariantN
   , updateVariant
   , updateVariantM
   , updateVariantFold
   , updateVariantFoldM
   , GetValue
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
   , updateVariant0
   , updateVariant1
   , updateVariant2
   , updateVariant3
   , updateVariant4
   , updateVariant5
   , liftEither
   , liftEitherM
   , removeType
   , pickVariant
   , headVariant
   , Found
   , RemoveType
   , singleVariant
   , appendVariant
   , prependVariant
   , fusionVariant
   , VariantExtend
   , extendVariant
   )
where

import Data.HList.FakePrelude (ApplyAB(..))
import Data.HList.HList
import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy
import Data.Maybe

import ViperVM.Utils.HList

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = forall a . Variant Int a

-- | Make GHC consider `l` as a representational parameter to make coercions
-- between Variant values unsafe
type role Variant representational

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
setVariantN :: forall (n :: Nat) l .
   (KnownNat n)
   => Proxy n -> TypeAt n l -> Variant l
setVariantN _ = Variant (fromIntegral (natVal (Proxy :: Proxy n)))


setVariant0 :: forall (l :: [*]). TypeAt 0 l -> Variant l
setVariant0 = setVariantN (Proxy :: Proxy 0)

setVariant1 :: forall (l :: [*]). TypeAt 1 l -> Variant l
setVariant1 = setVariantN (Proxy :: Proxy 1)

setVariant2 :: forall (l :: [*]). TypeAt 2 l -> Variant l
setVariant2 = setVariantN (Proxy :: Proxy 2)

setVariant3 :: forall (l :: [*]). TypeAt 3 l -> Variant l
setVariant3 = setVariantN (Proxy :: Proxy 3)

setVariant4 :: forall (l :: [*]). TypeAt 4 l -> Variant l
setVariant4 = setVariantN (Proxy :: Proxy 4)

setVariant5 :: forall (l :: [*]). TypeAt 5 l -> Variant l
setVariant5 = setVariantN (Proxy :: Proxy 5)

-- | Lift an Either into a Variant (reversed order by convention)
liftEither :: Either a b -> Variant '[b,a]
liftEither (Left a)  = setVariant1 a
liftEither (Right b) = setVariant0 b

-- | Lift an Either into a Variant (reversed order by convention)
liftEitherM :: (Monad m) => m (Either a b) -> m (Variant '[b,a])
liftEitherM = fmap liftEither

-- | Update a variant value
updateVariant :: forall (n :: Nat) l l2 .
   (KnownNat n)
   => Proxy n -> (TypeAt n l -> TypeAt n l2) -> Variant l -> Variant l2
updateVariant _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing -> Variant t a
      Just x  -> Variant t (f x)

updateVariant0 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 0 l -> TypeAt 0 l2) -> Variant l -> Variant l2
updateVariant0 = updateVariant (Proxy :: Proxy 0)

updateVariant1 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 1 l -> TypeAt 1 l2) -> Variant l -> Variant l2
updateVariant1 = updateVariant (Proxy :: Proxy 1)

updateVariant2 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 2 l -> TypeAt 2 l2) -> Variant l -> Variant l2
updateVariant2 = updateVariant (Proxy :: Proxy 2)

updateVariant3 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 3 l -> TypeAt 3 l2) -> Variant l -> Variant l2
updateVariant3 = updateVariant (Proxy :: Proxy 3)

updateVariant4 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 4 l -> TypeAt 4 l2) -> Variant l -> Variant l2
updateVariant4 = updateVariant (Proxy :: Proxy 4)

updateVariant5 :: forall (l :: [*]) (l2 :: [*]).
   (TypeAt 5 l -> TypeAt 5 l2) -> Variant l -> Variant l2
updateVariant5 = updateVariant (Proxy :: Proxy 5)

-- | Update a variant value in a Monad
updateVariantM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => Proxy n -> (TypeAt n l -> m (TypeAt n l2)) -> Variant l -> m (Variant l2)
updateVariantM _ f v@(Variant t a) =
   case getVariant (Proxy :: Proxy n) v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> f x

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

data GetValue    = GetValue
data RemoveType  = RemoveType
data VariantShow = VariantShow

instance forall (n :: Nat) l l2 i r .
   ( i ~ (Variant l, HList l2)                         -- input
   , r ~ (Variant l, HList (Maybe (TypeAt n l) ': l2)) -- result
   , KnownNat n
   ) => ApplyAB GetValue (Proxy n,i) r where
      applyAB _ (_, (v,xs)) = (v, getVariant (Proxy :: Proxy n) v `HCons` xs)


data Found
   = FoundSame
   | FoundDifferent


instance forall (n :: Nat) l l2 r i a (same :: Nat).
   ( i ~ (Variant l, Int, Maybe Found) -- input
   , r ~ (Variant l, Int, Maybe Found) -- result
   , l2 ~ Filter a l
   , KnownNat n
   , KnownNat same
   ) => ApplyAB RemoveType ((Proxy n,Proxy same),i) r where
      applyAB _ (_, (v,shift,r)) =
            case r of
               -- we already have a result: we update the shift
               Just _  -> (v, shift + same, r)
               -- we look for a result
               Nothing -> case getVariant (Proxy :: Proxy n) v of
                  Nothing -> (v,shift,r)
                  Just _  -> if same == 0
                     then (v, shift, Just FoundDifferent)
                     else (v, shift, Just FoundSame)
         where
            -- if (a /= TypeAt n l), same == 0, else same == 1
            same = fromIntegral (natVal (Proxy :: Proxy same))


removeType :: forall l a l2 r is.
   ( r ~ (Variant l, Int, Maybe Found)
   , is ~ Zip (Indexes l) (MapTest a l)
   , HFoldr' RemoveType r is r
   , l2 ~ Filter a l
   ) => Variant l -> Either a (Variant l2)
removeType v = case res of
      (Variant _ a, _,     Just FoundSame)      -> Left (unsafeCoerce a)
      (Variant t a, shift, Just FoundDifferent) -> Right (Variant (t-shift) a)
      _ -> error "removeType error" -- shouldn't happen
   where
      res :: (Variant l, Int, Maybe Found)
      res = hFoldr' RemoveType
               ((v,0,Nothing) :: r)
               (undefined :: HList (Zip (Indexes l) (MapTest a l)))

-- | Pick a variant value
pickVariant :: forall n l. 
   ( KnownNat n
   ) => Proxy n -> Variant l -> Either (Variant (RemoveAt n l)) (TypeAt n l)
pickVariant _ v@(Variant t a) = case getVariant (Proxy :: Proxy n) v of
   Just x  -> Right x
   Nothing -> Left $ if t > fromIntegral (natVal (Proxy :: Proxy n))
      then Variant (t-1) a
      else Variant t a

-- | Pick the head of a variant value
headVariant :: forall x xs. Variant (x ': xs) -> Either (Variant xs) x
headVariant v@(Variant t a) = case getVariant0 v of
   Just x  -> Right x
   Nothing -> Left $ Variant (t-1) a

-- | Get variant possible values in a HList of Maybe types
matchVariantHList :: forall l l2 m is . 
   ( m ~ Length l
   , KnownNat m
   , l2 ~ MapMaybe l
   , is ~ Indexes l
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

-- | Retreive the last v
singleVariant :: Variant '[a] -> a
singleVariant = fromJust . getVariant0


-- | Showing a variant value
instance forall (n :: Nat) l i r .
   ( i ~ (Variant l, Maybe String) -- input
   , r ~ (Variant l, Maybe String) -- result
   , Show (TypeAt n l)
   , KnownNat n
   ) => ApplyAB VariantShow (Proxy n,i) r where
      applyAB _ (_, i) = case i of
         (_, Just _)  -> i
         (v, Nothing) -> case getVariant (Proxy :: Proxy n) v of
               Nothing -> (v, Nothing)
               Just a  -> (v, Just s)
                  where
                     s = "setVariantN @" ++ show n ++ " "++show a
                     n = natVal (Proxy :: Proxy n)

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
appendVariant _ (Variant t a) = Variant t a

-- | Extend a variant by prepending other possible values
prependVariant :: forall (xs :: [*]) (ys :: [*]).
   ( KnownNat (Length ys)
   ) => Proxy ys -> Variant xs -> Variant (Concat ys xs)
prependVariant _ (Variant t a) = Variant (n+t) a
   where
      n = fromIntegral (natVal (Proxy :: Proxy (Length ys)))


-- | Fusion variant values of the same type
fusionVariant :: forall l r i.
   ( i ~ (Variant l, Maybe (Variant (Nub l)))
   , r ~ (Variant l, Maybe (Variant (Nub l)))
   , HFoldr' VariantExtend i (Indexes l) r
   ) => Variant l -> Variant (Nub l)
fusionVariant v = s
   where
      res :: r
      res = hFoldr' VariantExtend
               ((v,Nothing) :: i)
               (undefined :: HList (Indexes l))

      Just s = snd res

-- | Set the first matching type of a Variant
setVariant :: forall a l n.
   ( IsMember a l ~ 'True
   , n ~ IndexOf a l
   , a ~ TypeAt n l
   , KnownNat n
   ) => a -> Variant l
setVariant = setVariantN (Proxy :: Proxy n)


data VariantExtend = VariantExtend


-- | Merge a variant into another
instance forall (n :: Nat) (m :: Nat) xs ys i r x.
   ( i ~ (Variant xs, Maybe (Variant ys)) -- input
   , r ~ (Variant xs, Maybe (Variant ys)) -- output
   , x ~ TypeAt n xs
   , x ~ TypeAt m ys
   , m ~ IndexOf x ys
   , IsMember x ys ~ 'True
   , KnownNat m
   , KnownNat n
   ) => ApplyAB VariantExtend (Proxy n,i) r where
      applyAB _ (_, i) = case i of
         (_, Just _)  -> i
         (v, Nothing) -> case getVariant (Proxy :: Proxy n) v of
               Nothing -> (v, Nothing)
               Just a  -> (v, Just (setVariant a))


-- | Extend a variant
--
-- Set values to the first correspond type tag
extendVariant :: forall xs ys i r.
   ( i ~ (Variant xs, Maybe (Variant ys))
   , r ~ (Variant xs, Maybe (Variant ys))
   , HFoldr' VariantExtend i (Indexes xs) r
   ) => Variant xs -> Variant ys
extendVariant v = s
   where
      res :: r
      res = hFoldr' VariantExtend
               ((v,Nothing) :: i)
               (undefined :: HList (Indexes xs))

      Just s = snd res
