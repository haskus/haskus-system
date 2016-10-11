{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Heterogeneous list utils
module ViperVM.Utils.HList
   ( HList (..)
   , hHead
   , hTail
   , hLength
   , hAppend
   , HFoldr' (..)
   , HFoldl' (..)
   , HTuple' (..)
   , Single (..)
   , Apply (..)
   , HZipList
   , hZipList
   , HFoldr
   , hFoldr
   , HFoldl
   , hFoldl
   , HReverse (..)
   )
where

import ViperVM.Utils.Types
import ViperVM.Utils.Types.List

-- | Heterogeneous list
data family HList (l :: [*])
data instance HList '[]       = HNil
data instance HList (x ': xs) = x `HCons` HList xs

infixr 2 `HCons`

deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Ord (HList '[])
deriving instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))


instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (HCons x l) = let 'H':'[':s = show l
                       in "H[" ++ show x ++
                                  (if s == "]" then s else "," ++ s)

-- | Head
hHead :: HList (e ': l) -> e
hHead (HCons x _) = x

-- | Tail
hTail :: HList (e ': l) -> HList l
hTail (HCons _ l) = l

-- | Length
hLength :: forall xs. (KnownNat (Length xs)) => HList xs -> Word
hLength _ = fromIntegral (natVal (Proxy :: Proxy (Length xs)))

class HAppendList l1 l2 where
  hAppend :: HList l1 -> HList l2 -> HList (Concat l1 l2)

instance HAppendList '[] l2 where
  hAppend HNil l = l

instance HAppendList l l' => HAppendList (x ': l) l' where
  hAppend (HCons x l) l' = HCons x (hAppend l l')


-- | Apply the function identified by the data type f from type a to type b.
class Apply f a b where
  apply :: f -> a -> b

--------------------------------------
-- Folding
--------------------------------------

class HFoldr f v (l :: [*]) r where
    hFoldr :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr f v '[] v' where
    hFoldr _ v _   = v

instance
      ( Apply f (e, r) r'
      , HFoldr f v l r
      ) => HFoldr f v (e ': l) r'
   where
      hFoldr f v (HCons x l)    = apply f (x, hFoldr f v l :: r)


-- | Like HFoldr but only use types, not values!
--
-- It allows us to foldr over a list of types, without any associated hlist of
-- values.
class HFoldr' f v (l :: [*]) r where
   hFoldr' :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr' f v '[] v' where
   hFoldr' _ v _   = v

instance
      ( Apply f (e, r) r'
      , HFoldr' f v l r
      ) => HFoldr' f v (e ': l) r'
   where
      -- compared to hFoldr, we pass undefined values instead of the values
      -- supposedly in the list (we don't have a real list associated to HList l)
      hFoldr' f v _ = apply f (undefined :: e, hFoldr' f v (undefined :: HList l) :: r)

class HFoldl f (z :: *) xs (r :: *) where
    hFoldl :: f -> z -> HList xs -> r

instance forall f z z' r x zx xs.
      ( zx ~ (z,x)
      , Apply f zx z'
      , HFoldl f z' xs r
      ) => HFoldl f z (x ': xs) r
   where
      hFoldl f z (x `HCons` xs) = hFoldl f (apply f (z,x) :: z') xs

instance (z ~ z') => HFoldl f z '[] z' where
    hFoldl _ z _ = z

-- | Like HFoldl but only use types, not values!
--
-- It allows us to foldr over a list of types, without any associated hlist of
-- values.
class HFoldl' f (z :: *) xs (r :: *) where
    hFoldl' :: f -> z -> HList xs -> r

instance forall f z z' r x zx xs.
      ( zx ~ (z,x)
      , Apply f zx z'
      , HFoldl' f z' xs r
      ) => HFoldl' f z (x ': xs) r
   where
      hFoldl' f z (_ `HCons` xs) = hFoldl' f (apply f (z,(undefined :: x)) :: z') xs

instance (z ~ z') => HFoldl' f z '[] z' where
    hFoldl' _ z _ = z



class HZipList x y l | x y -> l, l -> x y where
  hZipList   :: HList x -> HList y -> HList l
  hUnzipList :: HList l -> (HList x, HList y)

instance HZipList '[] '[] '[] where
  hZipList _ _ = HNil
  hUnzipList _ = (HNil, HNil)

instance ((x,y)~z, HZipList xs ys zs) => HZipList (x ': xs) (y ': ys) (z ': zs) where
  hZipList (HCons x xs) (HCons y ys) = (x,y) `HCons` hZipList xs ys
  hUnzipList (HCons ~(x,y) zs) = let ~(xs,ys) = hUnzipList zs in (x `HCons` xs, y `HCons` ys)


class HRevApp l1 l2 l3 | l1 l2 -> l3 where
    hRevApp :: HList l1 -> HList l2 -> HList l3

instance HRevApp '[] l2 l2 where
    hRevApp _ l = l

instance HRevApp l (x ': l') z => HRevApp (x ': l) l' z where
    hRevApp (HCons x l) l' = hRevApp l (HCons x l')



class HReverse xs sx | xs -> sx, sx -> xs where
    hReverse :: HList xs -> HList sx

instance (HRevApp xs '[] sx,
          HRevApp sx '[] xs) => HReverse xs sx where
    hReverse l = hRevApp l HNil


--------------------------------------
-- Tuple convertion
--------------------------------------

-- * Conversion to and from tuples

-- | Convert between hlists and tuples
class HTuple' v t | v -> t, t -> v where
   -- | Convert an heterogeneous list into a tuple
   hToTuple'   :: HList v -> t
   
   -- | Convert a tuple into an heterogeneous list
   hFromTuple' :: t -> HList v


instance HTuple' '[] () where
    hToTuple' HNil = ()
    hFromTuple' () = HNil

-- | Tuple of a single element
data Single a = Single a deriving (Show,Eq)

instance HTuple' '[a] (Single a) where
    hToTuple' (a `HCons` HNil) = Single a
    hFromTuple' (Single a) = a `HCons` HNil

instance HTuple' '[a,b] (a,b) where
    hToTuple' (a `HCons` b `HCons` HNil) = (a,b)
    hFromTuple' (a,b) = a `HCons` b `HCons` HNil

instance HTuple' '[a,b,c] (a,b,c) where
    hToTuple' (a `HCons` b `HCons` c `HCons` HNil) = (a,b,c)
    hFromTuple' (a,b,c) = a `HCons` b `HCons` c `HCons` HNil

instance HTuple' '[a,b,c,d] (a,b,c,d) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` HNil) = (a,b,c,d)
    hFromTuple' (a,b,c,d) = a `HCons` b `HCons` c `HCons` d `HCons` HNil

instance HTuple' '[a,b,c,d,e] (a,b,c,d,e) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil) = (a,b,c,d,e)
    hFromTuple' (a,b,c,d,e) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil

instance HTuple' '[a,b,c,d,e,f] (a,b,c,d,e,f) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil) = (a,b,c,d,e,f)
    hFromTuple' (a,b,c,d,e,f) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil

instance HTuple' '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` HNil) = (a,b,c,d,e,f,g)
    hFromTuple' (a,b,c,d,e,f,g) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` HNil

instance HTuple' '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` HNil) = (a,b,c,d,e,f,g,h)
    hFromTuple' (a,b,c,d,e,f,g,h) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` HNil

instance HTuple' '[a,b,c,d,e,f,g,h,i] (a,b,c,d,e,f,g,h,i) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` HNil) = (a,b,c,d,e,f,g,h,i)
    hFromTuple' (a,b,c,d,e,f,g,h,i) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` HNil

instance HTuple' '[a,b,c,d,e,f,g,h,i,j] (a,b,c,d,e,f,g,h,i,j) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` j `HCons` HNil) = (a,b,c,d,e,f,g,h,i,j)
    hFromTuple' (a,b,c,d,e,f,g,h,i,j) = a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` j `HCons` HNil
