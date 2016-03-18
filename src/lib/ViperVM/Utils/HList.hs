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
{-# LANGUAGE FunctionalDependencies #-}

module ViperVM.Utils.HList
   ( HFoldr' (..)
   , HTuple' (..)
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



-- * Conversion to and from tuples (original HList only supports up to 6
-- elements)

class HTuple' v t | v -> t, t -> v where
    -- | alternatively: @hUncurry (,,,)@
    hToTuple' :: HList v -> t
    hFromTuple' :: t -> HList v

-- | @Iso (HList v) (HList v') a b@
--hTuple x = iso hToTuple hFromTuple x
--
---- | @Iso' (HList v) a@
--hTuple' x = simple (hTuple x)

instance HTuple' '[] () where
    hToTuple' HNil = ()
    hFromTuple' () = HNil

instance HTuple' '[a,b] (a,b) where
    hToTuple' (a `HCons` b `HCons` HNil) = (a,b)
    hFromTuple' (a,b) = (a `HCons` b `HCons` HNil)

instance HTuple' '[a,b,c] (a,b,c) where
    hToTuple' (a `HCons` b `HCons` c `HCons` HNil) = (a,b,c)
    hFromTuple' (a,b,c) = (a `HCons` b `HCons` c `HCons` HNil)

instance HTuple' '[a,b,c,d] (a,b,c,d) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` HNil) = (a,b,c,d)
    hFromTuple' (a,b,c,d) = (a `HCons` b `HCons` c `HCons` d `HCons` HNil)

instance HTuple' '[a,b,c,d,e] (a,b,c,d,e) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil) = (a,b,c,d,e)
    hFromTuple' (a,b,c,d,e) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil)

instance HTuple' '[a,b,c,d,e,f] (a,b,c,d,e,f) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil) = (a,b,c,d,e,f)
    hFromTuple' (a,b,c,d,e,f) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil)

instance HTuple' '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` HNil) = (a,b,c,d,e,f,g)
    hFromTuple' (a,b,c,d,e,f,g) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` HNil)

instance HTuple' '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` HNil) = (a,b,c,d,e,f,g,h)
    hFromTuple' (a,b,c,d,e,f,g,h) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` HNil)

instance HTuple' '[a,b,c,d,e,f,g,h,i] (a,b,c,d,e,f,g,h,i) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` HNil) = (a,b,c,d,e,f,g,h,i)
    hFromTuple' (a,b,c,d,e,f,g,h,i) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` HNil)

instance HTuple' '[a,b,c,d,e,f,g,h,i,j] (a,b,c,d,e,f,g,h,i,j) where
    hToTuple' (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` j `HCons` HNil) = (a,b,c,d,e,f,g,h,i,j)
    hFromTuple' (a,b,c,d,e,f,g,h,i,j) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` i `HCons` j `HCons` HNil)
