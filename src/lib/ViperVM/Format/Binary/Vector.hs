{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


-- | Vector with size in the type
module ViperVM.Format.Binary.Vector
   ( Vector (..)
   , vectorBuffer
   , take
   , drop
   , index
   , fromList
   , fromFilledList
   , fromFilledListZ
   , toList
   , replicate
   , concat
   )
where

import Prelude hiding (replicate, head, last,
                       tail, init, map, length, drop, take, concat)
import System.IO.Unsafe (unsafePerformIO)

import qualified ViperVM.Utils.List as List
import ViperVM.Utils.Types
import ViperVM.Utils.HList
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Buffer

-- | Vector with type-checked size
data Vector (n :: Nat) a = Vector Buffer

instance (Storable a, Show a, KnownNat n) => Show (Vector n a) where
   show v = "fromList " ++ show (toList v)

-- | Return the buffer backing the vector
vectorBuffer :: Vector n a -> Buffer
vectorBuffer (Vector b) = b

-- | Offset of the i-th element in a stored vector
type family ElemOffset a i n where
   ElemOffset a i n = IfNat (i+1 <=? n)
      (i * (SizeOf a))
      (TypeError ('Text "Invalid vector index: " ':<>: 'ShowType i
                 ':$$: 'Text "Vector size: "     ':<>: 'ShowType n))

instance forall a n.
   ( KnownNat (SizeOf a * n)
   ) => StaticStorable (Vector n a) where

   type SizeOf (Vector n a)    = SizeOf a * n
   type Alignment (Vector n a) = Alignment a

   staticPeek ptr =
      Vector <$> bufferPackPtr (natValue @(SizeOf a * n)) (castPtr ptr)

   staticPoke ptr (Vector b) = bufferPoke ptr b

instance forall a n.
   ( KnownNat n
   , Storable a
   ) => Storable (Vector n a) where
   sizeOf _    = natValue @n * sizeOfT @a
   alignment _ = alignmentT @a
   peek ptr    = do
      Vector <$> bufferPackPtr (sizeOfT' @(Vector n a)) (castPtr ptr)

   poke ptr (Vector b) = bufferPoke ptr b

-- | Yield the first n elements
take :: forall n m a.
   ( KnownNat (SizeOf a * n)
   ) => Proxy n -> Vector (m+n) a -> Vector n a
{-# INLINE take #-}
take _ (Vector b) = Vector (bufferTake (natValue @(SizeOf a * n)) b)

-- | Drop the first n elements
drop :: forall n m a.
   ( KnownNat (SizeOf a * n)
   ) => Proxy n -> Vector (m+n) a -> Vector m a
{-# INLINE drop #-}
drop _ (Vector b) = Vector (bufferDrop (natValue @(SizeOf a * n)) b)

-- | /O(1)/ Index safely into the vector using a type level index.
index :: forall a i n.
   ( KnownNat (ElemOffset a i n)
   , Storable a
   ) => Proxy i -> Vector n a -> a
{-# INLINE index #-}
index _ (Vector b) = bufferPeekStorableAt b (natValue @(ElemOffset a i n))

-- | Convert a list into a vector if the number of elements matches
fromList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => [a] -> Maybe (Vector n a)
{-# INLINE fromList #-}
fromList v
   | n' /= n   = Nothing
   | n' == 0   = Just $ Vector $ emptyBuffer
   | otherwise = Just $ Vector $ bufferPackStorableList v
   where
      n' = natValue' @n
      n  = fromIntegral (List.length v)

-- | Take at most n element from the list, then use z
fromFilledList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
{-# INLINE fromFilledList #-}
fromFilledList z v = Vector $ bufferPackStorableList v'
   where
      v' = List.take (natValue @n) (v ++ repeat z)

-- | Take at most (n-1) element from the list, then use z
fromFilledListZ :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
{-# INLINE fromFilledListZ #-}
fromFilledListZ z v = fromFilledList z v'
   where
      v' = List.take (natValue @n - 1) v

-- | Convert a vector into a list
toList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => Vector n a -> [a]
{-# INLINE toList #-}
toList (Vector b)
   | n == 0    = []
   | otherwise = fmap (bufferPeekStorableAt b . (sza*)) [0..n-1]
   where
      n   = natValue @n
      sza = sizeOfT' @a

-- | Create a vector by replicating a value
replicate :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> Vector n a
{-# INLINE replicate #-}
replicate v = fromFilledList v []


data StoreVector = StoreVector -- Store a vector at the right offset

instance forall n v a r.
   ( v ~ Vector n a
   , r ~ IO (Ptr a)
   , KnownNat n
   , KnownNat (SizeOf a)
   , StaticStorable a
   , Storable a
   ) => Apply StoreVector (v, IO (Ptr a)) r where
      apply _ (v, getP) = do
         p <- getP
         let
            vsz = natValue @n
            p'  = p `indexPtr'` (-1 * vsz * sizeOfT @a)
         poke (castPtr p') v 
         return p'

type family WholeSize fs :: Nat where
   WholeSize '[]                 = 0
   WholeSize (Vector n s ': xs)  = n + WholeSize xs

-- | Concat several vectors into a single one
concat :: forall l (n :: Nat) a .
   ( n ~ WholeSize l
   , KnownNat n
   , Storable a
   , StaticStorable a
   , HFoldr StoreVector (IO (Ptr a)) l (IO (Ptr a))
   )
   => HList l -> Vector n a
concat vs = unsafePerformIO $ do
   let sz = sizeOfT @a * natValue @n
   p <- mallocBytes (fromIntegral sz) :: IO (Ptr ())
   _ <- hFoldr StoreVector (return (castPtr p `indexPtr'` sz) :: IO (Ptr a)) vs :: IO (Ptr a)
   Vector <$> bufferUnsafePackPtr (fromIntegral sz) p
