{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


-- | Vector with size in the type
module ViperVM.Format.Binary.Vector
   ( Vector (..)
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

import GHC.TypeLits
import Data.Proxy
import Foreign.Storable
import Foreign.CStorable
import Prelude hiding (replicate, head, last,
                       tail, init, map, length, drop, take, concat)
import qualified Data.List as List
import Control.Monad(forM_)
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import ViperVM.Utils.Memory (memCopy)
import ViperVM.Utils.HList
import qualified ViperVM.Format.Binary.Storable as S
import ViperVM.Format.Binary.Word

-- | Vector with type-checked size
--
-- Use GHC type literals to avoid efficiency issues with Peano representation
-- (cf fixed-vector package).
data Vector (n :: Nat) a = Vector
   { vectorPtr    :: {-# UNPACK #-} !(ForeignPtr a)
   , vectorOffset :: {-# UNPACK #-} !Word64         -- ^ Number of elements to skip
   }

instance (Storable a, Show a, KnownNat n) => Show (Vector n a) where
   show v = "fromList " ++ show (toList v)


instance forall a n s.
   ( S.Storable a
   , s ~ (n * S.SizeOf a)
   , KnownNat s
   , KnownNat (S.SizeOf a)
   )=> S.Storable (Vector n a) where

   type SizeOf (Vector n a)    = (n * S.SizeOf a)
   type Alignment (Vector n a) = S.Alignment a

   peek ptr = do
      let sz = fromIntegral (natVal (Proxy :: Proxy s))
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \p ->
         memCopy p ptr (fromIntegral sz)
      return (Vector fp 0)

   poke ptr (Vector fp o) = do
      let
         off = fromIntegral o * fromIntegral (natVal (Proxy :: Proxy (S.SizeOf a)))
         sz = fromIntegral (natVal (Proxy :: Proxy s))
      withForeignPtr fp $ \p ->
         memCopy ptr (p `plusPtr` off) sz

-- | Offset the i-th element in a stored vector
-- Take into account the padding after each element
elemOffset :: Storable a => a -> Int -> Int
elemOffset a i = i * (sizeA + padding)
   where
     sizeA   = sizeOf a
     alignA  = alignment a
     padding = case sizeA `mod` alignA of
        0 -> 0
        x -> alignA - x

instance (KnownNat n, Storable a) => Storable (Vector n a) where
   sizeOf _    = elemOffset (undefined :: a)
                     (fromIntegral (natVal (Proxy :: Proxy n)))
   alignment _ = alignment (undefined :: a)
   peek ptr    = do
      let n = fromIntegral (natVal (Proxy :: Proxy n))
      fp <- mallocForeignPtrArray n
      withForeignPtr fp $ \p ->
         memCopy p ptr
            (fromIntegral (sizeOf (undefined :: Vector n a)))
      return (Vector fp 0)

   poke ptr v@(Vector fp o) = do
      let
         off = fromIntegral (elemOffset (undefined :: a) (fromIntegral o))
      withForeignPtr fp $ \p ->
         memCopy ptr (p `plusPtr` off)
            (fromIntegral (sizeOf v))

instance (KnownNat n, Storable a) => CStorable (Vector n a) where
   cSizeOf      = sizeOf
   cAlignment   = alignment
   cPeek        = peek
   cPoke        = poke

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements.
take :: (KnownNat n, KnownNat m) => Proxy n -> Vector (m+n) a -> Vector n a
take _ (Vector fp o) = Vector fp o -- the size only changes in the type
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first n elements.
drop :: (KnownNat n, KnownNat m) => Proxy n -> Vector (m+n) a -> Vector m a
drop n (Vector fp o) = Vector fp (o + fromIntegral (natVal n))
{-# INLINE drop #-}

-- | /O(1)/ Index safely into the vector using a type level index.
index :: forall a (n :: Nat) (m :: Nat) .
   ( KnownNat n
   , KnownNat m
   , Storable a
   , CmpNat n m ~ 'LT
   ) => Proxy n -> Vector m a -> a
index n (Vector fp o) = unsafePerformIO $ withForeignPtr fp $ \p ->
   peekByteOff p (elemOffset (undefined :: a)
      (fromIntegral o + fromIntegral (natVal n)))
{-# INLINE index #-}

-- | Convert a list into a vector
fromList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => [a] -> Maybe (Vector n a)
fromList v
   | n' /= n   = Nothing
   | otherwise = Just $ unsafePerformIO $ do
         fp <- mallocForeignPtrBytes (sizeOf (undefined :: Vector n a))
         withForeignPtr fp $ \p ->
            forM_ (v `zip` [0..]) $ \(e,i) ->
               pokeByteOff p (elemOffset e i) e
         return (Vector fp 0)
   where
      n' = natVal (Proxy :: Proxy n)
      n  = fromIntegral (List.length v)
{-# INLINE fromList #-}

-- | Take at most n element from the list, then use z
fromFilledList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
fromFilledList z v = unsafePerformIO $ do
   let 
      n' = fromIntegral (natVal (Proxy :: Proxy n))
      n  = List.length v
   fp <- mallocForeignPtrBytes (sizeOf (undefined :: Vector n a))
   withForeignPtr fp $ \p -> do
      forM_ (v `zip` [0..min (n-1) (n'-1)]) $ \(e,i) ->
         pokeByteOff p (elemOffset e i) e
      forM_ [n..n'-1] $ \i ->
         pokeByteOff p (elemOffset z i) z
   return (Vector fp 0)
{-# INLINE fromFilledList #-}

-- | Take at most (n-1) element from the list, then use z
fromFilledListZ :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
fromFilledListZ z v = unsafePerformIO $ do
   let 
      n' = fromIntegral (natVal (Proxy :: Proxy n))
      n  = List.length v
   fp <- mallocForeignPtrBytes (sizeOf (undefined :: Vector n a))
   withForeignPtr fp $ \p -> do
      forM_ (v `zip` [0.. min (n-1) (n'-2)]) $ \(e,i) ->
         pokeByteOff p (elemOffset e i) e
      forM_ [min n (n'-1)..n'-1] $ \i ->
         pokeByteOff p (elemOffset z i) z
   return (Vector fp 0)
{-# INLINE fromFilledListZ #-}

-- | Convert a vector into a list
toList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => Vector n a -> [a]
toList (Vector fp o) = unsafePerformIO $ withForeignPtr fp $ \p ->
      return (go (p `plusPtr` elemOffset (undefined :: a) (fromIntegral o)) n)
   where
      n      = natVal (Proxy :: Proxy n)
      off    = elemOffset (undefined :: a) 1
      go _ 0 = []
      go p i = unsafePerformIO (peek p) : go (p `plusPtr` off) (i-1)
{-# INLINE toList #-}

-- | Create a vector by replicating a value
replicate :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> Vector n a
replicate v = unsafePerformIO $ do
   let n = fromIntegral (natVal (Proxy :: Proxy n))
   fp <- mallocForeignPtrBytes (sizeOf (undefined :: Vector n a))
   withForeignPtr fp $ \p ->
      forM_ [0..n] $ \i ->
         pokeByteOff p (elemOffset v i) v
   return (Vector fp 0)
{-# INLINE replicate #-}


data SizeVectors = SizeVectors -- Get the cumulated size of a list of vector
data StoreVector = StoreVector -- Store a vector at the right offset

instance forall v r .
   ( r ~ Int
   , Storable v
   ) => ApplyAB SizeVectors (v, Int) r where
      applyAB _ (_, sz) = sz + sizeOf (undefined :: v)

instance forall (n :: Nat) v a r .
   ( v ~ Vector n a
   , r ~ IO (Ptr a)
   , KnownNat n
   , Storable a
   ) => ApplyAB StoreVector (v, IO (Ptr a)) r where
      applyAB _ (v, getP) = do
         p <- getP
         let
            vsz = fromIntegral (natVal (Proxy :: Proxy n))
            p'  = p `plusPtr` (-1 * vsz * sizeOf (undefined :: a))
         poke (castPtr p') v 
         return p'

type family WholeSize fs :: Nat where
   WholeSize '[]                 = 0
   WholeSize (Vector n s ': xs)  = n + WholeSize xs

wholeSize :: forall l .
   ( HFoldr' SizeVectors Int l Int
   ) => HList l -> Int
wholeSize = hFoldr' SizeVectors (0 :: Int)

-- | Concat several vectors into a single one
concat :: forall l (n :: Nat) a .
   ( WholeSize l ~ n
   , KnownNat n
   , Storable a
   , HFoldr StoreVector (IO (Ptr a)) l (IO (Ptr a))
   , HFoldr' SizeVectors Int l Int
   )
   => HList l -> Vector n a
concat vs = unsafePerformIO $ do
   let sz = wholeSize vs
   fp <- mallocForeignPtrBytes sz :: IO (ForeignPtr a)
   _  <- withForeignPtr fp $ \p ->
      hFoldr StoreVector (return (p `plusPtr` sz) :: IO (Ptr a)) vs :: IO (Ptr a)

   return (Vector fp 0)
