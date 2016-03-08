{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


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
   )
where

import GHC.TypeLits
import Data.Proxy
import Data.Word
import Foreign.Storable
import Foreign.CStorable
import Prelude hiding (replicate, head, last,
                       tail, init, map, length, drop, take)
import qualified Data.List as List
import Control.Monad(forM_)
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import ViperVM.Utils.Unsafe (inlinePerformIO)
import ViperVM.Utils.Memory (memCopy)

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
   ) => Vector (m+n) a -> Proxy n -> a
index (Vector fp o) n = inlinePerformIO $ withForeignPtr fp $ \p ->
   peekByteOff p (elemOffset (undefined :: a)
      (fromIntegral o + fromIntegral (natVal n)))
{-# INLINE index #-}

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
      forM_ (v `zip` [0..n-1]) $ \(e,i) ->
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
      forM_ (v `zip` [0..n-2]) $ \(e,i) ->
         pokeByteOff p (elemOffset e i) e
      forM_ [n-1..n'-1] $ \i ->
         pokeByteOff p (elemOffset z i) z
   return (Vector fp 0)
{-# INLINE fromFilledListZ #-}

toList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => Vector n a -> [a]
toList (Vector fp o) = inlinePerformIO $ withForeignPtr fp $ \p ->
      return (go (p `plusPtr` elemOffset (undefined :: a) (fromIntegral o)) n)
   where
      n      = natVal (Proxy :: Proxy n)
      off    = elemOffset (undefined :: a) 1
      go _ 0 = []
      go p i = inlinePerformIO (peek p) : go (p `plusPtr` off) (i-1)
{-# INLINE toList #-}

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
