{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ViperVM.Utils.Vector.Generic
 ( Vector
    -- * Construction
  , fromVector
  , replicate
  , singleton
  , generate
    -- * Monadic Construction
  , generateM
    -- * Elimination
  , length
  , index
  , head
  , last
    -- * Extract subsets
  , tail
  , init
  , take
  , drop
    -- * Mapping
  , map
    -- * Monadic Mapping
  , imapM_
    -- * Folding
  , foldl'
  , foldl1'
    -- * Conversions
    -- ** Lists
  , toList
  ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import GHC.TypeLits
import Data.Proxy
import Control.DeepSeq
import Foreign.Storable
import Foreign.Ptr (castPtr)
import Prelude hiding (replicate, head, last,
                       tail, init, map, length, drop, take)

newtype Vector v (n :: Nat) a = Vector (v a)
  deriving (Show, Eq, Ord, Foldable, NFData)

instance (KnownNat n, Storable a) 
      => Storable (Vector VS.Vector n a) where
  sizeOf _ = sizeOf (undefined :: a) * fromIntegral (natVal (Proxy :: Proxy n))
  alignment _ = alignment (undefined :: a)
  peek ptr = generateM (Proxy :: Proxy n) (peekElemOff (castPtr ptr))
  poke ptr = imapM_ (pokeElemOff (castPtr ptr))

-- | Convert a 'Data.Vector.Generic.Vector' into a
-- 'Data.Vector.Generic.Sized.Vector' if it has the correct size, otherwise
-- return Nothing.
fromVector :: forall a v (n :: Nat). (KnownNat n, VG.Vector v a)
           => v a -> Maybe (Vector v n a)
fromVector v
  | n' == fromIntegral (VG.length v) = Just (Vector v)
  | otherwise                        = Nothing
  where n' = natVal (Proxy :: Proxy n)
{-# INLINE fromVector #-}

-- | /O(1)/ construct a single element vector.
singleton :: forall a v. (VG.Vector v a)
          => a -> Vector v 1 a
singleton a = Vector (VG.singleton a)
{-# INLINE singleton #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index.
generate :: forall (n :: Nat) a v. (VG.Vector v a, KnownNat n)
         => Proxy n -> (Int -> a) -> Vector v n a
generate n f = Vector (VG.generate (fromIntegral $ natVal n) f)
{-# INLINE generate #-}

-- | /O(n)/ construct a vector of the given length by applying the monadic
-- action to each index.
generateM :: forall (n :: Nat) a v m. (VG.Vector v a, KnownNat n, Monad m)
         => Proxy n -> (Int -> m a) -> m (Vector v n a)
generateM n f = Vector <$> VG.generateM (fromIntegral $ natVal n) f
{-# INLINE generateM #-}

-- | Apply a function on unsized vectors to a sized vector. The function must
-- preserve the size of the vector, this is not checked.
withVectorUnsafe :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
                 => (v a -> v b) -> Vector v n a -> Vector v n b
withVectorUnsafe f (Vector v) = Vector (f v)
{-# INLINE withVectorUnsafe #-}

-- | /O(1)/ Index safely into the vector using a type level index.
index :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
      => Vector v (m+n) a -> Proxy n -> a
index (Vector v) i = v `VG.unsafeIndex` fromIntegral (natVal i)
{-# INLINE index #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements.
take :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
     => Proxy n -> Vector v (m+n) a -> Vector v n a
take i (Vector v) = Vector (VG.take (fromIntegral $ natVal i) v)
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first n elements.
drop :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
     => Proxy n -> Vector v (m+n) a -> Vector v m a
drop i (Vector v) = Vector (VG.drop (fromIntegral $ natVal i) v)
{-# INLINE drop #-}

-- | /O(1)/ Get the length of the vector.
length :: forall a v (n :: Nat). (VG.Vector v a)
       => Vector v n a -> Int
length (Vector v) = VG.length v
{-# INLINE length #-}

-- | /O(1)/ Get the first element of a non-empty vector.
head :: forall a v (n :: Nat). (VG.Vector v a)
     => Vector v (n+1) a -> a
head (Vector v) = VG.head v
{-# INLINE head #-}

-- | /O(1)/ Get the last element of a non-empty vector.
last :: forall a v (n :: Nat). (VG.Vector v a)
     => Vector v (n+1) a -> a
last (Vector v) = VG.last v
{-# INLINE last #-}

-- | /O(1)/ Yield all but the first element of a non-empty vector without
-- copying.
tail :: forall a v (n :: Nat). (VG.Vector v a)
     => Vector v (n+1) a -> Vector v n a
tail (Vector v) = Vector (VG.tail v)
{-# INLINE tail #-}

-- | /O(1)/ Yield all but the last element of a non-empty vector without
-- copying.
init :: forall a v (n :: Nat). (VG.Vector v a)
     => Vector v (n+1) a -> Vector v n a
init (Vector v) = Vector (VG.init v)
{-# INLINE init #-}

-- | /O(n)/ Construct a vector with the same element in each position.
replicate :: forall a v (n :: Nat). (VG.Vector v a, KnownNat n)
          => a -> Vector v n a
replicate a = Vector (VG.replicate (fromIntegral $ natVal (Proxy :: Proxy n)) a)
{-# INLINE replicate #-}

-- | /O(n)/ Map a function over the vector.
map :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
    => (a -> b) -> Vector v n a -> Vector v n b
map f = withVectorUnsafe (VG.map f)
{-# INLINE map #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: forall a v n b m. (VG.Vector v a, Monad m)
       => (Int -> a -> m b) -> Vector v n a -> m ()
imapM_ f (Vector v) = VG.imapM_ f v
{-# INLINE imapM_ #-}

-- | /O(n)/ Left fold with a strict accumulator.
foldl' :: forall a b v (n :: Nat). VG.Vector v b
       => (a -> b -> a) -> a -> Vector v n b -> a
foldl' f z (Vector v) = VG.foldl' f z v
{-# INLINE foldl' #-}

-- | /O(n)/ Left fold on a non-empty vector with a strict accumulator.
foldl1' :: forall a v (n :: Nat). (VG.Vector v a)
        => (a -> a -> a) -> Vector v (n+1) a -> a
foldl1' f (Vector v) = VG.foldl1' f v
{-# INLINE foldl1' #-}

-- | /O(n)/ Convert a vector to a list
toList :: forall a v (n :: Nat). (VG.Vector v a)
     => Vector v n a -> [a]
toList (Vector v) = Vector (VG.tolist v)
{-# INLINE toList #-}

