{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}


-- | Vector utils
--
-- Taken from the vector-sized package (0.2.0.0). Added:
--    * toList method
--    * CStorable instance

module ViperVM.Utils.Vector.Storable
 ( Vector
    -- * Construction
  , fromVector
  , replicate
  , singleton
  , generate
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
    -- * Folding
  , foldl'
  , foldl1'
    -- * Conversions
    -- ** Lists
  , toList
  ) where

import qualified ViperVM.Utils.Vector.Generic as VGS
import qualified Data.Vector.Storable as VS
import GHC.TypeLits
import Data.Proxy
import Foreign.Storable
import Prelude hiding (replicate, head, last,
                       tail, init, map, length, drop, take)

type Vector = VGS.Vector VS.Vector

-- | Convert a 'Data.Vector.Generic.Vector' into a
-- 'Data.Vector.Generic.Sized.Vector' if it has the correct size, otherwise
-- return Nothing.
fromVector :: forall a (n :: Nat). (KnownNat n, Storable a)
           => VS.Vector a -> Maybe (Vector n a)
fromVector = VGS.fromVector
{-# INLINE fromVector #-}

-- | /O(1)/ construct a single element vector.
singleton :: forall a. Storable a
          => a -> Vector 1 a
singleton = VGS.singleton
{-# INLINE singleton #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index.
generate :: forall (n :: Nat) a. (Storable a, KnownNat n)
         => Proxy n -> (Int -> a) -> Vector n a
generate = VGS.generate
{-# INLINE generate #-}

-- | /O(1)/ Index safely into the vector using a type level index.
index :: forall (m :: Nat) a (n :: Nat). (KnownNat n, KnownNat m, Storable a)
      => Vector (m+n) a -> Proxy n -> a
index = VGS.index
{-# INLINE index #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements.
take :: forall (m :: Nat) a (n :: Nat). (KnownNat n, KnownNat m, Storable a)
     => Proxy n -> Vector (m+n) a -> Vector n a
take = VGS.take
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first n elements.
drop :: forall (m :: Nat) a (n :: Nat). (KnownNat n, KnownNat m, Storable a)
     => Proxy n -> Vector (m+n) a -> Vector m a
drop = VGS.drop
{-# INLINE drop #-}

-- | /O(1)/ Get the length of the vector.
length :: forall a (n :: Nat). (Storable a)
       => Vector n a -> Int
length = VGS.length
{-# INLINE length #-}

-- | /O(1)/ Get the first element of a non-empty vector.
head :: forall a (n :: Nat). (Storable a)
     => Vector (n+1) a -> a
head = VGS.head
{-# INLINE head #-}

-- | /O(1)/ Get the last element of a non-empty vector.
last :: forall a (n :: Nat). (Storable a)
     => Vector (n+1) a -> a
last = VGS.last
{-# INLINE last #-}

-- | /O(1)/ Yield all but the first element of a non-empty vector without
-- copying.
tail :: forall a (n :: Nat). (Storable a)
     => Vector (n+1) a -> Vector n a
tail = VGS.tail
{-# INLINE tail #-}

-- | /O(1)/ Yield all but the last element of a non-empty vector without
-- copying.
init :: forall a (n :: Nat). (Storable a)
     => Vector (n+1) a -> Vector n a
init = VGS.init
{-# INLINE init #-}

-- | /O(n)/ Construct a vector with the same element in each position.
replicate :: forall a (n :: Nat). (Storable a, KnownNat n)
          => a -> Vector n a
replicate = VGS.replicate
{-# INLINE replicate #-}

-- | /O(n)/ Map a function over the vector.
map :: forall a b (n :: Nat). (Storable a, Storable b)
    => (a -> b) -> Vector n a -> Vector n b
map = VGS.map
{-# INLINE map #-}

-- | /O(n)/ Left fold with a strict accumulator.
foldl' :: forall a b (n :: Nat). Storable b
       => (a -> b -> a) -> a -> Vector n b -> a
foldl' = VGS.foldl'
{-# INLINE foldl' #-}

-- | /O(n)/ Left fold on a non-empty vector with a strict accumulator.
foldl1' :: forall a (n :: Nat). Storable a
        => (a -> a -> a) -> Vector (n+1) a -> a
foldl1' = VGS.foldl1'
{-# INLINE foldl1' #-}

-- | /O(n)/ Convert a vector to a list
toList :: forall a v (n :: Nat). Storable a
     => Vector n a -> [a]
toList = VGs.tolist
{-# INLINE toList #-}

