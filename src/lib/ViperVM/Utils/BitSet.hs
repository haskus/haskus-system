-- | A bit set based on Enum to name the bits. Use bitwise operations and
-- minimal storage.
--
-- Similar to Data.Bitset.Generic from bitset package, but
--    * We don't have the Num constraint
--    * We dont use the deprecated bitSize function
--    * We use countTrailingZeros instead of iterating on the
--    number of bits
--    * We add a typeclass EnumBitSet
module ViperVM.Utils.BitSet
   ( BitSet (..)
   , EnumBitSet (..)
   , null
   , empty
   , insert
   , delete
   , toBits
   , fromBits
   , member
   , notMember
   , intersection
   , union
   )
where

import Prelude hiding (null)

import Data.Bits
import Data.Foldable (foldl')

-- | Count trailing zeros. Return Nothing if all the bits are cleared
myCountTrailingZeros :: (FiniteBits b) => b -> Maybe Int
myCountTrailingZeros x = 
   if x == zeroBits
      then Nothing
      else Just (countTrailingZeros x)

-- | A bit set: use bitwise operations (fast!) and minimal storage (sizeOf
-- basetype)
--
-- b is the base type (Bits b)
-- a is the element type (Enum a)
newtype BitSet b a = BitSet b deriving (Eq,Ord)

instance (Show a, EnumBitSet a, FiniteBits b) => Show (BitSet b a) where
   show b = "fromList " ++ show (toList b)

-- | Indicate if the set is empty
null :: (FiniteBits b) => BitSet b a -> Bool
null (BitSet b) = b == zeroBits

{-# INLINE null #-}

-- | Empty bitset
empty :: (FiniteBits b) => BitSet b a
empty = BitSet zeroBits

{-# INLINE empty #-}


-- | Insert an element in the set
insert :: (Bits b, Enum a) => BitSet b a -> a -> BitSet b a
insert (BitSet b) e = BitSet $ setBit b (fromEnum e)

{-# INLINE insert #-}


-- | Remove an element from the set
delete :: (Bits b, Enum a) => BitSet b a -> a -> BitSet b a
delete (BitSet b) e = BitSet $ clearBit b (fromEnum e)

{-# INLINE delete #-}


-- | Unwrap the bitset
toBits :: BitSet b a -> b
toBits (BitSet b) = b

-- | Wrap a bitset
fromBits :: b -> BitSet b a
fromBits = BitSet

-- | Test if an element is in the set
member :: (Bits b, Enum a) => BitSet b a -> a -> Bool
member (BitSet b) e = testBit b (fromEnum e)

{-# INLINE member #-}


-- | Test if an element is not in the set
notMember :: (Bits b, Enum a) => BitSet b a -> a -> Bool
notMember b e = not (member b e)

{-# INLINE notMember #-}


-- | Retrieve elements in the set
elems :: (Enum a, FiniteBits b) => BitSet b a -> [a]
elems (BitSet b) = go b
   where
      go c = case myCountTrailingZeros c of
         Nothing -> []
         Just e  -> toEnum e : go (clearBit c e)


-- | Intersection of two sets
intersection :: Bits b => BitSet b a -> BitSet b a -> BitSet b a
intersection (BitSet b1) (BitSet b2) = BitSet (b1 .&. b2)

{-# INLINE intersection #-}

-- | Intersection of two sets
union :: Bits b => BitSet b a -> BitSet b a -> BitSet b a
union (BitSet b1) (BitSet b2) = BitSet (b1 .|. b2)

{-# INLINE union #-}


class Enum a => EnumBitSet a where
   -- | Convert a list of enum elements into a bitset Warning: b
   -- must have enough bits to store the given elements! (we don't
   -- perform any check, for performance reason)
   fromListToBits :: (Bits b, Foldable m) => m a -> b
   fromListToBits = toBits . fromList

   -- | Convert a bitset into a list of Enum elements
   toListFromBits :: (FiniteBits b) => b -> [a]
   toListFromBits = toList . BitSet

   -- | Convert a set into a list
   toList :: (FiniteBits b) => BitSet b a -> [a]
   toList = elems

   -- | Convert a Foldable into a set
   fromList :: (Bits b, Foldable m) => m a -> BitSet b a
   fromList = BitSet . foldl' f zeroBits
      where
         f bs a = setBit bs (fromEnum a)
