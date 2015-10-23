{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- | A bit set based on Enum to name the bits. Use bitwise operations and
-- minimal storage in a safer way.
--
-- Similar to Data.Bitset.Generic from bitset package, but
--
--     * We don't have the Num constraint
--     * We dont use the deprecated bitSize function
--     * We use countTrailingZeros instead of iterating on the
--     number of bits
--     * We add a typeclass EnumBitSet
--
-- Example:
--
-- @
-- data Flag
--    = FlagXXX
--    | FlagYYY
--    | FlagWWW
--    deriving (Show,Eq,Enum)
--
-- instance 'EnumBitSet' Flag
--
-- -- Adapt the backing type, here we choose Word16
-- type Flags = 'BitSet' Word16 Flag
-- @
--
-- Then you can convert (for free) a Word16 into Flags with 'fromBits' and
-- convert back with 'toBits'.
--
-- You can check if a flag is set or not with 'member' and 'notMember' and get
-- a list of set flags with 'toList'. You can 'insert' or 'delete' flags. You
-- can also perform set operations such as 'union' and 'intersection'.
--
module ViperVM.Format.Binary.BitSet
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

import qualified GHC.Exts as Ext

import Data.Bits
import Data.Foldable (foldl')
import Foreign.Storable

-- | A bit set: use bitwise operations (fast!) and minimal storage (sizeOf
-- basetype)
--
-- b is the base type (Bits b)
-- a is the element type (Enum a)
--
-- The elements in the Enum a are flags corresponding to each bit of b starting
-- from the least-significant bit.
newtype BitSet b a = BitSet b deriving (Eq,Ord,Storable)

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
      go !c
         | c == zeroBits = []
         | otherwise     = let e = countTrailingZeros c in toEnum e : go (clearBit c e)

-- | Intersection of two sets
intersection :: Bits b => BitSet b a -> BitSet b a -> BitSet b a
intersection (BitSet b1) (BitSet b2) = BitSet (b1 .&. b2)

{-# INLINE intersection #-}

-- | Intersection of two sets
union :: Bits b => BitSet b a -> BitSet b a -> BitSet b a
union (BitSet b1) (BitSet b2) = BitSet (b1 .|. b2)

{-# INLINE union #-}


-- | Indicate that an Enum is used as flags in a BitSet.
-- Default method implementations should be enough.
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


instance (FiniteBits b, EnumBitSet a) => Ext.IsList (BitSet b a) where
   type Item (BitSet b a) = a
   fromList = fromList
   toList   = toList
