{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}

-- | A bit set based on Enum to name the bits. Use bitwise operations and
-- minimal storage in a safer way.
--
-- Similar to Data.Bitset.Generic from bitset package, but
--
--     * We don't have the Num constraint
--     * We dont use the deprecated bitSize function
--     * We use countTrailingZeros instead of iterating on the
--     number of bits
--     * We add a typeclass CBitSet
--
-- Example:
--
-- @
-- {-# LANGUAGE DeriveAnyClass #-}
-- data Flag
--    = FlagXXX
--    | FlagYYY
--    | FlagWWW
--    deriving (Show,Eq,Enum,CBitSet)
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
   ( BitSet
   , CBitSet (..)
   , null
   , empty
   , singleton
   , insert
   , delete
   , toBits
   , fromBits
   , member
   , elem
   , notMember
   , elems
   , intersection
   , union
   , unions
   , fromListToBits
   , toListFromBits
   , fromList
   , toList
   )
where

import Prelude hiding (null,elem)

import qualified GHC.Exts as Ext

import Data.Foldable (foldl')
import Foreign.Storable
import Foreign.CStorable

import ViperVM.Format.Binary.Bits

-- | A bit set: use bitwise operations (fast!) and minimal storage (sizeOf
-- basetype)
--
-- b is the base type (Bits b)
-- a is the element type (Enum a)
--
-- The elements in the Enum a are flags corresponding to each bit of b starting
-- from the least-significant bit.
newtype BitSet b a = BitSet b deriving (Eq,Ord,Storable,CStorable)

instance (Show a, CBitSet a, FiniteBits b) => Show (BitSet b a) where
   show b = "fromList " ++ show (toList b)

-- | Indicate if the set is empty
null :: (FiniteBits b) => BitSet b a -> Bool
null (BitSet b) = b == zeroBits

{-# INLINE null #-}

-- | Empty bitset
empty :: (FiniteBits b) => BitSet b a
empty = BitSet zeroBits

{-# INLINE empty #-}

-- | Create a BitSet from a single element
singleton :: (Bits b, CBitSet a) => a -> BitSet b a
singleton e = BitSet $ setBit zeroBits (toBitOffset e)

{-# INLINE singleton #-}

-- | Insert an element in the set
insert :: (Bits b, CBitSet a) => BitSet b a -> a -> BitSet b a
insert (BitSet b) e = BitSet $ setBit b (toBitOffset e)

{-# INLINE insert #-}


-- | Remove an element from the set
delete :: (Bits b, CBitSet a) => BitSet b a -> a -> BitSet b a
delete (BitSet b) e = BitSet $ clearBit b (toBitOffset e)

{-# INLINE delete #-}


-- | Unwrap the bitset
toBits :: BitSet b a -> b
toBits (BitSet b) = b

-- | Wrap a bitset
fromBits :: (CBitSet a, FiniteBits b) => b -> BitSet b a
fromBits = BitSet

-- | Test if an element is in the set
member :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
member (BitSet b) e = testBit b (toBitOffset e)

{-# INLINE member #-}

-- | Test if an element is in the set
elem :: (CBitSet a, FiniteBits b) => a -> BitSet b a -> Bool
elem e (BitSet b) = testBit b (toBitOffset e)

{-# INLINE elem #-}

-- | Test if an element is not in the set
notMember :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
notMember b e = not (member b e)

{-# INLINE notMember #-}


-- | Retrieve elements in the set
elems :: (CBitSet a, FiniteBits b) => BitSet b a -> [a]
elems (BitSet b) = go b
   where
      go !c
         | c == zeroBits = []
         | otherwise     = let e = countTrailingZeros c in fromBitOffset e : go (clearBit c e)

-- | Intersection of two sets
intersection :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
intersection (BitSet b1) (BitSet b2) = BitSet (b1 .&. b2)

{-# INLINE intersection #-}

-- | Intersection of two sets
union :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
union (BitSet b1) (BitSet b2) = BitSet (b1 .|. b2)

{-# INLINE union #-}

-- | Intersection of several sets
unions :: FiniteBits b => [BitSet b a] -> BitSet b a
unions = foldl' union empty

{-# INLINE unions #-}

-- | Bit set indexed with a
class CBitSet a where
   -- | Return the bit offset of an element
   toBitOffset         :: a -> Int
   default toBitOffset :: Enum a => a -> Int
   toBitOffset         = fromEnum

   -- | Return the value associated with a bit offset
   fromBitOffset         :: Int -> a
   default fromBitOffset :: Enum a => Int -> a
   fromBitOffset         = toEnum

-- | It can be useful to get the indexes of the set bits
instance CBitSet Int where
   toBitOffset   = id
   fromBitOffset = id
   


-- | Convert a list of enum elements into a bitset Warning: b
-- must have enough bits to store the given elements! (we don't
-- perform any check, for performance reason)
fromListToBits :: (CBitSet a, FiniteBits b, Foldable m) => m a -> b
fromListToBits = toBits . fromList

-- | Convert a bitset into a list of Enum elements
toListFromBits :: (CBitSet a, FiniteBits b) => b -> [a]
toListFromBits = toList . BitSet

-- | Convert a set into a list
toList :: (CBitSet a, FiniteBits b) => BitSet b a -> [a]
toList = elems

-- | Convert a Foldable into a set
fromList :: (CBitSet a, FiniteBits b, Foldable m) => m a -> BitSet b a
fromList = foldl' insert (BitSet zeroBits)


instance (FiniteBits b, CBitSet a) => Ext.IsList (BitSet b a) where
   type Item (BitSet b a) = a
   fromList = fromList
   toList   = toList
