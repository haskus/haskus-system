module ViperVM.Utils.EnumSet
   ( EnumSet (..)
   , member
   , notMember
   , elems
   , toList
   , fromList
   )
where

import Data.Bits


-- | A bit set: use bitwise operations (fast!) and minimal storage (sizeOf basetype)
--
-- b is the base type (Num b, FiniteBits b)
-- a is the element type (Enum a)
newtype EnumSet b a = EnumSet b

-- | Test if an element is in the set
member :: (Bits b, Enum a) => EnumSet b a -> a -> Bool
member (EnumSet b) e = testBit b (fromEnum e)

-- | Test if an element is not in the set
notMember :: (Bits b, Enum a) => EnumSet b a -> a -> Bool
notMember b e = not (member b e)

-- | Count the trailing zeros
--
-- TODO: 12/12/14 replace this with the same function that will lend into
-- Data.Bits soon and that uses new primops (to use bsf for instance on X86). 
-- The same function name has been chosen so that a clash should occur on
-- package bounce :)
-- cf https://ghc.haskell.org/trac/ghc/ticket/9340
countTrailingZeros :: (Bits b) => b -> Int
countTrailingZeros = go 0
   where
      go n x
         | x == zeroBits = n
         | otherwise     = go (n+1) (x `shiftR` 1)


-- | Retrieve elements in the set
elems :: (Enum a, Bits b) => EnumSet b a -> [a]
elems (EnumSet b) = go [] b
   where
      go xs c
         | c == zeroBits = xs
         | otherwise     = go (x:xs) c'
            where
               e = countTrailingZeros c
               x = toEnum (1 `shiftL` e)
               c' = clearBit c e

-- | Convert a set into a list
toList :: (Enum a, Bits b) => EnumSet b a -> [a]
toList = elems

-- | Convert a list into a set
fromList :: (Enum a, Bits b) => [a] -> EnumSet b a
fromList = go zeroBits
   where
      go b []     = EnumSet b
      go b (x:xs) = go (setBit b (fromEnum x)) xs
