{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Store an Enum in the given backing word type
module ViperVM.Format.Binary.Enum
   ( EnumField
   , CEnum (..)
   , fromEnumField
   , toEnumField
   , makeEnum
   , makeEnumWithCustom
   )
where

import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr

import Data.Data

-----------------------------------------------------------------------------
-- EnumField b a: directly store the value of enum "a" as a "b"
-----------------------------------------------------------------------------

-- | Store enum 'a' as a 'b'
newtype EnumField b a = EnumField a deriving (Show,Eq)

instance
      ( Storable b
      , Integral b
      , CEnum a
      ) => Storable (EnumField b a)
   where
      sizeOf _             = sizeOfT    @b
      alignment _          = alignmentT @b
      peek p               = (EnumField . toCEnum) <$> peek (castPtr p :: Ptr b)
      poke p (EnumField v) = poke (castPtr p :: Ptr b) (fromCEnum v)

instance
      ( Integral b
      , StaticStorable b
      , CEnum a
      ) => StaticStorable (EnumField b a)
   where
      type SizeOf (EnumField b a)    = SizeOf b
      type Alignment (EnumField b a) = Alignment b
      staticPeek p                   = (EnumField . toCEnum) <$> staticPeek (castPtr p :: Ptr b)
      staticPoke p (EnumField v)     = staticPoke (castPtr p :: Ptr b) (fromCEnum v)

-- | Read an enum field
fromEnumField :: EnumField b a -> a
fromEnumField (EnumField a) = a

{-# INLINE fromEnumField #-}

-- | Create an enum field
toEnumField :: a -> EnumField b a
toEnumField = EnumField

{-# INLINE toEnumField #-}

-----------------------------------------------------------------------------
-- Extended Enum
-----------------------------------------------------------------------------

-- | By default, use fromEnum/toEnum to convert from/to an Integral.
--
-- But it can be overloaded to perform transformation before using
-- fromEnum/toEnum. E.g. if values are shifted by 1 compared to Enum values,
-- define fromCEnum = (+1) . fromIntegral . fromEnum
--
class CEnum a where
   fromCEnum         :: Integral b => a -> b
   default fromCEnum :: (Enum a, Integral b) => a -> b
   fromCEnum         = fromIntegral . fromEnum

   toCEnum         :: Integral b => b -> a
   default toCEnum :: (Enum a, Integral b) => b -> a
   toCEnum         = toEnum . fromIntegral

--
-- | Make an enum with the last constructor taking a parameter for the rest of
-- the range
--
-- E.g., data T = A | B | C | D Word8
-- makeEnumWithCustom :: Int -> T
-- makeEnumWithCustom x = case x of
--    0 -> A
--    1 -> B
--    2 -> C
--    n -> D (n - 3)
makeEnumWithCustom :: forall a i. (Data a,Integral i) => i -> a
{-# INLINE makeEnumWithCustom #-}
makeEnumWithCustom x =
   if x' < maxConstrIndex t
      then fromConstr (indexConstr t x')
      else fromConstrB (fromConstr (toConstr (x' - m)))
               (indexConstr t m)
   where
      m   = maxConstrIndex t
      x'  = fromIntegral x + 1
      t   = dataTypeOf (undefined :: a)

-- | Make an enum from a number (0 indexed)
makeEnum :: forall a i. (Data a,Integral i) => i -> a
{-# INLINE makeEnum #-}
makeEnum x =fromConstr (indexConstr t x')
   where
      x'  = fromIntegral x + 1
      t   = dataTypeOf (undefined :: a)

