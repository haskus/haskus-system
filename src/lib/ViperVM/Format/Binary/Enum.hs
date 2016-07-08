{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Store an Enum in the given backing word type
module ViperVM.Format.Binary.Enum
   ( EnumField
   , CEnum (..)
   , fromEnumField
   , toEnumField
   )
where

import qualified ViperVM.Format.Binary.Storable as S
import ViperVM.Format.Binary.Ptr
import Foreign.Storable
import Foreign.CStorable

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
      sizeOf _             = sizeOf (undefined :: b)
      alignment _          = alignment (undefined :: b)
      peek p               = (EnumField . toCEnum) <$> peek (castPtr p :: Ptr b)
      poke p (EnumField v) = poke (castPtr p :: Ptr b) (fromCEnum v)

instance
      ( Integral b
      , Storable b
      , CEnum a
      ) => CStorable (EnumField b a)
   where
      cPeek      = peek
      cPoke      = poke
      cAlignment = alignment
      cSizeOf    = sizeOf

instance
      ( Integral b
      , S.Storable b
      , CEnum a
      ) => S.Storable (EnumField b a)
   where
      type SizeOf (EnumField b a)    = S.SizeOf b
      type Alignment (EnumField b a) = S.Alignment b
      peek p               = (EnumField . toCEnum) <$> S.peek (castPtr p :: Ptr b)
      poke p (EnumField v) = S.poke (castPtr p :: Ptr b) (fromCEnum v)

-- | Read an enum field
fromEnumField :: CEnum a => EnumField b a -> a
fromEnumField (EnumField a) = a

{-# INLINE fromEnumField #-}

-- | Create an enum field
toEnumField :: CEnum a => a -> EnumField b a
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

