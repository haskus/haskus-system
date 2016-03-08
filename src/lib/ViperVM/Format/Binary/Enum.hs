{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Store an Enum in the given backing word type
module ViperVM.Format.Binary.Enum
   ( EnumField
   , CEnum (..)
   , fromEnumField
   , toEnumField
   )
where

import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr

-----------------------------------------------------------------------------
-- EnumField b a: directly store the value of enum "a" as a "b"
-----------------------------------------------------------------------------

newtype EnumField b a = EnumField a

instance (Storable b, Integral b, CEnum a) => Storable (EnumField b a) where
   sizeOf _             = sizeOf (undefined :: b)
   alignment _          = alignment (undefined :: b)
   peek p               = (EnumField . toCEnum) <$> peek (castPtr p :: Ptr b)
   poke p (EnumField v) = poke (castPtr p :: Ptr b) (fromCEnum v)

instance (Integral b, Storable b, CEnum a) => CStorable (EnumField b a) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf

fromEnumField :: CEnum a => EnumField b a -> a
fromEnumField (EnumField a) = a

toEnumField :: CEnum a => a -> EnumField b a
toEnumField = EnumField

-----------------------------------------------------------------------------
-- Extended Enum
-----------------------------------------------------------------------------

-- | By default, use fromEnum/toEnum to convert from/to an Integral.
--
-- But it can be overloaded so to perform transformation before using
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

