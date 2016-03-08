{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.Format.Binary.Enum
   ( EnumField
   , fromEnumField
   , toEnumField
   )
where

import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr

newtype EnumField b a = EnumField a

instance (Storable b, Integral b, Enum a) => Storable (EnumField b a) where
   sizeOf _             = sizeOf (undefined :: b)
   alignment _          = alignment (undefined :: b)
   peek p               = (EnumField . toEnum . fromIntegral) <$> peek (castPtr p :: Ptr b)
   poke p (EnumField v) = poke (castPtr p :: Ptr b) (fromIntegral (fromEnum v))

instance (Integral b, Storable b, Enum a) => CStorable (EnumField b a) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf

fromEnumField :: Enum a => EnumField b a -> a
fromEnumField (EnumField a) = a

toEnumField :: Enum a => a -> EnumField b a
toEnumField = EnumField
