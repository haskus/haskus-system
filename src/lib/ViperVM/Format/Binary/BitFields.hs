{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module ViperVM.Format.Binary.BitFields
   ( BitFields (..)
   , EnumBitField(..)
   , getField
   , getAllFields
   , getBitRange
   )
where

import ViperVM.Format.Binary.BitOps

import Data.Bits
import Foreign.Storable

newtype BitFields b a = BitFields b deriving (Eq,Ord,Storable)

class EnumBitField a where
   getBitField :: (BitReversable b, FiniteBits b, Integral b) => a -> b -> b

getAllFields :: (BitReversable b, FiniteBits b, Integral b, EnumBitField a, Enum a) => BitFields b a -> [(a,b)]
getAllFields (BitFields b) = fmap f as
   where
      as = enumFrom (toEnum 0)
      f a = (a, getBitField a b)


getField :: (EnumBitField a, FiniteBits b, BitReversable b, Integral b) => BitFields b a -> a -> b
getField (BitFields b) a = getBitField a b
