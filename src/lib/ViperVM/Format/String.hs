{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Various String formats (C string, etc.)
module ViperVM.Format.String
   ( module Foreign.C.String
   , CChar
   -- * Fixed-size CString buffer
   , CStringBuffer
   , fromCStringBuffer
   , toCStringBuffer
   , emptyCStringBuffer
   )
where

import Foreign.C.String
import Foreign.C.Types (CChar(..))

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Vector as Vec
import ViperVM.Utils.Types

-- | Fixed-size buffer containing a CString
newtype CStringBuffer (n :: Nat)
   = CStringBuffer (Vector n Int8)
   deriving (CStorable,Storable)

instance KnownNat n => Show (CStringBuffer n) where
   show = show . fromCStringBuffer

-- | Convert a \0-terminal vector into a string
fromCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat) -> String
fromCStringBuffer (CStringBuffer v) = fmap (castCCharToChar . CChar) . takeWhile (/= 0) . Vec.toList $ v

-- | Convert from a String into a \0-terminal vector
toCStringBuffer :: (KnownNat n) => String -> CStringBuffer (n :: Nat)
toCStringBuffer s = CStringBuffer (Vec.fromFilledListZ 0 . fmap (f . castCharToCChar) $ s)
   where
      f (CChar x) = x

-- | Empty string
emptyCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat)
emptyCStringBuffer = CStringBuffer (Vec.replicate 0)
