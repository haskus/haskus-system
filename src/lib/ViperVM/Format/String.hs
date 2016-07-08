{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

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
import Foreign.C.Types (CChar)
import GHC.TypeLits

import ViperVM.Format.Binary.Vector as Vec

-- | Fixed-size buffer containing a CString
type CStringBuffer (n :: Nat) = Vector n CChar

-- | Convert a \0-terminal vector into a string
fromCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat) -> String
fromCStringBuffer = fmap castCCharToChar . takeWhile (/= 0) . Vec.toList

-- | Convert from a String into a \0-terminal vector
toCStringBuffer :: (KnownNat n) => String -> CStringBuffer (n :: Nat)
toCStringBuffer = Vec.fromFilledListZ (castCharToCChar '\0') . fmap castCharToCChar

-- | Empty string
emptyCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat)
emptyCStringBuffer = Vec.replicate 0
