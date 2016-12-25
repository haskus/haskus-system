{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

-- | Unsigned and signed words
module Haskus.Format.Binary.Word
   ( Int8
   , Int16
   , Int32
   , Int64
   , BitSize
   , WordAtLeast
   -- * Some C types
   , CSize(..)
   , CUShort
   , CShort
   , CUInt
   , CInt
   , CULong
   , CLong
   -- * Unlifted
   , module GHC.Word
   , Word#
   , Int#
   , plusWord#
   , minusWord#
   , (+#)
   , (-#)
   , (==#)
   , (>#)
   , (<#)
   , (>=#)
   , (<=#)
   , ltWord#
   , leWord#
   , gtWord#
   , geWord#
   , eqWord#
   , isTrue#
   )
where

import Data.Word
import Data.Int
import Foreign.C.Types
import GHC.Word
import GHC.Exts

import Haskus.Utils.Types

-- | Return a Word with at least 'n' bits
type family WordAtLeast (n :: Nat) where
   WordAtLeast n =
       If (n <=? 8) Word8
      (If (n <=? 16) Word16
      (If (n <=? 32) Word32
      (If (n <=? 64) Word64
      (TypeError ('Text "Cannot find Word with size " ':<>: 'ShowType n))
      )))

-- | Bit size
type family BitSize a :: Nat
type instance BitSize Word8  = 8
type instance BitSize Word16 = 16
type instance BitSize Word32 = 32
type instance BitSize Word64 = 64
