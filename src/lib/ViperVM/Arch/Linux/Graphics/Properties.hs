module ViperVM.Arch.Linux.Graphics.Properties
   ( PropertyType(..)
   )
where

import Data.Bits
import Data.Word

data PropertyType
   = PropTypePending
   | PropTypeRange
   | PropTypeImmutable
   | PropTypeEnum       -- ^ Enumerated type with text strings
   | PropTypeBlob
   | PropTypeBitmask    -- ^ Bitmask of enumerated types
   | PropTypeObject
   | PropTypeSignedRange
   deriving (Eq,Ord,Show)

toPropType :: Word32 -> PropertyType
toPropType typ =
   case typ of
      -- legacy types: 1 bit per type...
      1  -> PropTypePending
      2  -> PropTypeRange
      4  -> PropTypeImmutable
      8  -> PropTypeEnum
      16 -> PropTypeBlob
      32 -> PropTypeBitmask
      -- newer types, shifted int
      n -> case (n `shiftR` 6) of
         1 -> PropTypeObject
         2 -> PropTypeSignedRange
         _ -> error "Unknown type"

fromPropType :: PropertyType -> Word32
fromPropType typ =
   case typ of
      -- legacy types: 1 bit per type...
      PropTypePending      -> 1
      PropTypeRange        -> 2
      PropTypeImmutable    -> 4
      PropTypeEnum         -> 8
      PropTypeBlob         -> 16
      PropTypeBitmask      -> 32
      -- newer types, shifted int
      PropTypeObject       -> 1 `shiftL` 6
      PropTypeSignedRange  -> 2 `shiftL` 6
