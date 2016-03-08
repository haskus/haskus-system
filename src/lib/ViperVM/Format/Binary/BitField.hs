{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Bit fields (as in C)
--
-- This module allows you to define bit fields over words. For instance, you can
-- have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
-- respectively.
--
--                   X             Y          Z
--  w :: Word16 |0 0 0 0 0|0 0 0 0 0 0 0 0 0|0 0|
-- 
-- You define it as follows:
-- @
-- w :: BitFields Word16 (Cons (BitField 5 "X" Word8) 
--                       (Cons (BitField 9 "Y" Word16)
--                       (Cons (BitField 2 "Z" Word8) Nil)))
-- w = BitFields 0x01020304
-- @
--
-- Note that each field has its own associated type (e.g. Word8 for X and Z)
-- that must be large enough to hold the number of bits for the field.
-- 
-- You can extract and update the value of a field by its name:
--
-- @
-- x = extractField w (Proxy :: Proxy "X")
-- z = extractField w (Proxy :: Proxy "Z")
-- w' = updateField w (Proxy :: Proxy "Y") 0x5566
-- @
--
-- Fields can also be 'BitSet'.
module ViperVM.Format.Binary.BitField
   ( BitFields (..)
   , BitField (..)
   , Cons (..)
   , Nil (..)
   , extractField
   , updateField
   )
where

import Data.Word
import Data.Int
import Data.Bits
import GHC.TypeLits
import Data.Proxy
import Numeric
import Foreign.Storable
import ViperVM.Format.Binary.BitSet as BitSet

-- | Bit fields on a base type b
data BitFields b fields = BitFields !b

instance (Integral b, Show b) => Show (BitFields b fields) where
   show (BitFields w) = "0x" ++ showHex w ""

-- | A field of n bits
newtype BitField (n :: Nat) (name :: Symbol) s = BitField s deriving (Storable)

type family BitSize a :: Nat
type instance BitSize Word8  = 8
type instance BitSize Word16 = 16
type instance BitSize Word32 = 32
type instance BitSize Word64 = 64

-- | Get the bit offset of a field from its name
type family Offset (name :: Symbol) fs :: Nat where
   Offset name (Cons (BitField n name s) xs)  = AddOffset xs
   Offset name (Cons (BitField n name2 s) xs) = Offset name xs

type family AddOffset fs :: Nat where
   AddOffset Nil = 0
   AddOffset (Cons (BitField n name s) Nil) = n
   AddOffset (Cons (BitField n name s) xs)  = n + AddOffset xs

-- | Get the type of a field from its name
type family Output (name :: Symbol) fs :: * where
   Output name (Cons (BitField n name s) xs)  = s
   Output name (Cons (BitField n name2 s) xs) = Output name xs

-- | Get the size of a field from it name
type family Size (name :: Symbol) fs :: Nat where
   Size name (Cons (BitField n name s) xs)  = n
   Size name (Cons (BitField n name2 s) xs) = Size name xs

-- | Get the whole size of a BitFields
type family WholeSize fs :: Nat where
   WholeSize Nil                            = 0
   WholeSize (Cons (BitField n name s) xs)  = n + WholeSize xs


data Cons x xs = Cons
data Nil = Nil

class Field f where
   fromField :: Integral b => f -> b
   toField   :: Integral b => b -> f

instance Field Bool where
   fromField True  = 1
   fromField False = 0
   toField 0  = False
   toField _  = True

instance Field Word where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word8 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word16 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word32 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word64 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int8 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int16 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int32 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int64 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Integral b => Field (BitSet b a) where
   fromField = fromIntegral . BitSet.toBits
   toField   = BitSet.fromBits . fromIntegral


extractField :: forall name fields b . (KnownNat (Offset name fields), KnownNat (Size name fields), WholeSize fields ~ BitSize b, Bits b, Integral b, Field (Output name fields)) => BitFields b fields -> Proxy name -> Output name fields
extractField (BitFields w) _ = toField ((w `shiftR` fromIntegral off) .&. ((1 `shiftL` fromIntegral sz) - 1))
   where
      off = natVal (Proxy :: Proxy (Offset name fields))
      sz  = natVal (Proxy :: Proxy (Size name fields))

{-# INLINE extractField #-}

updateField :: forall name fields b . (KnownNat (Offset name fields), KnownNat (Size name fields), WholeSize fields ~ BitSize b, Bits b, Integral b, Field (Output name fields)) => BitFields b fields -> Proxy name -> Output name fields -> BitFields b fields
updateField (BitFields w) _ value = BitFields $ ((fromField value `shiftL` off) .&. mask) .|. (w .&. complement mask)
   where
      off  = fromIntegral $ natVal (Proxy :: Proxy (Offset name fields))
      sz   = natVal (Proxy :: Proxy (Size name fields))
      mask = ((1 `shiftL` fromIntegral sz) - 1) `shiftL` off

{-# INLINE updateField #-}
