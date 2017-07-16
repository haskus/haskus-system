{-# LANGUAGE LambdaCase #-}

-- | Sizes
module Haskus.Arch.X86_64.ISA.Size
   ( Size(..)
   , sizeInBits
   , AddressSize(..)
   , SizedValue(..)
   , OperandSize(..)
   , opSizeInBits
   , getSize
   ) where

import Haskus.Format.Binary.Get
import Haskus.Format.Binary.Word

-- | Size
data Size
   = Size8
   | Size16
   | Size32
   | Size64
   | Size128
   | Size256
   | Size512
   deriving (Show,Eq,Ord)

-- | Get a size in bits
sizeInBits :: Size -> Word
sizeInBits = \case
   Size8   -> 8
   Size16  -> 16
   Size32  -> 32
   Size64  -> 64
   Size128 -> 128
   Size256 -> 256
   Size512 -> 512

-- | Address size
data AddressSize
   = AddrSize16 
   | AddrSize32 
   | AddrSize64 
   deriving (Show,Eq,Ord)

-- | Sized value
data SizedValue
   = SizedValue8  !Word8
   | SizedValue16 !Word16
   | SizedValue32 !Word32
   | SizedValue64 !Word64
   deriving (Show,Eq,Ord)

-- | Operand size
data OperandSize
   = OpSize8 
   | OpSize16 
   | OpSize32 
   | OpSize64 
   deriving (Show,Eq,Ord)

-- | Operand size in bits
opSizeInBits :: OperandSize -> Word
opSizeInBits = \case
   OpSize8  -> 8
   OpSize16 -> 16
   OpSize32 -> 32
   OpSize64 -> 64

-- | Read a SizedValue
getSize :: Size -> Get SizedValue
getSize Size8  = SizedValue8  <$> getWord8
getSize Size16 = SizedValue16 <$> getWord16le
getSize Size32 = SizedValue32 <$> getWord32le
getSize Size64 = SizedValue64 <$> getWord64le
getSize s      = error ("getSize: unsupported size: " ++ show s)
