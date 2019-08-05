{-# LANGUAGE LambdaCase #-}

-- | Sizes
module Haskus.Arch.X86_64.ISA.Size
   ( Size(..)
   , sizeInBits
   , AddressSize(..)
   , SizedValue(..)
   , toSizedValue
   , fromSizedValue
   , OperandSize(..)
   , opSizeInBits
   , getSize
   , getSize64
   , getOpSize64
   ) where

import Haskus.Binary.Get
import Haskus.Number.Word

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

-- | Convert a value into a SizedValue
toSizedValue :: Size -> Word64 -> SizedValue
toSizedValue s v = case s of
   Size8  -> SizedValue8  (fromIntegral v)
   Size16 -> SizedValue16 (fromIntegral v)
   Size32 -> SizedValue32 (fromIntegral v)
   Size64 -> SizedValue64 (fromIntegral v)
   _      -> error ("toSizedValue: invalid size (" ++ show s ++ ")")

-- | Convert a value from a SizedValue
fromSizedValue :: SizedValue -> Word64
fromSizedValue = \case
   SizedValue8  v -> fromIntegral v
   SizedValue16 v -> fromIntegral v
   SizedValue32 v -> fromIntegral v
   SizedValue64 v -> v

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

-- | Read a value in a Word64
getSize64 :: Size -> Get Word64
getSize64 Size8  = fromIntegral <$> getWord8
getSize64 Size16 = fromIntegral <$> getWord16le
getSize64 Size32 = fromIntegral <$> getWord32le
getSize64 Size64 =                  getWord64le
getSize64 s      = error ("getSize: unsupported size: " ++ show s)

-- | Read a value in a Word64
getOpSize64 :: OperandSize -> Get Word64
getOpSize64 OpSize8  = fromIntegral <$> getWord8
getOpSize64 OpSize16 = fromIntegral <$> getWord16le
getOpSize64 OpSize32 = fromIntegral <$> getWord32le
getOpSize64 OpSize64 =                  getWord64le
