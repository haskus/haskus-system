-- | Sizes
module ViperVM.Arch.X86_64.Assembler.Size
   ( Size(..)
   , AddressSize(..)
   , SizedValue(..)
   , addressSize
   , OperandSize(..)
   , operandSize
   ) where

import Data.Word

-- | Size
data Size
   = Size8
   | Size16
   | Size32
   | Size64
   | Size128
   | Size256
   | Size512
   deriving (Show,Eq)

-- | Address size
data AddressSize
   = AddrSize16 
   | AddrSize32 
   | AddrSize64 
   deriving (Show,Eq)

-- | Address size to size
addressSize :: AddressSize -> Size
addressSize op = case op of
   AddrSize16 -> Size16
   AddrSize32 -> Size32
   AddrSize64 -> Size64

-- | Sized value
data SizedValue
   = SizedValue8  !Word8
   | SizedValue16 !Word16
   | SizedValue32 !Word32
   | SizedValue64 !Word64
   deriving (Show,Eq)

-- | Operand size
data OperandSize
   = OpSize8 
   | OpSize16 
   | OpSize32 
   | OpSize64 
   deriving (Show,Eq)

-- | Convert an OperandSize into a more generic Size
operandSize :: OperandSize -> Size
operandSize op = case op of
   OpSize8  -> Size8
   OpSize16 -> Size16
   OpSize32 -> Size32
   OpSize64 -> Size64
