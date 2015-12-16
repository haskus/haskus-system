module ViperVM.Arch.X86_64.Assembler.Size
   ( Size(..)
   , AddressSize(..)
   , SizedValue(..)
   , addressSize
   ) where

import Data.Word

data Size
   = Size8
   | Size16
   | Size32
   | Size64
   | Size128
   | Size256
   | Size512
   deriving (Show,Eq)

data AddressSize
   = AddrSize16 
   | AddrSize32 
   | AddrSize64 
   deriving (Show,Eq)

addressSize :: AddressSize -> Size
addressSize op = case op of
   AddrSize16 -> Size16
   AddrSize32 -> Size32
   AddrSize64 -> Size64

data SizedValue
   = SizedValue8  !Word8
   | SizedValue16 !Word16
   | SizedValue32 !Word32
   | SizedValue64 !Word64
   deriving (Show,Eq)
