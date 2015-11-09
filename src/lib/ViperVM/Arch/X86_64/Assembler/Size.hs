module ViperVM.Arch.X86_64.Assembler.Size
   ( Size(..)
   , OperandSize(..)
   , AddressSize(..)
   , SizedValue(..)
   , effectiveOperandSize
   , effectiveAddressSize
   , operandSize
   , addressSize
   ) where

import Data.Maybe
import Data.Word

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix

data Size
   = Size8
   | Size16
   | Size32
   | Size64
   | Size128
   | Size256
   | Size512
   deriving (Show,Eq)

data OperandSize
   = OpSize8 
   | OpSize16 
   | OpSize32 
   | OpSize64 
   deriving (Show,Eq)

operandSize :: OperandSize -> Size
operandSize op = case op of
   OpSize8 -> Size8
   OpSize16 -> Size16
   OpSize32 -> Size32
   OpSize64 -> Size64

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

-- | Return effective operand size
--
-- See Table 1-2 "Operand-Size Overrides" in AMD Manual v3
effectiveOperandSize :: X86Mode -> [LegacyPrefix] -> Maybe Rex -> OperandSize -> OperandSize
effectiveOperandSize mode prefixes rex defaultSize = 
   let 
      isOverrided = PrefixOperandSizeOverride `elem` prefixes
      rexw = fromMaybe False (fmap rexW rex)
   in case (mode, defaultSize, isOverrided, rexw) of
      (LongMode Long64bitMode, _, _, True)         -> OpSize64
      (LongMode Long64bitMode, def, False, False)  -> def
      (LongMode Long64bitMode, _, True, False)     -> OpSize16
      (_, def, False, _) 
         | def `elem` [OpSize16,OpSize32]          -> def
      (_, OpSize32, True, _)                       -> OpSize16
      (_, OpSize16, True, _)                       -> OpSize32
      _ -> error "Invalid combination of modes and operand sizes"

-- | Return effective address size
--
-- See Table 1-1 "Address-Size Overrides" in AMD Manual v3
--
-- The prefix also changes the size of RCX when it is implicitly accessed
--
-- Address size for implicit accesses to the stack segment is determined by D
-- bit in the stack segment descriptor or is 64 bit in 64 bit mode.
--
effectiveAddressSize :: X86Mode -> [LegacyPrefix] -> AddressSize -> AddressSize
effectiveAddressSize mode prefixes defaultSize =
   let 
      isOverrided = PrefixAddressSizeOverride `elem` prefixes
   in case (mode, defaultSize, isOverrided) of
      (LongMode Long64bitMode, _, False)     -> AddrSize64
      (LongMode Long64bitMode, _, True)      -> AddrSize32
      (_, def, False)
         | def `elem` [AddrSize16,AddrSize32]-> def
      (_, AddrSize32, True)                  -> AddrSize16
      (_, AddrSize16, True)                  -> AddrSize32
      _ -> error "Invalid combination of modes and address sizes"
   

