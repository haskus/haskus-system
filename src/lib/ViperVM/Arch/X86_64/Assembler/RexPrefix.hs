module ViperVM.Arch.X86_64.Assembler.RexPrefix
   ( Rex(..)
   , rexW
   , rexR
   , rexX
   , rexB
   , putRexPrefix
   , decodeREX
   ) where

import Data.Word
import Data.Bits
import Control.Monad.State

import ViperVM.Format.Binary.Put
import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size

{-
 Note [REX prefix]
 ~~~~~~~~~~~~~~~~~

 With the 64bit extension of the X86 ISA, a new prefix has been introduced.
 It is only available in Long mode and can be used to select the additional
 registers and use 64 bit operands. It overrides the one-byte alterinative
 encoding of INC and DEC in X86-32 ABI, hence these encodings must not be
 used in Long mode.

 The REX prefix is an optional single byte after the legacy prefixes (if
 any). If more than one REX prefix is present, the behavior is undefined
 (however it seems that the last one is used).

 The presence of the REX prefix disallows the use of the *H registers
 (AH,BH,etc.). Instead their codes encode new registers.

 There are 4 fields in the REX prefix:
    W: force 64 bit operand size if set
    R: 1-bit extension to the ModRM.reg field (see below)
    X: 1-bit extension to the SIB.index field (see below)
    B: 1-bit extension to the ModRM.rm field (see below)
-}

newtype Rex = Rex Word8 deriving (Show)

-- | Write a REX prefix
putRexPrefix :: Rex -> Put
putRexPrefix (Rex x) = putWord8 (x .|. 0x40)

-- | Test W bit of REX prefix
rexW :: Rex -> Bool
rexW (Rex v) = testBit v 4

-- | Test R bit of REX prefix
rexR :: Rex -> Word8
rexR (Rex v) = if testBit v 3 then 1 else 0

-- | Test X bit of REX prefix
rexX :: Rex -> Word8
rexX (Rex v) = if testBit v 2 then 1 else 0

-- | Test B bit of REX prefix
rexB :: Rex -> Word8
rexB (Rex v) = if testBit v 1 then 1 else 0

-- | Try to decode a REX prefix. See Note [REX prefix]
decodeREX :: X86Dec ()
decodeREX = do
   mode <- getMode

   when (is64bitMode mode) $ lookWord8 >>= \x -> do
      when (x .&. 0xF0 == 0x40) $ do
         skipWord8
         let rex = Rex x
         modify (\y -> y
            { stateBaseRegExt   = rexB rex
            , stateIndexRegExt  = rexX rex
            , stateRegExt       = rexR rex
            , stateUseExtRegs   = True
            , stateAddressSize  = AddrSize64
            , stateOperandSize  = OpSize64
            , stateHasRexPrefix = True
            })

