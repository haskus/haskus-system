module Haskus.Arch.X86_64.ISA.RegisterFamilies
   ( Predicate (..)
   , X86RegFam
   , regST
   , regVec64
   , regVec128
   , regVec256
   , regFixed
   , regSegment
   , regControl
   , regDebug
   , regGPR8
   , regGPR16
   , regGPR32
   , regGPR64
   , regGPR32o64
   , regGPR
   , regCounter
   , regAccu
   , regStackPtr
   , regStackBase
   , regFamAX
   , regFamBX
   , regFamCX
   , regFamDX
   , regFamSI
   , regFamDI
   )
where

import Haskus.Arch.Common.Register
import Haskus.Arch.X86_64.ISA.RegisterNames
import Haskus.Arch.X86_64.ISA.Size

-- | Encoding predicates
data Predicate
   = OperandSizeEqual Size
   | AddressSizeEqual Size
   | Mode64bit
   | Not Predicate
   deriving (Show,Eq,Ord)


type X86RegFam = RegFam Predicate RegBank

-- | FPU stack register
regST :: X86RegFam
regST = (regFamFromReg (R_ST 0))
   { regFamId = Any
   }

-- | 64-bit vector register (mmx)
regVec64 :: X86RegFam
regVec64 = (regFamFromReg (R_MMX 0))
   { regFamId = Any
   }

-- | 128-bit vector register (xmm)
regVec128 :: X86RegFam
regVec128 = (regFamFromReg (R_XMM 0))
   { regFamId = Any
   }

-- | 256-bit vector register (ymm)
regVec256 :: X86RegFam
regVec256 = (regFamFromReg (R_YMM 0))
   { regFamId = Any
   }

-- | Fixed register
regFixed :: X86Reg -> X86RegFam
regFixed = regFamFromReg

-- | Segment register
regSegment :: X86RegFam
regSegment = (regFamFromReg R_CS)
   { regFamId = Any
   }

-- | Control register
regControl :: X86RegFam
regControl = (regFamFromReg (R_CR32 0))
   { regFamId   = Any
   , regFamSize = OneOf [32,64]
   }

-- | Debug register
regDebug :: X86RegFam
regDebug = (regFamFromReg (R_DR32 0))
   { regFamId   = Any
   , regFamSize = OneOf [32,64]
   }

-- | General purpose 8-bit register
regGPR8 :: X86RegFam
regGPR8 = (regFamFromReg R_AL)
   { regFamId     = Any
   , regFamOffset = OneOf [0,8]
   }

-- | General purpose 16-bit register
regGPR16 :: X86RegFam
regGPR16 = (regFamFromReg R_AX)
   { regFamId     = Any
   }

-- | General purpose 32-bit register
regGPR32 :: X86RegFam
regGPR32 = (regFamFromReg R_EAX)
   { regFamId     = Any
   }

-- | General purpose 64-bit register
regGPR64 :: X86RegFam
regGPR64 = (regFamFromReg R_RAX)
   { regFamId     = Any
   }

-- | General purpose 32-bit register in legacy mode,
-- general purpose 64-bit register in 64-bit mode.
regGPR32o64 :: X86RegFam
regGPR32o64 = (regFamFromReg R_RAX)
   { regFamId     = Any
   , regFamSize   = Or
      [ Guard Mode64bit (Set 64)
      , Guard (Not Mode64bit) (Set 32)
      ]
   }

-- | General purpose register (size = operand-size)
regGPR :: X86RegFam
regGPR = (regFamFromReg R_RAX)
   { regFamId     = Any
   , regFamSize   = Or
      [ Guard (OperandSizeEqual Size8)  (Set 8 )
      , Guard (OperandSizeEqual Size16) (Set 16)
      , Guard (OperandSizeEqual Size32) (Set 32)
      , Guard (OperandSizeEqual Size64) (Set 64)
      ]
   }

-- | CX,ECX,RCX depending on the address-size
regCounter :: X86RegFam
regCounter = (regFamFromReg R_CX)
   { regFamSize   = Or
      [ Guard (AddressSizeEqual Size16) (Set 16)
      , Guard (AddressSizeEqual Size32) (Set 32)
      , Guard (AddressSizeEqual Size64) (Set 64)
      ]
   }

-- | AL,AX,EAX,RAX depending on the operand-size
regAccu :: X86RegFam
regAccu = (regFamFromReg R_AX)
   { regFamSize   = Or
      [ Guard (OperandSizeEqual Size8)  (Set 8 )
      , Guard (OperandSizeEqual Size16) (Set 16)
      , Guard (OperandSizeEqual Size32) (Set 32)
      , Guard (OperandSizeEqual Size64) (Set 64)
      ]
   }

-- | SP, ESP or RSP (default in 64-bit mode)
regStackPtr :: X86RegFam
regStackPtr = (regFamFromReg R_SP)
   { regFamSize     = OneOf [16,32,64]
   }

-- | BP, EBP or RBP (default in 64-bit mode)
regStackBase :: X86RegFam
regStackBase = (regFamFromReg R_SP)
   { regFamSize     = OneOf [16,32,64]
   }


-- | Helper for families
famSizes :: Qualifier Predicate Word
famSizes = Or
   [ Guard (OperandSizeEqual Size16) (Set 16)
   , Guard (OperandSizeEqual Size32) (Set 32)
   , Guard (OperandSizeEqual Size64) (Set 64)
   ]

-- | AX,EAX,RAX depending on the operand-size
regFamAX :: X86RegFam
regFamAX = (regFamFromReg R_AX)
   { regFamSize   = famSizes
   }

-- | BX,EBX,RBX depending on the operand-size
regFamBX :: X86RegFam
regFamBX = (regFamFromReg R_BX)
   { regFamSize   = famSizes
   }

-- | CX,ECX,RCX depending on the operand-size
regFamCX :: X86RegFam
regFamCX = (regFamFromReg R_CX)
   { regFamSize   = famSizes
   }

-- | DX,EDX,RDX depending on the operand-size
regFamDX :: X86RegFam
regFamDX = (regFamFromReg R_DX)
   { regFamSize   = famSizes
   }

-- | SI,ESI,RSI depending on the operand-size
regFamSI :: X86RegFam
regFamSI = (regFamFromReg R_SI)
   { regFamSize   = famSizes
   }

-- | DI,EDI,RDI depending on the operand-size
regFamDI :: X86RegFam
regFamDI = (regFamFromReg R_DI)
   { regFamSize   = famSizes
   }
