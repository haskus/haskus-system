module Haskus.Arch.X86_64.ISA.RegisterFamilies
   ( Predicate (..)
   , X86RegFam
   , regFamST
   , regFamVec64
   , regFamVec128
   , regFamVec256
   , regFamFixed
   , regFamSegment
   , regFamControl
   , regFamDebug
   , regFamGPR8
   , regFamGPR16
   , regFamGPR32
   , regFamGPR64
   , regFamGPR32o64
   , regFamGPR
   , regFamCounter
   , regFamAccu
   , regFamStackPtr
   , regFamStackBase
   , regFamAX'
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
   = OperandSizeEqual OperandSize
   | AddressSizeEqual AddressSize
   | Mode64bit
   | Not Predicate
   deriving (Show,Eq,Ord)


type X86RegFam = RegFam Predicate RegBank

-- | FPU stack register
regFamST :: X86RegFam
regFamST = (regFamFromReg (R_ST 0))
   { regFamId = Any
   }

-- | 64-bit vector register (mmx)
regFamVec64 :: X86RegFam
regFamVec64 = (regFamFromReg (R_MMX 0))
   { regFamId = Any
   }

-- | 128-bit vector register (xmm)
regFamVec128 :: X86RegFam
regFamVec128 = (regFamFromReg (R_XMM 0))
   { regFamId = Any
   }

-- | 256-bit vector register (ymm)
regFamVec256 :: X86RegFam
regFamVec256 = (regFamFromReg (R_YMM 0))
   { regFamId = Any
   }

-- | Fixed register
regFamFixed :: X86Reg -> X86RegFam
regFamFixed = regFamFromReg

-- | Segment register
regFamSegment :: X86RegFam
regFamSegment = (regFamFromReg R_CS)
   { regFamId = Any
   }

-- | Control register
regFamControl :: X86RegFam
regFamControl = (regFamFromReg (R_CR32 0))
   { regFamId   = Any
   , regFamSize = OneOf [32,64]
   }

-- | Debug register
regFamDebug :: X86RegFam
regFamDebug = (regFamFromReg (R_DR32 0))
   { regFamId   = Any
   , regFamSize = OneOf [32,64]
   }

-- | General purpose 8-bit register
regFamGPR8 :: X86RegFam
regFamGPR8 = (regFamFromReg R_AL)
   { regFamId     = Any
   , regFamOffset = OneOf [0,8]
   }

-- | General purpose 16-bit register
regFamGPR16 :: X86RegFam
regFamGPR16 = (regFamFromReg R_AX)
   { regFamId     = Any
   }

-- | General purpose 32-bit register
regFamGPR32 :: X86RegFam
regFamGPR32 = (regFamFromReg R_EAX)
   { regFamId     = Any
   }

-- | General purpose 64-bit register
regFamGPR64 :: X86RegFam
regFamGPR64 = (regFamFromReg R_RAX)
   { regFamId     = Any
   }

-- | General purpose 32-bit register in legacy mode,
-- general purpose 64-bit register in 64-bit mode.
regFamGPR32o64 :: X86RegFam
regFamGPR32o64 = (regFamFromReg R_RAX)
   { regFamId     = Any
   , regFamSize   = Or
      [ Guard Mode64bit (Set 64)
      , Guard (Not Mode64bit) (Set 32)
      ]
   }

-- | General purpose register (size = operand-size)
regFamGPR :: X86RegFam
regFamGPR = (regFamFromReg R_RAX)
   { regFamId     = Any
   , regFamSize   = Or
      [ Guard (OperandSizeEqual OpSize8)  (Set 8 )
      , Guard (OperandSizeEqual OpSize16) (Set 16)
      , Guard (OperandSizeEqual OpSize32) (Set 32)
      , Guard (OperandSizeEqual OpSize64) (Set 64)
      ]
   }

-- | CX,ECX,RCX depending on the address-size
regFamCounter :: X86RegFam
regFamCounter = (regFamFromReg R_CX)
   { regFamSize   = Or
      [ Guard (AddressSizeEqual AddrSize16) (Set 16)
      , Guard (AddressSizeEqual AddrSize32) (Set 32)
      , Guard (AddressSizeEqual AddrSize64) (Set 64)
      ]
   }

-- | AL,AX,EAX,RAX depending on the operand-size
regFamAccu :: X86RegFam
regFamAccu = (regFamFromReg R_AX)
   { regFamSize   = Or
      [ Guard (OperandSizeEqual OpSize8)  (Set 8 )
      , Guard (OperandSizeEqual OpSize16) (Set 16)
      , Guard (OperandSizeEqual OpSize32) (Set 32)
      , Guard (OperandSizeEqual OpSize64) (Set 64)
      ]
   }

-- | SP, ESP or RSP
--
-- Use RSP in 64-bit mode, otherwise use address-size
regFamStackPtr :: X86RegFam
regFamStackPtr = (regFamFromReg R_SP)
   { regFamSize     = Or
      [ Guard Mode64bit (Set 64)
      , Guard (AddressSizeEqual AddrSize16) (Set 16)
      , Guard (AddressSizeEqual AddrSize32) (Set 32)
      , Guard (AddressSizeEqual AddrSize64) (Set 64)
      ]
   }

-- | BP, EBP or RBP
--
-- Use RBP in 64-bit mode, otherwise use address-size
regFamStackBase :: X86RegFam
regFamStackBase = (regFamFromReg R_BP)
   { regFamSize     = Or
      [ Guard Mode64bit (Set 64)
      , Guard (AddressSizeEqual AddrSize16) (Set 16)
      , Guard (AddressSizeEqual AddrSize32) (Set 32)
      , Guard (AddressSizeEqual AddrSize64) (Set 64)
      ]
   }


-- | Helper for families
famSizes :: Qualifier Predicate Word
famSizes = Or
   [ Guard (OperandSizeEqual OpSize16) (Set 16)
   , Guard (OperandSizeEqual OpSize32) (Set 32)
   , Guard (OperandSizeEqual OpSize64) (Set 64)
   ]

-- | AX,AX,EAX,RAX depending on the operand-size
--
-- This on is used to encode AX, DX:AX, EDX:EAX, RDX:RAX
regFamAX' :: X86RegFam
regFamAX' = (regFamFromReg R_AX)
   { regFamSize   = Or
      [ Guard (OperandSizeEqual OpSize8)  (Set 16)
      , Guard (OperandSizeEqual OpSize16) (Set 16)
      , Guard (OperandSizeEqual OpSize32) (Set 32)
      , Guard (OperandSizeEqual OpSize64) (Set 64)
      ]
   }

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
