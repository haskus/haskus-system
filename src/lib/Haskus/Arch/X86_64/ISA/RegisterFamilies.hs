{-# LANGUAGE RecordWildCards #-}


-- | X86 register families
module Haskus.Arch.X86_64.ISA.RegisterFamilies
   ( X86RegFam
   -- * Families
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
   , regFamGPRh
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
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Utils.Solver

type X86RegFam = RegFam X86Pred RegBank

-- | FPU stack register
regFamST :: X86RegFam
regFamST = (regFamFromReg (R_ST 0))
   { regFamId = Terminal Any
   }

-- | 64-bit vector register (mmx)
regFamVec64 :: X86RegFam
regFamVec64 = (regFamFromReg (R_MMX 0))
   { regFamId = Terminal Any
   }

-- | 128-bit vector register (xmm)
regFamVec128 :: X86RegFam
regFamVec128 = (regFamFromReg (R_XMM 0))
   { regFamId = Terminal Any
   }

-- | 256-bit vector register (ymm)
regFamVec256 :: X86RegFam
regFamVec256 = (regFamFromReg (R_YMM 0))
   { regFamId = Terminal Any
   }

-- | Fixed register
regFamFixed :: X86Reg -> X86RegFam
regFamFixed = regFamFromReg

-- | Segment register
regFamSegment :: X86RegFam
regFamSegment = (regFamFromReg R_CS)
   { regFamId = Terminal Any
   }

-- | Control register
regFamControl :: X86RegFam
regFamControl = (regFamFromReg (R_CR32 0))
   { regFamId   = Terminal Any
   , regFamSize = Terminal $ OneOf [32,64]
   }

-- | Debug register
regFamDebug :: X86RegFam
regFamDebug = (regFamFromReg (R_DR32 0))
   { regFamId   = Terminal Any
   , regFamSize = Terminal $ OneOf [32,64]
   }

-- | General purpose 8-bit register
regFamGPR8 :: X86RegFam
regFamGPR8 = (regFamFromReg R_AL)
   { regFamId     = Terminal Any
   , regFamOffset = pCheck $ NonTerminal
      [ (pLegacy8bitRegs    , Terminal (OneOf [0,8]))
      , (Not pLegacy8bitRegs, Terminal (Singleton 0))
      ]
   }

-- | General purpose 16-bit register
regFamGPR16 :: X86RegFam
regFamGPR16 = (regFamFromReg R_AX)
   { regFamId     = Terminal Any
   }

-- | General purpose 32-bit register
regFamGPR32 :: X86RegFam
regFamGPR32 = (regFamFromReg R_EAX)
   { regFamId     = Terminal Any
   }

-- | General purpose 64-bit register
regFamGPR64 :: X86RegFam
regFamGPR64 = (regFamFromReg R_RAX)
   { regFamId     = Terminal Any
   }

-- | General purpose 32-bit register in legacy mode,
-- general purpose 64-bit register in 64-bit mode.
regFamGPR32o64 :: X86RegFam
regFamGPR32o64 = (regFamFromReg R_RAX)
   { regFamId     = Terminal Any
   , regFamSize   = pCheck $ NonTerminal
      [ (pMode64bit    , Terminal (Singleton 64))
      , (Not pMode64bit, Terminal (Singleton 32))
      ]
   }

-- | General purpose register (size = operand-size)
regFamGPR :: X86RegFam
regFamGPR = (regFamFromReg R_RAX)
   { regFamId     = pCheck $ orderedNonTerminal
      [ (Not (pOverriddenOperationSize64 OpSize8)  , Terminal Any)
      , (Not pLegacy8bitRegs                       , Terminal Any)
                                                   -- disable SIL,DIL,etc.
      , (pLegacy8bitRegs                           , Terminal (NoneOf [4,5,6,7]))
      ]
   , regFamSize   = pCheck $ orderedNonTerminal
      [ (pOverriddenOperationSize64 OpSize8 , Terminal (Singleton 8 ))
      , (pOverriddenOperationSize64 OpSize16, Terminal (Singleton 16))
      , (pOverriddenOperationSize64 OpSize32, Terminal (Singleton 32))
      , (pOverriddenOperationSize64 OpSize64, Terminal (Singleton 64))
      ]
   , regFamOffset = pCheck $ orderedNonTerminal
      [ (Not (pOverriddenOperationSize64 OpSize8), Terminal (Singleton 0))
                                                   -- disable AH,BH,CH,DH
                                                   -- (offset = 8)
      , (Not pLegacy8bitRegs                     , Terminal (Singleton 0))
      , (pLegacy8bitRegs                         , Terminal (OneOf [0,8]))
      ]
   }

-- | AH,BH,CH,DH
regFamGPRh :: X86RegFam
regFamGPRh = (regFamFromReg R_AH)
   { regFamId     = Terminal $ OneOf [0,1,2,3]
   , regFamSize   = Terminal $ Singleton 8
   , regFamOffset = Terminal $ Singleton 8
   }

-- | CX,ECX,RCX depending on the address-size
regFamCounter :: X86RegFam
regFamCounter = (regFamFromReg R_CX)
   { regFamSize   = pCheck $ orderedNonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal (Singleton 16))
      , (pOverriddenAddressSize AddrSize32, Terminal (Singleton 32))
      , (pOverriddenAddressSize AddrSize64, Terminal (Singleton 64))
      ]
   }

-- | AL,AX,EAX,RAX depending on the operand-size
regFamAccu :: X86RegFam
regFamAccu = (regFamFromReg R_AX)
   { regFamSize   = pCheck $ orderedNonTerminal
      [ (pOverriddenOperationSize64 OpSize8 , Terminal (Singleton 8 ))
      , (pOverriddenOperationSize64 OpSize16, Terminal (Singleton 16))
      , (pOverriddenOperationSize64 OpSize32, Terminal (Singleton 32))
      , (pOverriddenOperationSize64 OpSize64, Terminal (Singleton 64))
      ]
   }

-- | SP, ESP or RSP
--
-- Use RSP in 64-bit mode, otherwise use address-size
regFamStackPtr :: X86RegFam
regFamStackPtr = (regFamFromReg R_SP)
   { regFamSize     = pCheck $ orderedNonTerminal
      [ (pMode64bit                       , Terminal (Singleton 64))
      , (pOverriddenAddressSize AddrSize16, Terminal (Singleton 16))
      , (pOverriddenAddressSize AddrSize32, Terminal (Singleton 32))
      , (pOverriddenAddressSize AddrSize64, Terminal (Singleton 64))
      ]
   }

-- | BP, EBP or RBP
--
-- Use RBP in 64-bit mode, otherwise use address-size
regFamStackBase :: X86RegFam
regFamStackBase = (regFamFromReg R_BP)
   { regFamSize     = pCheck $ orderedNonTerminal
      [ (pMode64bit                       , Terminal (Singleton 64))
      , (pOverriddenAddressSize AddrSize16, Terminal (Singleton 16))
      , (pOverriddenAddressSize AddrSize32, Terminal (Singleton 32))
      , (pOverriddenAddressSize AddrSize64, Terminal (Singleton 64))
      ]
   }


-- | Helper for families
famSizes :: Qualifier X86Pred Word
famSizes = pCheck $ orderedNonTerminal
   [ (pOverriddenOperationSize64 OpSize16, Terminal (Singleton 16))
   , (pOverriddenOperationSize64 OpSize32, Terminal (Singleton 32))
   , (pOverriddenOperationSize64 OpSize64, Terminal (Singleton 64))
   ]

-- | AX,AX,EAX,RAX depending on the operand-size
--
-- This on is used to encode AX, DX:AX, EDX:EAX, RDX:RAX
regFamAX' :: X86RegFam
regFamAX' = (regFamFromReg R_AX)
   { regFamSize   = pCheck $ orderedNonTerminal
      [ (pOverriddenOperationSize64 OpSize8 , Terminal (Singleton 16))
      , (pOverriddenOperationSize64 OpSize16, Terminal (Singleton 16))
      , (pOverriddenOperationSize64 OpSize32, Terminal (Singleton 32))
      , (pOverriddenOperationSize64 OpSize64, Terminal (Singleton 64))
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
