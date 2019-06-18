-- | Immediate operand
module Haskus.Arch.X86_64.ISA.Immediate
   ( X86ImmFamP
   , X86ImmFamT
   , X86ImmFam
   , X86Imm
   , Imm (..)
   , immFamFixedSize
   , immFamOpSize
   , immFamOpSizeSE
   , immFamConst
   )
where

import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.Common.Immediate
import Haskus.Utils.Solver
import Haskus.Format.Binary.Word

data ImmType
   = ImmGeneric
   deriving (Show,Eq,Ord)

type X86ImmFamP  = ImmFamP X86Pred X86Err ImmType OperandSize
type X86ImmFamT  = ImmFamT ImmType OperandSize
type X86ImmFam t = ImmFam t ImmType OperandSize
type X86Imm      = Imm ImmType OperandSize

-- | Fixed size immediate
immFamFixedSize :: OperandSize -> X86ImmFamP
immFamFixedSize s = ImmFam
   { immFamSize         = Terminal s
   , immFamSignExtended = Terminal Nothing
   , immFamValue        = Terminal Nothing
   , immFamType         = Terminal Nothing
   }

-- | Operand-sized immediate
immFamOpSize :: X86ImmFamP
immFamOpSize = ImmFam
   { immFamSize = pOpSize64 OpSize8 OpSize16 OpSize32 OpSize64
   , immFamSignExtended = Terminal Nothing
   , immFamValue        = Terminal Nothing
   , immFamType         = Terminal (Just ImmGeneric)
   }

-- | Operand-sized immediate (size-extendable if the bit is set or in 64-bit
-- mode)
immFamOpSizeSE :: X86ImmFamP
immFamOpSizeSE = ImmFam
   { immFamSize = OrderedNonTerminal
      [ (pForce8bit                         , Terminal OpSize8)
      , (pSignExtendBit                     , Terminal OpSize8)
      , (pOverriddenOperationSize64 OpSize16, Terminal OpSize16)
      , (pOverriddenOperationSize64 OpSize32, Terminal OpSize32)
      , (pOverriddenOperationSize64 OpSize64, Terminal OpSize32) -- sign-extend
      ]
   , immFamSignExtended = OrderedNonTerminal
      [ (pOverriddenOperationSize64 OpSize64, Terminal $ Just OpSize64)
      , (pSignExtendBit                     , NonTerminal
         [ (pOverriddenOperationSize64 OpSize16, Terminal $ Just OpSize16)
         , (pOverriddenOperationSize64 OpSize32, Terminal $ Just OpSize32)
         ])
      , (CBool True                         , Terminal Nothing)
      ]
   , immFamValue        = Terminal Nothing
   , immFamType         = Terminal (Just ImmGeneric)
   }


-- | Constant immediate
immFamConst :: OperandSize -> Word64 -> X86ImmFamP
immFamConst s v = ImmFam
   { immFamSize         = Terminal s
   , immFamSignExtended = Terminal Nothing
   , immFamValue        = Terminal (Just v)
   , immFamType         = Terminal Nothing
   }

