module ViperVM.Arch.X86_64.Assembler.OperandSize
   ( OperandSize(..)
   , operandSize
   )
where

import ViperVM.Arch.X86_64.Assembler.Size

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
