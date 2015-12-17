module ViperVM.Arch.X86_64.Assembler.Insn
   ( Instruction(..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.X87
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.OperandSize
import ViperVM.Arch.X86_64.Assembler.Operand

data Instruction
   = InsnX87 X87Instruction
   | InsnX86 X86Insn Encoding (Maybe OperandSize) [Variant] [Op]
   deriving (Show)

