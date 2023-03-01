module Haskus.Arch.X86_64.ISA.Asm
  ( Asm (..)
  , DefaultOperandSize (..)
  , os16
  , os32
  )
where

import Haskus.Arch.X86_64.ISA.Output

-- | Assembler code generator type-class
--
-- For most instructions, it supports value arguments or markers. Markers
-- produce relocations that can be fixed up in a following pass.
class Output m => Asm m where
  -- | Get current default operand size
  getOperandSize :: m DefaultOperandSize

data DefaultOperandSize
  = DefaultOperandSize16
  | DefaultOperandSize32
  deriving (Show)

-- | Enable 16-bit operand size if it isn't the default
os16 :: Asm m => m ()
os16 = getOperandSize >>= \case
  DefaultOperandSize16 -> pure ()
  DefaultOperandSize32 -> putW8 0x66

-- | Enable 32-bit operand size if it isn't the default
os32 :: Asm m => m ()
os32 = getOperandSize >>= \case
  DefaultOperandSize16 -> putW8 0x66
  DefaultOperandSize32 -> pure ()
