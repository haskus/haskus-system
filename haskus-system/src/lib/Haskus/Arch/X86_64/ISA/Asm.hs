module Haskus.Arch.X86_64.ISA.Asm
  ( DefaultOperandSize (..)
  , os16
  , os32
  )
where

import Haskus.Arch.X86_64.ISA.Output

data DefaultOperandSize
  = DefaultOperandSize16
  | DefaultOperandSize32
  deriving (Show)

-- | Enable 16-bit operand size if it isn't the default
os16 :: Output m => DefaultOperandSize -> m ()
os16 = \case
  DefaultOperandSize16 -> pure ()
  DefaultOperandSize32 -> putW8 0x66

-- | Enable 32-bit operand size if it isn't the default
os32 :: Output m => DefaultOperandSize -> m ()
os32 = \case
  DefaultOperandSize16 -> putW8 0x66
  DefaultOperandSize32 -> pure ()
