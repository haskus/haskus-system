-- | Module containing helper functions that use different things and cannot be
-- easily put in another module because of circualr dependencies
module ViperVM.Arch.X86_64.Assembler.Utils
   ( computeOperandSize
   )
where

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.OperandSize


-- Note [Operand Size]
-- ~~~~~~~~~~~~~~~~~~~
--
-- Operand size depends on several factors:
--
-- |---------------------------------------------------------------------|
-- | Mode             | Default | Insn def | 66h prefix | REX.W | Opsize |
-- |------------------|---------|----------|------------|-------|--------|
-- |                  |         |    64    |      N     |   N   |   64   |
-- |                  |         |    64    |      Y     |   N   |   16   |
-- | Long Mode 64-Bit |   32    |   -/64   |     Y/N    |   Y   |   64   |
-- |                  |         |    -     |      N     |   N   |   32   |
-- |---------------------------------------------------------------------|
-- | Long mode Compat |   32    |    -     |      N     |  N/A  |   32   |
-- |         &        |   32    |    -     |      Y     |  N/A  |   16   |
-- |    Legacy mode   |   16    |    -     |      N     |  N/A  |   16   |
-- |                  |   16    |    -     |      Y     |  N/A  |   32   |
-- |---------------------------------------------------------------------|
--
-- Note that when the default instruction operand size is 64, there is no way
-- to encode a 32-bit operand size


-- | Compute effective operand size
computeOperandSize :: X86Mode -> [LegacyPrefix] -> OperandSize -> Bool -> Encoding -> OperandSize
computeOperandSize mode prefixes osize rexw enc = 
      case (mode, osize, prefix66, rexw, hasDefault64) of
         (LongMode Long64bitMode, _, _, True, _)         -> OpSize64
         (LongMode Long64bitMode, _, False, False,True)  -> OpSize64
         (LongMode Long64bitMode, _, False, False,False) -> OpSize32
         (LongMode Long64bitMode, _, True, False,_)      -> OpSize16
         (_, OpSize16, False, _, _)                      -> OpSize16
         (_, OpSize32, False, _, _)                      -> OpSize32
         (_, OpSize32, True, _, _)                       -> OpSize16
         (_, OpSize16, True, _, _)                       -> OpSize32
         _ -> error "Invalid combination of modes and operand sizes"
   where
      hasDefault64 = DefaultOperandSize64 `elem` encProperties enc
      prefix66     = PrefixOperandSizeOverride `elem` prefixes

