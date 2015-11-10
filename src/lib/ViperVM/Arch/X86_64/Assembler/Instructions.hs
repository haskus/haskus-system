{-# LANGUAGE LambdaCase #-}
module ViperVM.Arch.X86_64.Assembler.Instructions
   ( X86Instruction(..)
   , ExtImm(..)
   , decodeX86
   )
where

import Data.Word
import Data.Bits
import Control.Monad.Trans.Either

import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.Addressing
import ViperVM.Arch.X86_64.Assembler.ModRM

type Imm = SizedValue

-- | Extendable immediate
data ExtImm
   = SignExtend Imm
   | Neutral Imm
   deriving (Show,Eq)

data X86Instruction
   = ADD_rAX_imm     OperandSize   ExtImm
   | ADD             OperandSize   Op          Op
   | ADD_reg_imm     OperandSize   Register    ExtImm
   | ADD_mem_imm     OperandSize   Addr        ExtImm
   | ADD_reg_reg     OperandSize   Register    Register
   | ADD_mem_reg     OperandSize   Addr        Register
   | ADD_reg_mem     OperandSize   Register    Addr
   deriving (Show,Eq)


decodeX86 :: Word8 -> X86Dec X86Instruction
decodeX86 x = do
   osize <- getEffectiveOperandSize

   let
      getImm8  = SizedValue8  <$> nextWord8
      getImm16 = SizedValue16 <$> nextWord16
      getImm32 = SizedValue32 <$> nextWord32
      getNeutralImm8  = Neutral <$> getImm8
      getNeutralImm16 = Neutral <$> getImm16
      getNeutralImm32 = Neutral <$> getImm32
      getSignExtendedImm8  = SignExtend <$> getImm8
      getSignExtendedImm32 = SignExtend <$> getImm32

      -- get immediate adapted to the operand size
      getImm = case osize of
         OpSize8  -> getNeutralImm8
         OpSize16 -> getNeutralImm16
         OpSize32 -> getNeutralImm32
         OpSize64 -> getSignExtendedImm32

      -- get signed-extended immediate
      getExtImm = case osize of
         OpSize8  -> getNeutralImm8
         OpSize16 -> getSignExtendedImm8
         OpSize32 -> getSignExtendedImm8
         OpSize64 -> getSignExtendedImm8

      ext_rm sz = do
         m  <- ModRM <$> nextWord8
         rm <- getRMOp RF_GPR (Just (operandSize sz)) m
         return (regField m, rm, sz)

      reg_rm sz = do
         m   <- ModRM <$> nextWord8
         reg <- getRegRegister RF_GPR (Just (operandSize sz)) m
         rm  <- getRMOp        RF_GPR (Just (operandSize sz)) m
         return (reg, rm, sz)

      rm_reg sz = do
         m   <- ModRM <$> nextWord8
         reg <- getRegRegister RF_GPR (Just (operandSize sz)) m
         rm  <- getRMOp        RF_GPR (Just (operandSize sz)) m
         return (rm, reg, sz)

      rm_reg2 sz = do
         m   <- ModRM <$> nextWord8
         reg <- getRegRegister RF_GPR (Just (operandSize sz)) m
         rm  <- getRMOp        RF_GPR (Just (operandSize sz)) m
         return (rm, OpReg reg)

      decodeAddWithoutImm :: Word8 -> X86Dec X86Instruction
      decodeAddWithoutImm x = do
         -- Sizable
         let sz  = if testBit x 0 then osize else OpSize8
         -- Reversable
         let flp = if testBit x 1 then flip  else id

         rm_reg2 sz >>= right . uncurry (flp (ADD sz))

   case x of
      00 -> rm_reg OpSize8 >>= \case
         (OpReg rm, reg, sz) -> right $ ADD_reg_reg sz rm reg
         (OpMem rm, reg, sz) -> right $ ADD_mem_reg sz rm reg
      01 -> rm_reg osize >>= \case
         (OpReg rm, reg, sz) -> right $ ADD_reg_reg sz rm reg
         (OpMem rm, reg, sz) -> right $ ADD_mem_reg sz rm reg
      02 -> reg_rm OpSize8 >>= \case
         (reg, OpReg rm, sz) -> right $ ADD_reg_reg sz reg rm
         (reg, OpMem rm, sz) -> right $ ADD_reg_mem sz reg rm
      03 -> reg_rm osize >>= \case
         (reg, OpReg rm, sz) -> right $ ADD_reg_reg sz reg rm
         (reg, OpMem rm, sz) -> right $ ADD_reg_mem sz reg rm
      04 -> ADD_rAX_imm OpSize8 <$> getNeutralImm8
      05 -> ADD_rAX_imm osize   <$> getImm
      80 -> ext_rm OpSize8 >>= \case
         (0, OpReg rm, sz) -> ADD_reg_imm sz rm <$> getNeutralImm8
         (0, OpMem rm, sz) -> ADD_mem_imm sz rm <$> getNeutralImm8
      81 -> ext_rm OpSize8 >>= \case
         (0, OpReg rm, sz) -> ADD_reg_imm sz rm <$> getImm
         (0, OpMem rm, sz) -> ADD_mem_imm sz rm <$> getImm
      83 -> ext_rm osize >>= \case
         (0, OpReg rm, sz) -> ADD_reg_imm sz rm <$> getExtImm
         (0, OpMem rm, sz) -> ADD_mem_imm sz rm <$> getExtImm
