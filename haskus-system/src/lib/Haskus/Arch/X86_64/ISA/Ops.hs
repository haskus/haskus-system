{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Haskus.Arch.X86_64.ISA.Ops where

import Data.Word
import Data.Int
import qualified Control.Monad.State as S
import Data.Bifunctor

import Haskus.Arch.X86_64.ISA.Output

-- General purpose instructions
data GPInsn cond mem reg imm
  -- Data transfer
  = MoveMemToReg mem reg
  | MoveRegToMem reg mem
  | MoveRegToReg reg reg
  | MoveImmToReg imm reg
  | MoveImmToMem imm mem

  | CondMoveMemToReg cond mem reg
  | CondMoveRegToMem cond reg mem
  | CondMoveRegToReg cond reg reg
  | CondMoveImmToReg cond imm reg
  | CondMoveImmToMem cond imm mem

  -- Exchange
  | Exchange
  | ExchangeThenAdd
  | CompareExchange Lock

  | ByteSwap

  -- Stack manipulation
  | StackPush
  | StackPop
  | StackPushAllRegs
  | StackPopAllRegs

  -- Conversions
  | SignExtendInplace   -- CBW/CWDE
  | SignExtensionInto   -- CWD/CDQ
  | SignExtendInto      -- MOVSX/MOVSXD
  | ZeroExtendInto      -- MOVZX

  -- Arithmetic
  | Add
  | AddWithCarry
  | Sub
  | SubWithBorrow
  | Increment
  | Decrement
  | Negate
  | MultiplyUnsigned
  | MultiplySigned
  | DivideUnsigned
  | DivideSigned

  -- Comparison
  | Compare
  | Test

  -- Logical
  | And
  | Or
  | Xor
  | Not
  | ShiftLeft
  | ShiftRight
  | ShiftRightSigned
  | ShiftLeftInto           -- SHLD
  | ShiftRightInto          -- SHRD
  | RotateLeft              -- ROL
  | RotateRight             -- ROR
  | RotateThroughCarryLeft  -- RCL
  | RotateThroughCarryRight -- RCR

  -- Bit test
  | BitTest
  | BitTestAndSet
  | BitTestAndClear
  | BitTestAndComplement

  -- Bit scan
  | BitScanLowToHigh -- BSF
  | BitScanHighToLow -- BSR

  -- Byte set
  | CondSet cond -- SETcc

  -- Control transfer
  | Jump
  | Call
  | Return
  | CondJump
  | Loop LoopCond
  | JumpOnZeroCounter -- JCXZ/JECXZ
  | ProcEnter         -- ENTER
  | ProcLeave         -- LEAVE

  -- Interruptions
  | Interrupt
  | InterruptReturn
  | InterruptOnOverflow -- INTO

  -- String instructions
  -- TODO
  -- (MOVS, CMPS, SCAS, LODS, STOS)
  -- (REP, REPE/Z, REPNE/NZ)

  -- I/O instructions
  | InputIntoReg        -- IN
  | InputIntoString     -- INS
  | OutputFromReg       -- OUT
  | OutputFromString    -- OUTS

  -- Flags control
  | SetCarryFlag        -- STC
  | ClearCarryFlag      -- CLC
  | ComplementCarryFlag -- CMC
  | SetDirectionFlag    -- STD
  | ClearDirectionFlag  -- CLD
  | LoadFlags           -- LAHF
  | StoreFlags          -- SAHF
  | PushFlags           -- PUSHF
  | PopFlags            -- POPF
  | PushAllFlags        -- PUSHFD
  | PopAllFlags         -- POPFD
  | SetInterruptFlag    -- STI
  | ClearInterruptFlag  -- CLI

  -- Miscellaneous
  | ComputeAddress      -- LEA
  | TableLookup         -- XLAT/XLATB
  | CPUID
  | NOP
  | Undefined           -- UD (invalid opcode)

data Lock
  = Lock   -- ^ Lock memory access during the operation
  | NoLock -- ^ Don't lock memory

data LoopCond


-- | Relocation
data Reloc mark loc
  = RelocImm8 !mark !loc
      -- ^ Write imm8 value for marker at the given location
  | RelocImm16 !mark !loc
      -- ^ Write imm16 value for marker at the given location
  | RelocImm32 !mark !loc
      -- ^ Write imm32 value for marker at the given location
  deriving (Show)

data DefaultOperandSize
  = DefaultOperandSize16
  | DefaultOperandSize32
  deriving (Show)

-- | Assembler code generator type-class
--
-- For most instructions, it supports value arguments or markers. Markers
-- produce relocations that can be fixed up in a following pass.
class Output m => Asm m where
  -- | Get current default operand size
  getOperandSize :: m DefaultOperandSize

data CGState = CGState
  { cg_bytes   :: ![Word8]
  , cg_pos     :: !Location
  , cg_op_size :: !DefaultOperandSize
  }
  deriving (Show)

initCGState :: CGState
initCGState = CGState [] 0 DefaultOperandSize32


newtype CodeGen a = CodeGen (S.State CGState a)
  deriving newtype (Functor,Applicative,Monad)

instance Output CodeGen where
  putW8 b    = CodeGen $ S.modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  getLoc = CodeGen (S.gets cg_pos)

instance Asm CodeGen where
  getOperandSize = CodeGen (S.gets cg_op_size)

runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ S.runState m initCGState
  where
    fix_state s = s
      { cg_bytes  = reverse (cg_bytes s)
      }

newtype LocImm8  = LocImm8  Location
newtype LocImm16 = LocImm16 Location
newtype LocImm32 = LocImm32 Location
newtype LocImm64 = LocImm64 Location

-- | Location of a sign-extended 32-bit value
newtype LocImm32SE = LocImm32SE Location

-- | Enable 16-bit operand size if it isn't the default
setOperandSize16 :: Asm m => m ()
setOperandSize16 = getOperandSize >>= \case
  DefaultOperandSize16 -> pure ()
  DefaultOperandSize32 -> putW8 0x66

-- | Enable 32-bit operand size if it isn't the default
setOperandSize32 :: Asm m => m ()
setOperandSize32 = getOperandSize >>= \case
  DefaultOperandSize16 -> putW8 0x66
  DefaultOperandSize32 -> pure ()
  
-- | Enable 64-bit operand size (REX.W)
putRexW :: Asm m => m ()
putRexW = putW8 0b01001000

putOpcode :: Asm m => Word8 -> m ()
putOpcode = putW8

putImm8 :: Asm m => Word8 -> m LocImm8
putImm8 v = do
  loc <- LocImm8 <$> getLoc
  putW8 v
  pure loc

putImm16 :: Asm m => Word16 -> m LocImm16
putImm16 v = do
  loc <- LocImm16 <$> getLoc
  putW16 v
  pure loc

putImm32 :: Asm m => Word32 -> m LocImm32
putImm32 v = do
  loc <- LocImm32 <$> getLoc
  putW32 v
  pure loc

putImm32SE :: Asm m => Word32 -> m LocImm32SE
putImm32SE v = do
  loc <- LocImm32SE <$> getLoc
  putW32 v
  pure loc

putModRM :: Asm m => Word8 -> m ()
putModRM = putW8

-- ========================================
-- Instructions
-- ========================================

class Put a where
  type PutResult a
  put :: Asm m => a -> m (PutResult a)

instance Put Word8  where
  type PutResult Word8 = ()
  put = putW8

instance Put Word16 where
  type PutResult Word16 = ()
  put = putW16

instance Put Word32 where
  type PutResult Word32 = ()
  put = putW32

instance Put Word64 where
  type PutResult Word64 = ()
  put = putW64

instance Put Int8   where
  type PutResult Int8 = ()
  put = putI8

instance Put Int16  where
  type PutResult Int16 = ()
  put = putI16

instance Put Int32  where
  type PutResult Int32 = ()
  put = putI32

instance Put Int64  where
  type PutResult Int64 = ()
  put = putI64

------------------------------------------
-- ADC: add with carry
------------------------------------------

-- Variants
-----------

-- | Add with carry imm8 to AL
newtype ADC_AL_imm8 = ADC_AL_imm8 Word8

-- | Add with carry imm16 to AX
newtype ADC_AX_imm16 = ADC_AX_imm16 Word16

-- | Add with carry imm32 to EAX
newtype ADC_EAX_imm32 = ADC_EAX_imm32 Word32

-- | Add with carry sign-extended imm32 to RAX
newtype ADC_RAX_imm32 = ADC_RAX_imm32 Word32

-- Machine code generation
--------------------------

instance Put ADC_AL_imm8 where
  -- return offset of the imm8 value
  type PutResult ADC_AL_imm8 = LocImm8

  put (ADC_AL_imm8 v) = do
    putOpcode 0x14
    putImm8 v

instance Put ADC_AX_imm16 where
  -- return offset of the imm16 value
  type PutResult ADC_AX_imm16 = LocImm16

  put (ADC_AX_imm16 v) = do
    setOperandSize16
    putOpcode 0x15
    putImm16 v

instance Put ADC_EAX_imm32 where
  -- return offset of the imm32 value
  type PutResult ADC_EAX_imm32 = LocImm32

  put (ADC_EAX_imm32 v) = do
    setOperandSize32
    putOpcode 0x15
    putImm32 v

instance Put ADC_RAX_imm32 where
  -- return offset of the imm32 value
  type PutResult ADC_RAX_imm32 = LocImm32SE

  put (ADC_RAX_imm32 v) = do
    putRexW
    putOpcode 0x15
    putImm32SE v



-- > runCodeGen $ putADC_AL_imm8 (V 15) >> putADC_AL_imm8 (M "Imm8 marker") >> putADC_AL_imm8 (V 27) >> putADC_AX_imm16 (V 0x0102)
-- ((),CGState {cg_relocs = [RelocImm8 "Imm8 marker" 3], cg_bytes = [20,15,20,0,20,27,102,21,2,1], cg_pos = 10, cg_op_size = DefaultOperandSize32})
