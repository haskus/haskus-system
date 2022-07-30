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

module Haskus.Arch.X86_64.ISA.Ops where

import Data.Word
import Control.Monad.State
import Data.Bifunctor
import Data.Bits

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
class Monad m => Gen m where
  type Location m
  type Marker   m

  -- | Get current location (in bytes)
  getLoc   :: m (Location m)

  -- | Get current default operand size
  getOperandSize :: m DefaultOperandSize

  -- | Write a Word8
  putW8    :: Word8 -> m ()

  -- | Add a relocation
  addReloc :: Reloc (Marker m) (Location m) -> m ()


data CGState = CGState
  { cg_relocs  :: ![Reloc (Marker CodeGen) (Location CodeGen)]
  , cg_bytes   :: ![Word8]
  , cg_pos     :: !Word
  , cg_op_size :: !DefaultOperandSize
  }
  deriving (Show)

initCGState :: CGState
initCGState = CGState [] [] 0 DefaultOperandSize32


newtype CodeGen a = CodeGen (State CGState a)
  deriving newtype (Functor,Applicative,Monad)

instance Gen CodeGen where
  type Location CodeGen = Word
  type Marker   CodeGen = String

  getLoc         = CodeGen (gets cg_pos)
  getOperandSize = CodeGen (gets cg_op_size)

  putW8 b    = CodeGen $ modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  addReloc r = CodeGen $ modify \s -> s
                  { cg_relocs = r : cg_relocs s
                  }

runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ runState m initCGState
  where
    fix_state s = s
      { cg_bytes  = reverse (cg_bytes s)
      , cg_relocs = reverse (cg_relocs s)
      }

-- | Constant value or marker
data ValueOrMarker a m
  = V a
  | M (Marker m)

type Imm8  m = ValueOrMarker Word8  m
type Imm16 m = ValueOrMarker Word16 m
type Imm32 m = ValueOrMarker Word32 m

-- | Put a 16-bit immediate
putW16 :: Gen m => Word16 -> m ()
putW16 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))

-- | Put a 32-bit immediate
putW32 :: Gen m => Word32 -> m ()
putW32 v = do
  putW8 (fromIntegral v)
  putW8 (fromIntegral (v `unsafeShiftR` 8))
  putW8 (fromIntegral (v `unsafeShiftR` 16))
  putW8 (fromIntegral (v `unsafeShiftR` 24))

-- | Enable 16-bit operand size if it isn't the default
setOperandSize16 :: Gen m => m ()
setOperandSize16 = getOperandSize >>= \case
  DefaultOperandSize16 -> pure ()
  DefaultOperandSize32 -> putW8 0x66

-- | Enable 32-bit operand size if it isn't the default
setOperandSize32 :: Gen m => m ()
setOperandSize32 = getOperandSize >>= \case
  DefaultOperandSize16 -> putW8 0x66
  DefaultOperandSize32 -> pure ()
  
-- | Enable 64-bit operand size (REX.W)
putRexW :: Gen m => m ()
putRexW = putW8 0b01001000

-- | Add with carry imm8 to AL
putADC_AL_imm8 :: Gen m => Imm8 m -> m ()
putADC_AL_imm8 a = do
  putW8 0x14
  case a of
    V v -> do
      putW8 v
    M m -> do
      l <- getLoc
      addReloc (RelocImm8 m l)
      putW8 0x00 -- placeholder

-- | Add with carry imm16 to AX
putADC_AX_imm16 :: Gen m => Imm16 m -> m ()
putADC_AX_imm16 a = do
  setOperandSize16
  putW8 0x15
  case a of
    V v -> do
      putW16 v
    M m -> do
      l <- getLoc
      addReloc (RelocImm16 m l)
      putW8 0x00 -- placeholder
      putW8 0x00

-- | Add with carry imm32 to EAX
putADC_EAX_imm32 :: Gen m => Imm32 m -> m ()
putADC_EAX_imm32 a = do
  setOperandSize32
  putW8 0x15
  case a of
    V v -> do
      putW32 v
    M m -> do
      l <- getLoc
      addReloc (RelocImm32 m l)
      putW8 0x00 -- placeholder
      putW8 0x00
      putW8 0x00
      putW8 0x00

-- | Add with carry imm32 sign-extended to 64-bits to RAX
putADC_RAX_imm32 :: Gen m => Imm32 m -> m ()
putADC_RAX_imm32 a = do
  putRexW
  putW8 0x15
  case a of
    V v -> do
      putW32 v
    M m -> do
      l <- getLoc
      addReloc (RelocImm32 m l)
      putW8 0x00 -- placeholder
      putW8 0x00
      putW8 0x00
      putW8 0x00
