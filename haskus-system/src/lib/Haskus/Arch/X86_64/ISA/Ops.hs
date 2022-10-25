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
  type Location m
  type Marker   m

  -- | Get current location (in bytes)
  getLoc   :: m (Location m)

  -- | Get current default operand size
  getOperandSize :: m DefaultOperandSize

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


newtype CodeGen a = CodeGen (S.State CGState a)
  deriving newtype (Functor,Applicative,Monad)

instance Output CodeGen where
  putW8 b    = CodeGen $ S.modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  getPos = CodeGen $ S.gets cg_pos

instance Asm CodeGen where
  type Location CodeGen = Word
  type Marker   CodeGen = String

  getLoc         = CodeGen (S.gets cg_pos)
  getOperandSize = CodeGen (S.gets cg_op_size)

  addReloc r = CodeGen $ S.modify \s -> s
                  { cg_relocs = r : cg_relocs s
                  }

runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ S.runState m initCGState
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

-- | Add with carry imm8 to AL
putADC_AL_imm8 :: Asm m => Imm8 m -> m ()
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
putADC_AX_imm16 :: Asm m => Imm16 m -> m ()
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
putADC_EAX_imm32 :: Asm m => Imm32 m -> m ()
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
putADC_RAX_imm32 :: Asm m => Imm32 m -> m ()
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


-- > runCodeGen $ putADC_AL_imm8 (V 15) >> putADC_AL_imm8 (M "Imm8 marker") >> putADC_AL_imm8 (V 27) >> putADC_AX_imm16 (V 0x0102)
-- ((),CGState {cg_relocs = [RelocImm8 "Imm8 marker" 3], cg_bytes = [20,15,20,0,20,27,102,21,2,1], cg_pos = 10, cg_op_size = DefaultOperandSize32})
