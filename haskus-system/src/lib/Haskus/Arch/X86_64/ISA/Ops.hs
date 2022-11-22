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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Haskus.Arch.X86_64.ISA.Ops where

import Data.Word
import Data.Int
import Data.Bits
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

-- | Location of a sign-extended 8-bit value
newtype LocImm8SE = LocImm8SE Location

newtype LocDisp8  = LocDisp8  Location
newtype LocDisp16 = LocDisp16 Location
newtype LocDisp32 = LocDisp32 Location

-- | Displacement location (optional)
data LocDispMaybe
  = NoLocDisp
  | SomeLocDisp8  !LocDisp8
  | SomeLocDisp16 !LocDisp16
  | SomeLocDisp32 !LocDisp32

newtype Disp8  = Disp8  Word8
newtype Disp16 = Disp16 Word16
newtype Disp32 = Disp32 Word32

putDisp8 :: Output m => Disp8 -> m LocDisp8
putDisp8 (Disp8 v) = do
  loc <- LocDisp8 <$> getLoc
  putW8 v
  pure loc

putDisp16 :: Output m => Disp16 -> m LocDisp16
putDisp16 (Disp16 v) = do
  loc <- LocDisp16 <$> getLoc
  putW16 v
  pure loc

putDisp32 :: Output m => Disp32 -> m LocDisp32
putDisp32 (Disp32 v) = do
  loc <- LocDisp32 <$> getLoc
  putW32 v
  pure loc

-- | Displacement (optional)
data DispMaybe
  = NoDisp
  | SomeDisp8  !Disp8
  | SomeDisp16 !Disp16
  | SomeDisp32 !Disp32

putDispMaybe :: Output m => DispMaybe -> m LocDispMaybe
putDispMaybe = \case
  NoDisp      -> pure NoLocDisp
  SomeDisp8  d -> SomeLocDisp8  <$> putDisp8 d
  SomeDisp16 d -> SomeLocDisp16 <$> putDisp16 d
  SomeDisp32 d -> SomeLocDisp32 <$> putDisp32 d

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

data REX_B
  = REX_B_0
  | REX_B_1

data REX_R
  = REX_R_0
  | REX_R_1

data REX_W
  = REX_W_0
  | REX_W_1

data REX_X
  = REX_X_0
  | REX_X_1

-- | Put REX prefix if needed
--
-- Throw an exception if constraints can't be fulfilled, i.e. some field
-- requires the REX prefix to be absent and another requires it to be present.
putRexWRXB :: Asm m => Field REX_W -> Field REX_R -> Field REX_X -> Field REX_B -> m ()
{-# INLINE putRexWRXB #-} -- inline as it often simplifies
putRexWRXB w r x b = case wrxb of
    Absent  -> pure ()
    Default -> pure ()
    SetTo v -> putW8 (0b0100000 .|. v)
  where
    wr = merge2 fromREX_W fromREX_R w r
    xb = merge2 fromREX_X fromREX_B x b
    wrxb = merge2 id id wr xb
    {-# INLINE merge2 #-}
    merge2 from_u from_v fu fv = case (fu,fv) of
          (Absent  , Absent)  -> Absent
          (Default , Default) -> Default
          (Default , Absent)  -> Absent
          (Absent  , Default) -> Absent
          (SetTo _ , Absent)  -> error "REX prefix both needed and unallowed"
          (Absent  , SetTo _) -> error "REX prefix both needed and unallowed"
          (SetTo u , Default) -> SetTo (from_u u)
          (Default , SetTo v) -> SetTo (from_v v)
          (SetTo u , SetTo v) -> SetTo (from_u u .|. from_v v)

fromREX_W :: REX_W -> Word8
fromREX_W = \case
  REX_W_0 -> 0b0000
  REX_W_1 -> 0b1000

fromREX_R :: REX_R -> Word8
fromREX_R = \case
  REX_R_0 -> 0b0000
  REX_R_1 -> 0b0100

fromREX_X :: REX_X -> Word8
fromREX_X = \case
  REX_X_0 -> 0b0000
  REX_X_1 -> 0b0010

fromREX_B :: REX_B -> Word8
fromREX_B = \case
  REX_B_0 -> 0b0000
  REX_B_1 -> 0b0001

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

putImm8SE :: Asm m => Word8 -> m LocImm8SE
putImm8SE v = do
  loc <- LocImm8SE <$> getLoc
  putW8 v
  pure loc

putImm32SE :: Asm m => Word32 -> m LocImm32SE
putImm32SE v = do
  loc <- LocImm32SE <$> getLoc
  putW32 v
  pure loc

putLock :: Output m => Lock -> m ()
putLock = \case
  Lock   -> putW8 0xF0
  NoLock -> pure ()

----------------
-- ModRM
----------------

newtype ModRM = RawModRM Word8

data Mod
  = Mod00
  | Mod01
  | Mod10
  | Mod11

fromMod :: Mod -> Word8
fromMod = \case
  Mod00 -> 0b00
  Mod01 -> 0b01
  Mod10 -> 0b10
  Mod11 -> 0b11

toMod :: Word8 -> Mod
toMod = \case
  0b00 -> Mod00
  0b01 -> Mod01
  0b10 -> Mod10
  _    -> Mod11

-- | Subset of Mod for memory addressing
data MemMod
  = MemMod00
  | MemMod01
  | MemMod10

memModToMod :: MemMod -> Mod
memModToMod = \case
  MemMod00 -> Mod00
  MemMod01 -> Mod01
  MemMod10 -> Mod10

-- | ModRM's RM field contents for a memory operand
newtype FieldRM_mem = FieldRM_mem { unFieldRM_mem :: Word3 }

-- | ModRM's RM field contents for a register
newtype FieldRM_reg = FieldRM_reg { unFieldRM_reg :: Word3 }

-- | ModRM's RM field contents for a base register
newtype FieldRM_base = FieldRM_base { unFieldRM_base :: Word3 }

-- | ModRM's Reg field contents for a register
newtype FieldReg_reg = FieldReg_reg { unFieldReg_reg :: Word3 }

-- | ModRM's Reg field contents for an opcode
newtype FieldReg_opcode = FieldReg_opcode { unFieldReg_opcode :: Word3 }

{-# COMPLETE ModRM #-}
pattern ModRM :: Mod -> Word3 -> Word3 -> ModRM
pattern ModRM m r rm <- (extractModRM -> (m,r,rm))
  where
    ModRM m r rm = RawModRM ((fromMod m `unsafeShiftL` 6) .|. (fromWord3 r `unsafeShiftL` 3) .|. fromWord3 rm)

extractModRM :: ModRM -> (Mod, Word3, Word3)
extractModRM (RawModRM w) =
  ( toMod (w `unsafeShiftR` 6)
  , Word3 ((w `unsafeShiftR` 3) .&. 0b111)
  , Word3 (w .&. 0b111)
  )

putModRM :: Asm m => ModRM -> m ()
putModRM (RawModRM w) = putW8 w

-- | Assert that a Word8 is on 3 bits
assertWord3 :: Word8 -> Word3
assertWord3 w
  | w .&. 0b11111000 == 0 = Word3 w
  | otherwise = error $ "assertWord3: number too large (" ++ show w ++ ")"

-- | 3-bit word
newtype Word3
  = Word3 { fromWord3 :: Word8 }
  deriving (Eq)

instance Num Word3 where
  fromInteger v         = assertWord3 (fromInteger v)
  (Word3 a) + (Word3 b) = Word3 ((a+b) .&. 0b111)
  (Word3 a) * (Word3 b) = Word3 ((a*b) .&. 0b111)
  abs a                 = a
  signum (Word3 0)      = Word3 0
  signum _              = Word3 1
  negate _              = error "negate for Word3 not supported"

-- | Assert that a Word8 is on 2 bits
assertWord2 :: Word8 -> Word2
assertWord2 w
  | w .&. 0b11111100 == 0 = Word2 w
  | otherwise = error $ "assertWord2: number too large (" ++ show w ++ ")"

-- | 2-bit word
newtype Word2 = Word2 { fromWord2 :: Word8 }

instance Num Word2 where
  fromInteger v         = assertWord2 (fromInteger v)
  (Word2 a) + (Word2 b) = Word2 ((a+b) .&. 0b11)
  (Word2 a) * (Word2 b) = Word2 ((a*b) .&. 0b11)
  abs a                 = a
  signum (Word2 0)      = Word2 0
  signum _              = Word2 1
  negate _              = error "negate for Word2 not supported"

modRM_OpcodeReg :: FieldReg_opcode -> FieldRM_reg -> ModRM
modRM_OpcodeReg opcode reg = ModRM Mod11 (unFieldReg_opcode opcode) (unFieldRM_reg reg)

-- | Put opcode, opcode extension, and register
--
-- Opcode extension and register are stored in ModRM.{reg,rm} respectively.
putOpcodeExt_Reg :: Asm m => Word8 -> Word3 -> FieldRM_reg -> m ()
putOpcodeExt_Reg opcode ext reg = do
  putOpcode opcode
  putModRM (modRM_OpcodeReg (FieldReg_opcode ext) reg)

modRM_RegReg :: FieldReg_reg -> FieldRM_reg -> ModRM
modRM_RegReg reg1 reg2 = ModRM Mod11 (unFieldReg_reg reg1) (unFieldRM_reg reg2)

modRM_OpcodeMem :: FieldReg_opcode -> MemMod -> FieldRM_mem -> ModRM
modRM_OpcodeMem opcode m_mod m_rm
  = ModRM (memModToMod m_mod) (unFieldReg_opcode opcode) (unFieldRM_mem m_rm)

-- | Some registers are encoded with 3 bits, some others with 4 bits, some
-- others can be encoded with either 3 or 4 bits.
--
-- The number of available bits to encode a register is variable because
-- prefixes (especially REX) are optional and contain the additional bits.
--
-- That's why some registers encoded only on 3-bits (AH, BH, CH, DH) can't be
-- encoded when a REX prefix is used (for another reason). That's why some
-- registers encoded only on 4-bits (SIL, DIL, SPL, BPL) can't be encoded
-- whithout a REX prefix.
--
-- We can't just encode a register with its number, we need to keep track of its
-- number of bits. We use the 2 MSB of the reg code to indicate if it must be encoded on
-- 3 bits (MSB = 0b01), on 4 bits (MSB = 0b10), or on any number of bits (MSB =
-- 0b00). The actual value of the additional bit is at its expected position:
-- bit 3 (counting from 0).
--
-- We could use an ADT instead of packing RegBits and RegNum in a Word8. This is
-- an example of premature optimization.
newtype RegCode = RawRegCode Word8

data RegBits
  = Only3Bits
  | Only4Bits
  | AnyBits

fromRegBits :: RegBits -> Word8
fromRegBits = \case
  AnyBits   -> 0b00000000
  Only3Bits -> 0b01000000
  Only4Bits -> 0b10000000

toRegBits :: Word8 -> RegBits
toRegBits w = case w .&. 0b1100000 of
  0b00000000         -> AnyBits
  0b01000000         -> Only3Bits
  _ {- 0b10000000 -} -> Only4Bits

toRegNum :: Word8 -> RegNum
toRegNum w = RegNum (w .&. 0b00111111)

newtype RegNum = RegNum { fromRegNum :: Word8 }

{-# COMPLETE RegCode #-}
pattern RegCode :: RegBits -> RegNum -> RegCode
pattern RegCode b n <- (extractRegCode -> (b,n))
  where
    RegCode b n = toRegCode b n

extractRegCode :: RegCode -> (RegBits, RegNum)
extractRegCode (RawRegCode w) = (toRegBits w, toRegNum w)

toRegCode :: RegBits -> RegNum -> RegCode
toRegCode b n = RawRegCode (fromRegBits b .|. fromRegNum n)

data Field a
  = Absent  -- ^ Need the field (e.g. REX.B) to be absent (i.e. no REX prefix at all)
  | Default -- ^ The field may be present or not (we use its value by default)
  | SetTo a -- ^ Need the field (e.g. REX.B) to be present and set to the given value
  deriving (Functor)

-- | Indicate if a register encoding requires an additional bit, and its value.
regEncoding :: RegCode -> (Field Bool, Word3)
regEncoding (RegCode bits num) =
  let n = fromRegNum num
  in case bits of
      Only3Bits -> (Absent             , Word3 n)
      Only4Bits -> (SetTo (testBit n 3), Word3 (n .&. 0b0111))
      AnyBits   -> (Default            , Word3 n)

-- | Encode a register in RM field of ModRM
encodeFieldRM_reg :: RegCode -> (Field REX_B, FieldRM_reg)
encodeFieldRM_reg c = (fmap toREX_B mextra, FieldRM_reg n)
  where
    (mextra,n) = regEncoding c
    toREX_B = \case
      False -> REX_B_0
      True  -> REX_B_1

-- | Encode a base register in RM field of ModRM
encodeFieldRM_base :: RegCode -> (Field REX_B, FieldRM_base)
encodeFieldRM_base c = (fmap toREX_B mextra, FieldRM_base n)
  where
    (mextra,n) = regEncoding c
    toREX_B = \case
      False -> REX_B_0
      True  -> REX_B_1

-- | Encode a register in Reg field of ModRM
encodeFieldReg_reg :: RegCode -> (Field REX_R, FieldReg_reg)
encodeFieldReg_reg c = (fmap toREX_R mextra, FieldReg_reg n)
  where
    (mextra,n) = regEncoding c
    toREX_R = \case
      False -> REX_R_0
      True  -> REX_R_1

-- | Some instructions have two encodings because they have a reverse bit that
-- can be used to switch source/destination operands. When operands are of the
-- same type (e.g. r8/r8), we can set the reverse bit or not, and select the
-- operand encoding order accordingly.
data ReverseBit
  = ReverseBit1
  | ReverseBit0

-- | Compute appropriate ModRM, REX_B, REX_R, and opcode, according to the
-- reverse bit.
reversable_RegRM :: ReverseBit -> Word8 -> RegCode -> RegCode -> (Field REX_R, Field REX_B, ModRM, Word8)
reversable_RegRM rev opcode dst src = (rex_r, rex_b, modrm, opcode')
  where
    modrm        = modRM_RegReg reg rm
    (rrm,rreg)   = case rev of
                      ReverseBit0 -> (dst,src)
                      ReverseBit1 -> (src,dst)
    (rex_b, rm)  = encodeFieldRM_reg rrm
    (rex_r, reg) = encodeFieldReg_reg rreg
    opcode'      = case rev of
                      ReverseBit0 -> opcode
                      ReverseBit1 -> setBit opcode 1

data Segment = CS | DS | ES | FS | GS | SS

putSegmentOverrideMaybe :: Output m => Maybe Segment -> m ()
putSegmentOverrideMaybe = \case
  Nothing -> pure ()
  Just s  -> putSegmentOverride s

putSegmentOverride :: Output m => Segment -> m ()
putSegmentOverride = \case
  CS -> putW8 0x2E
  DS -> putW8 0x3E
  ES -> putW8 0x26
  FS -> putW8 0x64
  GS -> putW8 0x65
  SS -> putW8 0x36

data AddrSize
  = Addr64 -- ^ 64-bit address size (default)
  | Addr32 -- ^ 32-bit address size

putAddrSizeOverrideMaybe :: Output m => Maybe AddrSize -> m ()
putAddrSizeOverrideMaybe = \case
  Nothing -> pure ()
  Just s  -> putAddrSizeOverride s

putAddrSizeOverride :: Output m => AddrSize -> m ()
putAddrSizeOverride = \case
  Addr64 -> pure ()
  Addr32 -> putW8 0x67

data Addr = Addr
  { addr_ea      :: !EAddr            -- ^ Effective address
  , addr_segment :: !(Maybe Segment)  -- ^ Segment override
  , addr_size    :: !(Maybe AddrSize) -- ^ Address-size override
  }

data EAddr = EAddr
  { ea_base  :: !(Maybe RegCode) -- ^ Base register
  , ea_index :: !(Maybe RegCode) -- ^ Index register
  , ea_scale :: !Scale           -- ^ Index scale
  , ea_disp  :: !(Maybe EADisp)  -- ^ Optional displacement
  }

data EADisp
  = EADisp8  Disp8
  | EADisp32 Disp32

data Scale
  = Scale1
  | Scale2
  | Scale4
  | Scale8

fromScale :: Scale -> Word8
fromScale = \case
  Scale1 -> 0b00000000
  Scale2 -> 0b01000000
  Scale4 -> 0b10000000
  Scale8 -> 0b11000000

toScale :: Word8 -> Scale
toScale x = case x .&. 0b11000000 of
  0b00000000 -> Scale1
  0b01000000 -> Scale2
  0b10000000 -> Scale4
  _          -> Scale8

computeAddrFields
  :: Addr
  -> ( Maybe Segment
     , Maybe AddrSize
     , MemMod
     , FieldRM_mem
     , Maybe SIB
     , DispMaybe
     , Field REX_X
     , Field REX_B
     )
computeAddrFields (Addr ea mseg msize)
  = (mseg, msize, m_mod, m_rm, sib, disp, rex_x, rex_b)
  where
    -- we don't remove redundant segment override here, the responsibility is
    -- left to the caller to canonicalize the address.
    (m_mod, m_rm, sib, disp, rex_x, rex_b) = computeEAddrFields ea

computeEAddrFields
  :: EAddr
  -> ( MemMod
     , FieldRM_mem
     , Maybe SIB
     , DispMaybe
     , Field REX_X
     , Field REX_B
     )
computeEAddrFields = \case
  -- encode address 0 as disp8 of 0
  EAddr Nothing Nothing scale Nothing
    -> computeEAddrFields (EAddr Nothing Nothing scale (Just (EADisp8 (Disp8 0))))

  -- encode disp8 only as disp32 only
  EAddr Nothing Nothing scale (Just (EADisp8 (Disp8 d)))
    -> computeEAddrFields (EAddr Nothing Nothing scale (Just (EADisp32 (Disp32 (fromIntegral d)))))

  -- encode disp32 only (using SIB)
  EAddr Nothing Nothing scale (Just (EADisp32 d))
    -> ( MemMod00
       , FieldRM_mem 0b100
       , Just (SIB scale 0b100 0b101)
       , SomeDisp32 d
       , Default
       , Default
       )

  -- TODO: RIP+disp32 addressing
  -- (does it support segment override?)

  -- base register lower bits are 0b101 (rBP, r13) without disp
  -- Need to be encoded with disp=0
  EAddr b@(Just base) index scale Nothing
    | (_rex_b, FieldRM_base rm) <- encodeFieldRM_base base
    , rm == 0b101
    -> computeEAddrFields (EAddr b index scale (Just (EADisp8 (Disp8 0))))

  -- base register lower bits are 0b101 (rBP, r13) with disp
  EAddr (Just base) Nothing _scale (Just eadisp)
    | (rex_b, FieldRM_base rm) <- encodeFieldRM_base base
    , rm == 0b101
    , (mode,disp) <- case eadisp of
        EADisp8  d -> (MemMod01, SomeDisp8 d)
        EADisp32 d -> (MemMod10, SomeDisp32 d)
    -> (mode, FieldRM_mem rm, Nothing, disp, Default, rex_b)

  -- base register lower bits different from 0b100 and 0b101 (handled above)
  EAddr (Just base) Nothing _scale mdisp
    | (rex_b, FieldRM_base rm) <- encodeFieldRM_base base
    , (mode,disp) <- case mdisp of
        Nothing           -> (MemMod00, NoDisp)
        Just (EADisp8  d) -> (MemMod01, SomeDisp8 d)
        Just (EADisp32 d) -> (MemMod10, SomeDisp32 d)
    -> (mode, FieldRM_mem rm, Nothing, disp, Default, rex_b)


  -- base register lower bits are 0b100 (rSP, r12), requires SIB byte

newtype SIB = RawSIB Word8

{-# COMPLETE SIB #-}
pattern SIB :: Scale -> Word3 -> Word3 -> SIB
pattern SIB scale index base <- (extractSIB -> (scale,index,base))
  where
    SIB scale index base = RawSIB (fromScale scale .|. (fromWord3 index `unsafeShiftL` 3) .|. fromWord3 base)

extractSIB :: SIB -> (Scale, Word3, Word3)
extractSIB (RawSIB w) =
  ( toScale w
  , Word3 ((w `unsafeShiftR` 3) .&. 0b111)
  , Word3 (w .&. 0b111)
  )

putSIBMaybe :: Output m => Maybe SIB -> m ()
putSIBMaybe = \case
  Nothing -> pure ()
  Just v  -> putSIB v

putSIB :: Output m => SIB -> m ()
putSIB (RawSIB w) = putW8 w

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

-- | Add imm8 to AL + CF
newtype ADC_AL_i8 = ADC_AL_i8 Word8

-- | Add imm16 to AX + CF
newtype ADC_AX_i16 = ADC_AX_i16 Word16

-- | Add imm32 to EAX + CF
newtype ADC_EAX_i32 = ADC_EAX_i32 Word32

-- | Add sign-extended imm32 to RAX + CF
newtype ADC_RAX_i32 = ADC_RAX_i32 Word32

-- | Add imm8 to reg8 + CF
data ADC_r8_i8 = ADC_r8_i8 !RegCode !Word8

-- | Add imm16 to reg16 + CF
data ADC_r16_i16 = ADC_r16_i16 !RegCode !Word16

-- | Add imm32 to reg32 + CF
data ADC_r32_i32 = ADC_r32_i32 !RegCode !Word32

-- | Add sign-extended imm32 to reg64 + CF
data ADC_r64_i32 = ADC_r64_i32 !RegCode !Word32

-- | Add sign-extended imm8 to reg16 + CF
data ADC_r16_i8 = ADC_r16_i8 !RegCode !Word8

-- | Add sign-extended imm8 to reg32 + CF
data ADC_r32_i8 = ADC_r32_i8 !RegCode !Word8

-- | Add sign-extended imm8 to reg64 + CF
data ADC_r64_i8 = ADC_r64_i8 !RegCode !Word8

-- | Add reg8 to reg8 + CF
data ADC_r8_r8 = ADC_r8_r8
  { adc_r8_r8_dst :: !RegCode
  , adc_r8_r8_src :: !RegCode
  , adc_r8_r8_rev :: !ReverseBit
  }

-- | Add reg16 to reg16 + CF
data ADC_r16_r16 = ADC_r16_r16
  { adc_r16_r16_dst :: !RegCode
  , adc_r16_r16_src :: !RegCode
  , adc_r16_r16_rev :: !ReverseBit
  }

-- | Add reg32 to reg32 + CF
data ADC_r32_r32 = ADC_r32_r32
  { adc_r32_r32_dst :: !RegCode
  , adc_r32_r32_src :: !RegCode
  , adc_r32_r32_rev :: !ReverseBit
  }

-- | Add reg64 to reg64 + CF
data ADC_r64_r64 = ADC_r64_r64
  { adc_r64_r64_dst :: !RegCode
  , adc_r64_r64_src :: !RegCode
  , adc_r64_r64_rev :: !ReverseBit
  }

-- | Add imm8 to mem8 + CF
data ADC_m8_i8 = ADC_m8_i8
  { adc_m8_i8_lock :: !Lock
  , adc_m8_i8_dst  :: !Addr
  , adc_m8_i8_src  :: !Word8
  }

-- Machine code generation
--------------------------

instance Put ADC_AL_i8 where
  -- return offset of the imm8 value
  type PutResult ADC_AL_i8 = LocImm8

  put (ADC_AL_i8 v) = do
    putOpcode 0x14
    putImm8 v

instance Put ADC_AX_i16 where
  -- return offset of the imm16 value
  type PutResult ADC_AX_i16 = LocImm16

  put (ADC_AX_i16 v) = do
    setOperandSize16
    putOpcode 0x15
    putImm16 v

instance Put ADC_EAX_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_EAX_i32 = LocImm32

  put (ADC_EAX_i32 v) = do
    setOperandSize32
    putOpcode 0x15
    putImm32 v

instance Put ADC_RAX_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_RAX_i32 = LocImm32SE

  put (ADC_RAX_i32 v) = do
    putRexW
    putOpcode 0x15
    putImm32SE v

instance Put ADC_r8_i8 where
  -- return offset of the imm8 value
  type PutResult ADC_r8_i8 = LocImm8

  put (ADC_r8_i8 r v) = do
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB Default Default Default rex_b
    putOpcodeExt_Reg 0x80 2 r_lsb
    putImm8 v

instance Put ADC_r16_i16 where
  -- return offset of the imm16 value
  type PutResult ADC_r16_i16 = LocImm16

  put (ADC_r16_i16 r v) = do
    setOperandSize16
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB Default Default Default rex_b
    putOpcodeExt_Reg 0x81 2 r_lsb
    putImm16 v

instance Put ADC_r32_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_r32_i32 = LocImm32

  put (ADC_r32_i32 r v) = do
    setOperandSize32
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB Default Default Default rex_b
    putOpcodeExt_Reg 0x81 2 r_lsb
    putImm32 v

instance Put ADC_r64_i32 where
  type PutResult ADC_r64_i32 = LocImm32SE

  put (ADC_r64_i32 r v) = do
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB (SetTo REX_W_1) Default Default rex_b
    putOpcodeExt_Reg 0x81 2 r_lsb
    putImm32SE v

instance Put ADC_r16_i8 where
  type PutResult ADC_r16_i8 = LocImm8SE

  put (ADC_r16_i8 r v) = do
    setOperandSize16
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB Default Default Default rex_b
    putOpcodeExt_Reg 0x83 2 r_lsb
    putImm8SE v

instance Put ADC_r32_i8 where
  type PutResult ADC_r32_i8 = LocImm8SE

  put (ADC_r32_i8 r v) = do
    setOperandSize32
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB Default Default Default rex_b
    putOpcodeExt_Reg 0x83 2 r_lsb
    putImm8SE v

instance Put ADC_r64_i8 where
  type PutResult ADC_r64_i8 = LocImm8SE

  put (ADC_r64_i8 r v) = do
    let (rex_b, r_lsb) = encodeFieldRM_reg r
    putRexWRXB (SetTo REX_W_1) Default Default rex_b
    putOpcodeExt_Reg 0x83 2 r_lsb
    putImm8SE v

instance Put ADC_r8_r8 where
  type PutResult ADC_r8_r8 = ()

  put (ADC_r8_r8 dst src rev) = do
    let (rex_r, rex_b, modrm, opcode) = reversable_RegRM rev 0x10 dst src
    putRexWRXB Default rex_r Default rex_b
    putOpcode opcode
    putModRM modrm

instance Put ADC_r16_r16 where
  type PutResult ADC_r16_r16 = ()

  put (ADC_r16_r16 dst src rev) = do
    let (rex_r, rex_b, modrm, opcode) = reversable_RegRM rev 0x11 dst src
    setOperandSize16
    putRexWRXB Default rex_r Default rex_b
    putOpcode opcode
    putModRM modrm

instance Put ADC_r32_r32 where
  type PutResult ADC_r32_r32 = ()

  put (ADC_r32_r32 dst src rev) = do
    let (rex_r, rex_b, modrm, opcode) = reversable_RegRM rev 0x11 dst src
    setOperandSize32
    putRexWRXB Default rex_r Default rex_b
    putOpcode opcode
    putModRM modrm

instance Put ADC_r64_r64 where
  type PutResult ADC_r64_r64 = ()

  put (ADC_r64_r64 dst src rev) = do
    let (rex_r, rex_b, modrm, opcode) = reversable_RegRM rev 0x11 dst src
    putRexWRXB (SetTo REX_W_1) rex_r Default rex_b
    putOpcode opcode
    putModRM modrm

instance Put ADC_m8_i8 where
  type PutResult ADC_m8_i8 = (LocDispMaybe, LocImm8)

  put (ADC_m8_i8 lock m v) = do
    putLock lock
    let (mseg, masize, m_mod, m_rm, sib, disp, rex_x, rex_b) = computeAddrFields m
    putSegmentOverrideMaybe mseg
    putAddrSizeOverrideMaybe masize
    putRexWRXB Default Default rex_x rex_b
    putOpcode 0x80
    putModRM (modRM_OpcodeMem (FieldReg_opcode 2) m_mod m_rm)
    putSIBMaybe sib
    loc_disp <- putDispMaybe disp
    loc_imm <- putImm8 v
    pure (loc_disp, loc_imm)

-- > runCodeGen $ putADC_AL_imm8 (V 15) >> putADC_AL_imm8 (M "Imm8 marker") >> putADC_AL_imm8 (V 27) >> putADC_AX_imm16 (V 0x0102)
-- ((),CGState {cg_relocs = [RelocImm8 "Imm8 marker" 3], cg_bytes = [20,15,20,0,20,27,102,21,2,1], cg_pos = 10, cg_op_size = DefaultOperandSize32})
