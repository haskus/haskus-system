{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Haskus.Arch.X86_64.ISA.Ops
  ( Lock (..)
  , Addr (..)
  , Addr64 (..)
  , AddrSize (..)
  , BSID (..)
  , Disp8 (..)
  , Disp16 (..)
  , Disp32 (..)
  , Segment (..)
  , LocImm8 (..)
  , LocImm8sx (..)
  , LocImm16 (..)
  , LocImm32 (..)
  , LocImm32sx (..)
  , LocImm64 (..)
  , LocDispMaybe (..)
  , LocDisp8 (..)
  , LocDisp16 (..)
  , LocDisp32 (..)
  , ReverseBit (..)
  , RegCode (..)
  , pattern RegCode
  , pattern RAX
  , pattern RBX
  , pattern RCX
  , pattern RDX
  , oc
  , i8
  , i8sx
  , i16
  , i32
  , i32sx
  , rex
  , rexW
  , regRM
  , pattern W
  , pattern W0
  , pattern W1
  , pattern R
  , pattern R1
  , pattern R0
  , pattern X
  , pattern X0
  , pattern X1
  , pattern B
  , pattern B0
  , pattern B1
  , ocxReg
  , ocxMem
  , revRegs
  , putModRM
  , addrFields
  , addrSize
  , addrSizeMaybe
  , sibMaybe
  , dispMaybe
  , lockMaybe
  , segMaybe
  -- Instruction encoding schemes
  , gen_r8_i8
  , gen_r16_i16
  , gen_r32_i32
  , gen_r64_i32sx
  , gen_r16_i8sx
  , gen_r32_i8sx
  , gen_r64_i8sx
  , gen_r8_r8_rev
  , gen_r16_r16_rev
  , gen_r32_r32_rev
  , gen_r64_r64_rev
  , gen_m8_i8
  , gen_m16_i16
  , gen_m32_i32
  , gen_m64_i32sx
  , module Haskus.Arch.X86_64.ISA.Put
  , module Haskus.Arch.X86_64.ISA.Output
  , module Haskus.Arch.X86_64.ISA.Asm
  , module Data.Word
  , module Data.Int
  )
where

import Data.Word
import Data.Int
import Data.Bits

import Haskus.Arch.X86_64.ISA.Output
import Haskus.Arch.X86_64.ISA.Put
import Haskus.Arch.X86_64.ISA.Asm

data Lock
  = Lock   -- ^ Lock memory access during the operation
  | NoLock -- ^ Don't lock memory

newtype LocImm8  = LocImm8  Location deriving (Show,Eq,Ord)
newtype LocImm16 = LocImm16 Location deriving (Show,Eq,Ord)
newtype LocImm32 = LocImm32 Location deriving (Show,Eq,Ord)
newtype LocImm64 = LocImm64 Location deriving (Show,Eq,Ord)

-- | Location of a sign-extended 32-bit value
newtype LocImm32sx = LocImm32sx Location deriving (Show,Eq,Ord)

-- | Location of a sign-extended 8-bit value
newtype LocImm8sx = LocImm8sx Location deriving (Show,Eq,Ord)

newtype LocDisp8  = LocDisp8  Location deriving (Show,Eq,Ord)
newtype LocDisp16 = LocDisp16 Location deriving (Show,Eq,Ord)
newtype LocDisp32 = LocDisp32 Location deriving (Show,Eq,Ord)

newtype InsnSize
  = InsnSize Word8
  deriving (Show,Eq,Ord)

-- | Displacement location (optional)
data LocDispMaybe
  = NoLocDisp
  | SomeLocDisp8     !LocDisp8
  | SomeLocDisp16    !LocDisp16
  | SomeLocDisp32    !LocDisp32
  | SomeLocRelDisp32 !InsnSize !LocDisp32 -- ^ Insn size and location is (original disp - insn size)
  deriving (Show,Eq,Ord)

newtype Disp8  = Disp8  Word8  deriving (Show,Eq,Ord)
newtype Disp16 = Disp16 Word16 deriving (Show,Eq,Ord)
newtype Disp32 = Disp32 Word32 deriving (Show,Eq,Ord)

disp8 :: Output m => Disp8 -> m LocDisp8
disp8 (Disp8 v) = do
  loc <- LocDisp8 <$> getLoc
  putW8 v
  pure loc

disp16 :: Output m => Disp16 -> m LocDisp16
disp16 (Disp16 v) = do
  loc <- LocDisp16 <$> getLoc
  putW16 v
  pure loc

disp32 :: Output m => Disp32 -> m LocDisp32
disp32 (Disp32 v) = do
  loc <- LocDisp32 <$> getLoc
  putW32 v
  pure loc

-- | Displacement (optional)
data DispMaybe
  = NoDisp
  | SomeDisp8     !Disp8
  | SomeDisp16    !Disp16
  | SomeDisp32    !Disp32
  | SomeRelDisp32 !Disp32

-- | Put displacement (if any)
--
-- The first argument is the size of the rest of the instruction (typically
-- immediate bytes). This is used to compute PC-relative addresses.
dispMaybe :: Output m => Word8 -> DispMaybe -> m LocDispMaybe
dispMaybe following_bytes = \case
  NoDisp                   -> pure NoLocDisp
  SomeDisp8  d             -> SomeLocDisp8         <$> disp8 d
  SomeDisp16 d             -> SomeLocDisp16        <$> disp16 d
  SomeDisp32 d             -> SomeLocDisp32        <$> disp32 d
  SomeRelDisp32 (Disp32 d) -> do
    -- take into account the following bytes and the 4 bytes we dump
    isz <- getInsnSize
    let sz = isz + fromIntegral following_bytes + 4
    let insn_size = InsnSize sz
    SomeLocRelDisp32 insn_size <$> disp32 (Disp32 (d-fromIntegral sz))

-- | Enable 64-bit operand size (REX.W)
rexW :: Output m => m ()
rexW = putW8 0b01001000

data REX_B
  = REX_B_0
  | REX_B_1
  deriving (Show,Eq,Ord)

data REX_R
  = REX_R_0
  | REX_R_1
  deriving (Show,Eq,Ord)

data REX_W
  = REX_W_0
  | REX_W_1
  deriving (Show,Eq,Ord)

data REX_X
  = REX_X_0
  | REX_X_1
  deriving (Show,Eq,Ord)

pattern W0 :: Field REX_W
pattern W0 = SetTo REX_W_0

pattern W1 :: Field REX_W
pattern W1 = SetTo REX_W_1

pattern W :: Field REX_W
pattern W = Default

pattern R0 :: Field REX_R
pattern R0 = SetTo REX_R_0

pattern R1 :: Field REX_R
pattern R1 = SetTo REX_R_1

pattern R :: Field REX_R
pattern R = Default

pattern X0 :: Field REX_X
pattern X0 = SetTo REX_X_0

pattern X1 :: Field REX_X
pattern X1 = SetTo REX_X_1

pattern X :: Field REX_X
pattern X = Default

pattern B0 :: Field REX_B
pattern B0 = SetTo REX_B_0

pattern B1 :: Field REX_B
pattern B1 = SetTo REX_B_1

pattern B :: Field REX_B
pattern B = Default

-- | Put REX prefix if needed
--
-- Throw an exception if constraints can't be fulfilled, i.e. some field
-- requires the REX prefix to be absent and another requires it to be present.
rex :: Output m => Field REX_W -> Field REX_R -> Field REX_X -> Field REX_B -> m ()
{-# INLINE rex #-} -- inline as it often simplifies
rex w r x b = case wrxb of
    Absent  -> pure ()
    Default -> pure ()
    SetTo v -> putW8 (0b01000000 .|. v)
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

oc :: Output m => Word8 -> m ()
oc = putW8

i8 :: Output m => Word8 -> m LocImm8
i8 v = do
  loc <- LocImm8 <$> getLoc
  putW8 v
  pure loc

i16 :: Output m => Word16 -> m LocImm16
i16 v = do
  loc <- LocImm16 <$> getLoc
  putW16 v
  pure loc

i32 :: Output m => Word32 -> m LocImm32
i32 v = do
  loc <- LocImm32 <$> getLoc
  putW32 v
  pure loc

i8sx :: Output m => Word8 -> m LocImm8sx
i8sx v = do
  loc <- LocImm8sx <$> getLoc
  putW8 v
  pure loc

i32sx :: Output m => Word32 -> m LocImm32sx
i32sx v = do
  loc <- LocImm32sx <$> getLoc
  putW32 v
  pure loc

lockMaybe :: Output m => Lock -> m ()
lockMaybe = \case
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
newtype FieldRM_base = FieldRM_base Word3

-- | Field contents for an index register
newtype Field_index = Field_index Word3

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

putModRM :: Output m => ModRM -> m ()
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
modRM_OpcodeReg :: FieldReg_opcode -> FieldRM_reg -> ModRM
modRM_OpcodeReg opcode reg = ModRM Mod11 (unFieldReg_opcode opcode) (unFieldRM_reg reg)

-- | Put opcode extension and register as arguments
--
-- Opcode extension and register are stored in ModRM.{reg,rm} respectively.
ocxReg :: Output m => Word3 -> FieldRM_reg -> m ()
ocxReg ext reg = putModRM (modRM_OpcodeReg (FieldReg_opcode ext) reg)

-- | Put opcode extension and address as arguments
--
-- Opcode extension and address are stored in ModRM.{reg,rm} respectively.
ocxMem :: Output m => Word3 -> MemMod -> FieldRM_mem -> m ()
ocxMem ext md rm = putModRM (modRM_OpcodeMem (FieldReg_opcode ext) md rm)

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

pattern RAX :: RegCode
pattern RAX = RawRegCode 0x00

pattern RCX :: RegCode
pattern RCX = RawRegCode 0x01

pattern RDX :: RegCode
pattern RDX = RawRegCode 0x02

pattern RBX :: RegCode
pattern RBX = RawRegCode 0x03

extractRegCode :: RegCode -> (RegBits, RegNum)
extractRegCode (RawRegCode w) = (toRegBits w, toRegNum w)

toRegCode :: RegBits -> RegNum -> RegCode
toRegCode b n = RawRegCode (fromRegBits b .|. fromRegNum n)

data Field a
  = Absent  -- ^ Need the field (e.g. REX.B) to be absent (i.e. no REX prefix at all)
  | Default -- ^ The field may be present or not (we use its value by default)
  | SetTo a -- ^ Need the field (e.g. REX.B) to be present and set to the given value
  deriving (Eq,Ord,Functor)

-- | Indicate if a register encoding requires an additional bit, and its value.
regEncoding :: RegCode -> (Field Bool, Word3)
regEncoding (RegCode bits num) =
  let n = fromRegNum num
  in case bits of
      Only3Bits -> (Absent             , Word3 n)
      Only4Bits -> (SetTo (testBit n 3), Word3 (n .&. 0b0111))
      AnyBits   -> (Default            , Word3 n)

-- | Encode a register in RM field of ModRM
regRM :: RegCode -> (Field REX_B, FieldRM_reg)
regRM c = (fmap toREX_B mextra, FieldRM_reg n)
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

-- | Encode an index register
encodeField_index :: RegCode -> (Field REX_X, Field_index)
encodeField_index c = (fmap toREX_X mextra, Field_index n)
  where
    (mextra,n) = regEncoding c
    toREX_X = \case
      False -> REX_X_0
      True  -> REX_X_1

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
revRegs :: ReverseBit -> Word8 -> RegCode -> RegCode -> (Field REX_R, Field REX_B, ModRM, Word8)
revRegs rev opcode dst src = (rex_r, rex_b, modrm, opcode')
  where
    modrm        = modRM_RegReg reg rm
    (rrm,rreg)   = case rev of
                      ReverseBit0 -> (dst,src)
                      ReverseBit1 -> (src,dst)
    (rex_b, rm)  = regRM rrm
    (rex_r, reg) = encodeFieldReg_reg rreg
    opcode'      = case rev of
                      ReverseBit0 -> opcode
                      ReverseBit1 -> setBit opcode 1

data Segment = CS | DS | ES | FS | GS | SS

segMaybe :: Output m => Maybe Segment -> m ()
segMaybe = \case
  Nothing -> pure ()
  Just s  -> seg s

seg :: Output m => Segment -> m ()
seg = \case
  CS -> putW8 0x2E
  DS -> putW8 0x3E
  ES -> putW8 0x26
  FS -> putW8 0x64
  GS -> putW8 0x65
  SS -> putW8 0x36

data AddrSize
  = Addr64 -- ^ 64-bit address size (default)
  | Addr32 -- ^ 32-bit address size

addrSizeMaybe :: Output m => Maybe AddrSize -> m ()
addrSizeMaybe = \case
  Nothing -> pure ()
  Just s  -> addrSize s

addrSize :: Output m => AddrSize -> m ()
addrSize = \case
  Addr64 -> pure ()
  Addr32 -> putW8 0x67

data Addr = Addr
  { addr_ea      :: !Addr64           -- ^ Effective address
  , addr_segment :: !(Maybe Segment)  -- ^ Segment override
  , addr_size    :: !(Maybe AddrSize) -- ^ Address-size override
  }

data Addr64
  = RIPRelative !Disp32       -- ^ RIP + disp32
  | BaseScaledIndexDisp !BSID -- ^ base + scaled index + disp

data BSID = BSID
  { bsid_base  :: !(Maybe RegCode) -- ^ Base register
  , bsid_index :: !(Maybe RegCode) -- ^ Index register
  , bsid_scale :: !Scale           -- ^ Index scale
  , bsid_disp  :: !(Maybe EADisp)  -- ^ Optional displacement
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

addrFields
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
addrFields (Addr ea mseg msize)
  = (mseg, msize, m_mod, m_rm, msib, disp, rex_x, rex_b)
  where
    -- we don't remove redundant segment override here, the responsibility is
    -- left to the caller to canonicalize the address.
    (m_mod, m_rm, msib, disp, rex_x, rex_b) = computeAddr64Fields ea

computeAddr64Fields
  :: Addr64
  -> ( MemMod
     , FieldRM_mem
     , Maybe SIB
     , DispMaybe
     , Field REX_X
     , Field REX_B
     )
computeAddr64Fields = \case
  BaseScaledIndexDisp bsid -> computeBSIDFields bsid

  -- RIP+disp32 addressing
  -- (does it support segment override?)
  RIPRelative d32
    -> ( MemMod00
       , FieldRM_mem 0b101
       , Nothing
       , SomeRelDisp32 d32
       , Default
       , Default
       )


computeBSIDFields
  :: BSID
  -> ( MemMod
     , FieldRM_mem
     , Maybe SIB
     , DispMaybe
     , Field REX_X
     , Field REX_B
     )
computeBSIDFields = \case
  -- encode empty address as disp8 of 0
  BSID Nothing Nothing scale Nothing
    -> computeBSIDFields (BSID Nothing Nothing scale (Just (EADisp8 (Disp8 0))))

  -- Encode disp only (using SIB)
  --
  -- Disp32 only without SIB isn't available in 64-bit mode: it has been
  -- replaced with RIP+disp32.
  -- Disp8 only without has never been available.
  BSID Nothing Nothing scale (Just disp)
    -> ( case disp of
          EADisp8 _  -> MemMod01
          EADisp32 _ -> MemMod10
       , FieldRM_mem 0b100
       , Just (SIB scale 0b100 0b101)
       , case disp of
          EADisp32 d -> SomeDisp32 d
          EADisp8  d -> SomeDisp8 d
       , Default
       , Default
       )

  -- using RSP as an index register isn't possible.
  BSID base (Just index) scale disp
    | (rex_x, Field_index rm) <- encodeField_index index
    , rm == 0b100            -- RSP or R12
    , rex_x /= SetTo REX_X_1 -- definitely RSP
    -> case (base,scale) of
        -- try to transform it into a base register if
        --  scale=1
        --  no base register already in use
        (Nothing,Scale1) -> computeBSIDFields (BSID (Just index) Nothing Scale1 disp)

        -- otherwise fail
        _ -> error "RSP can't be used as an index register"

  -- base register lower bits are 0b101 (rBP, r13) without disp
  -- Need to be encoded with disp=0.
  --
  -- rm = 0b101 is used to encoded RIP+disp32
  BSID b@(Just base) index scale Nothing
    | (_rex_b, FieldRM_base rm) <- encodeFieldRM_base base
    , rm == 0b101
    -> computeBSIDFields (BSID b index scale (Just (EADisp8 (Disp8 0))))

  -- base register lower bits are 0b100 (rSP, r12), requires SIB byte
  BSID (Just base) mindex scale mdisp
    | (rex_b, FieldRM_base ase) <- encodeFieldRM_base base
    , ase == 0b100
    ->
      let (rex_x, idx) = case mindex of
            Nothing -> (Default, 0b100)
            Just i  -> case encodeField_index i of
              -- the case where the index is RSP is handled above.
              -- We don't have to handle it here.
              (x, Field_index ix) -> (x,ix)
      in
       ( case mdisp of
          Nothing           -> MemMod00
          Just (EADisp8 _)  -> MemMod01
          Just (EADisp32 _) -> MemMod10
       , FieldRM_mem 0b100
       , Just (SIB scale idx ase)
       , case mdisp of
          Nothing           -> NoDisp
          Just (EADisp32 d) -> SomeDisp32 d
          Just (EADisp8  d) -> SomeDisp8 d
       , rex_x
       , rex_b
       )

  -- base register lower bits different from 0b100 and 0b101 (handled above)
  BSID (Just base) Nothing _scale mdisp
    | (rex_b, FieldRM_base rm) <- encodeFieldRM_base base
    , (mode,disp) <- case mdisp of
        Nothing           -> (MemMod00, NoDisp)
        Just (EADisp8  d) -> (MemMod01, SomeDisp8 d)
        Just (EADisp32 d) -> (MemMod10, SomeDisp32 d)
    -> (mode, FieldRM_mem rm, Nothing, disp, Default, rex_b)

  -- no base register: force the encoding of an empty Disp32 if needed
  BSID Nothing (Just index) scale mdisp
    ->
      let (rex_x, Field_index idx) = encodeField_index index
            -- the case where the index is RSP is handled above.
            -- We don't have to handle it here.
      in
       ( MemMod00
       , FieldRM_mem 0b100
       , Just (SIB scale idx 0b101)
       , case mdisp of
          Nothing                   -> SomeDisp32 (Disp32 0)
          Just (EADisp32 d)         -> SomeDisp32 d
          Just (EADisp8  (Disp8 d)) -> SomeDisp32 (Disp32 (fromIntegral d))
       , rex_x
       , Default
       )

  -- base and index
  BSID (Just base) (Just index) scale mdisp
    ->
      let (rex_x, Field_index idx) = encodeField_index index
            -- the case where the index is RSP is handled above.
            -- We don't have to handle it here.
          (rex_b, FieldRM_base ase) = encodeFieldRM_base base
            -- the case where the base is RBP is handled above.
            -- We don't have to handle it here.
      in
       ( case mdisp of
          Nothing           -> MemMod00
          Just (EADisp8 _)  -> MemMod01
          Just (EADisp32 _) -> MemMod10
       , FieldRM_mem 0b100
       , Just (SIB scale idx ase)
       , case mdisp of
          Nothing           -> NoDisp
          Just (EADisp32 d) -> SomeDisp32 d
          Just (EADisp8  d) -> SomeDisp8 d
       , rex_x
       , rex_b
       )


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

sibMaybe :: Output m => Maybe SIB -> m ()
sibMaybe = \case
  Nothing -> pure ()
  Just v  -> sib v

sib :: Output m => SIB -> m ()
sib (RawSIB w) = putW8 w

gen_r8_i8 :: Output m => Word8 -> Word3 -> RegCode -> Word8 -> m LocImm8
gen_r8_i8 opc opx (regRM -> (b,r)) v = do
  beginInsn
  rex W R X b
  oc opc
  ocxReg opx r
  i8 v

gen_r16_i16 :: Output m => Word8 -> Word3 -> DefaultOperandSize -> RegCode -> Word16 -> m LocImm16
gen_r16_i16 opc opx dos (regRM -> (b,r)) v = do
  beginInsn
  os16 dos
  rex W R X b
  oc opc
  ocxReg opx r
  i16 v

gen_r32_i32 :: Output m => Word8 -> Word3 -> DefaultOperandSize -> RegCode -> Word32 -> m LocImm32
gen_r32_i32 opc opx dos (regRM -> (b,r)) v = do
  beginInsn
  os32 dos
  rex W R X b
  oc opc
  ocxReg opx r
  i32 v

gen_r64_i32sx :: Output m => Word8 -> Word3 -> RegCode -> Word32 -> m LocImm32sx
gen_r64_i32sx opc opx (regRM -> (b,r)) v = do
  beginInsn
  rex W1 R X b
  oc opc
  ocxReg opx r
  i32sx v

gen_r16_i8sx :: Output m => Word8 -> Word3 -> DefaultOperandSize -> RegCode -> Word8 -> m LocImm8sx
gen_r16_i8sx opc opx dos (regRM -> (b,r)) v = do
  beginInsn
  os16 dos
  rex W R X b
  oc opc
  ocxReg opx r
  i8sx v

gen_r32_i8sx :: Output m => Word8 -> Word3 -> DefaultOperandSize -> RegCode -> Word8 -> m LocImm8sx
gen_r32_i8sx opc opx dos (regRM -> (b,r)) v = do
  beginInsn
  os32 dos
  rex W R X b
  oc opc
  ocxReg opx r
  i8sx v

gen_r64_i8sx :: Output m => Word8 -> Word3 -> RegCode -> Word8 -> m LocImm8sx
gen_r64_i8sx opc opx (regRM -> (b,r)) v = do
  beginInsn
  rex W1 R X b
  oc opc
  ocxReg opx r
  i8sx v

gen_r8_r8_rev :: Output m => Word8 -> RegCode -> RegCode -> ReverseBit -> m ()
gen_r8_r8_rev opc dst src rev = do
  beginInsn
  let (r, b, modrm, o) = revRegs rev opc dst src
  rex W r X b
  oc o
  putModRM modrm

gen_r16_r16_rev :: Output m => Word8 -> DefaultOperandSize -> RegCode -> RegCode -> ReverseBit -> m ()
gen_r16_r16_rev opc dos dst src rev = do
  beginInsn
  let (r, b, modrm, o) = revRegs rev opc dst src
  os16 dos
  rex W r X b
  oc o
  putModRM modrm

gen_r32_r32_rev :: Output m => Word8 -> DefaultOperandSize -> RegCode -> RegCode -> ReverseBit -> m ()
gen_r32_r32_rev opc dos dst src rev = do
  beginInsn
  let (r, b, modrm, o) = revRegs rev opc dst src
  os32 dos
  rex W r X b
  oc o
  putModRM modrm

gen_r64_r64_rev :: Output m => Word8 -> RegCode -> RegCode -> ReverseBit -> m ()
gen_r64_r64_rev opc dst src rev = do
  beginInsn
  let (r, b, modrm, o) = revRegs rev opc dst src
  rex W1 r X b
  oc o
  putModRM modrm

gen_m8_i8 :: Output m => Word8 -> Word3 -> Lock -> Addr -> Word8 -> m (LocDispMaybe, LocImm8)
gen_m8_i8 opc opx lock m v = do
  beginInsn
  let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
  lockMaybe lock
  segMaybe mseg
  addrSizeMaybe masize
  rex W R x b
  oc opc
  ocxMem opx m_mod m_rm
  sibMaybe msib
  loc_disp <- dispMaybe 1 disp
  loc_imm <- i8 v
  pure (loc_disp, loc_imm)

gen_m16_i16 :: Output m => Word8 -> Word3 -> DefaultOperandSize -> Lock -> Addr -> Word16 -> m (LocDispMaybe, LocImm16)
gen_m16_i16 opc opx dos lock m v = do
  beginInsn
  let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
  lockMaybe lock
  segMaybe mseg
  addrSizeMaybe masize
  os16 dos
  rex W R x b
  oc opc
  ocxMem opx m_mod m_rm
  sibMaybe msib
  loc_disp <- dispMaybe 2 disp
  loc_imm <- i16 v
  pure (loc_disp, loc_imm)

gen_m32_i32 :: Output m => Word8 -> Word3 -> DefaultOperandSize -> Lock -> Addr -> Word32 -> m (LocDispMaybe, LocImm32)
gen_m32_i32 opc opx dos lock m v = do
  beginInsn
  let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
  lockMaybe lock
  segMaybe mseg
  addrSizeMaybe masize
  os32 dos
  rex W R x b
  oc opc
  ocxMem opx m_mod m_rm
  sibMaybe msib
  loc_disp <- dispMaybe 4 disp
  loc_imm <- i32 v
  pure (loc_disp, loc_imm)

gen_m64_i32sx :: Output m => Word8 -> Word3 -> Lock -> Addr -> Word32 -> m (LocDispMaybe, LocImm32sx)
gen_m64_i32sx opc opx lock m v = do
  beginInsn
  let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
  lockMaybe lock
  segMaybe mseg
  addrSizeMaybe masize
  rex W1 R x b
  oc opc
  ocxMem opx m_mod m_rm
  sibMaybe msib
  loc_disp <- dispMaybe 4 disp
  loc_imm <- i32sx v
  pure (loc_disp, loc_imm)
