{-# LANGUAGE LambdaCase #-}

-- | X86 (and X87) instructions
--
-- TODO: add flag for control-flow instructions (branches, return, call, etc.)
--
-- FIXME: X87 instructions don't encode precisely the stack popping (e.g., it is
-- not enough to say that ST(1) is accessed in Read/Write mode, we need to
-- encode that ST(n+1) becomes ST(n) for all n)
--
-- TODO: X87 instructions: FPU status flags (C0,C1,C2,C3) are not indicated yet
--
-- FIXME: MemAlign property needs to be checked (added only since vhaddpd)
--
-- TODO: add potential exceptions
-- TODO: add required privilege 
--
-- TODO: add pseudo-code for each op
module ViperVM.Arch.X86_64.Assembler.Insns
   ( X86Insn(..)
   , X86Arch(..)
   , X86Extension(..)
   , Properties(..)
   , FlagOp(..)
   , Encoding(..)
   , VexLW (..)
   , EncodingProperties(..)
   , OperandSpec(..)
   , OperandEnc(..)
   , OpcodeMap(..)
   , LegacyMap(..)
   , AccessMode(..)
   , EncodingVariant(..)
   , instructions
   -- * Helper methods
   , hasImmediate
   , isImmediate
   , isLegacyEncoding
   , isVexEncoding
   , encOpcode
   , encOpcodeExt
   , encOpcodeFullExt
   , encOpcodeMap
   , encOperands
   , encMandatoryPrefix
   , encProperties
   , encParams
   , encNoForce8Bit
   , encSignExtendImmBit
   , encReversableBit
   , encFPUSizableBit
   , encFPUDestBit
   , encFPUPopBit
   , encLockable
   , encRequireModRM
   , amd3DNowEncoding
   )
where

import Data.Word
import Data.List ((\\))
import Data.Maybe

import ViperVM.Arch.X86_64.MicroArch
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Opcode
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Registers

-- | X86 instruction
data X86Insn = X86Insn
   { insnDesc        :: String
   , insnMnemonic    :: String
   , insnProperties  :: [Properties]
   , insnFlags       :: [FlagOp Flag]
   , insnEncodings   :: [Encoding]
   } deriving (Show)

-- | Flag state modification
data FlagOp a
   = St        [a]  -- ^ Set flag to 1
   | Unset     [a]  -- ^ Set flag to 0
   | Modified  [a]  -- ^ Set flag depending on the result
   | Undefined [a]  -- ^ Flag is undefined after the operation
   | Read      [a]  -- ^ Flag read by the instruction
   deriving (Show,Eq)

-- | Instruction encoding
data Encoding
   = LegacyEncoding
      { legacyMandatoryPrefix :: Maybe Word8          -- ^ Mandatory prefix
      , legacyOpcodeMap       :: LegacyMap            -- ^ Map
      , legacyOpcode          :: Word8                -- ^ Opcode
      , legacyOpcodeExt       :: Maybe Word8          -- ^ Opcode extension in ModRM.reg
      , legacyOpcodeFullExt   :: Maybe Word8          -- ^ Opcode extension in full ModRM byte
      , legacyReversable      :: Maybe Int            -- ^ Args are reversed if the given bit is
                                                      --   set in the opcode.
      , legacyNoForce8bit     :: Maybe Int            -- ^ Operand size is 8 if the given bit is
                                                      --   unset in the opcode. Otherwise, the
                                                      --   size is defined by operand-size
                                                      --   prefix and REX.W bit
      , legacySignExtendable  :: Maybe Int            -- ^ Used in conjunction with a set
                                                      --   Sizable bit.  Imm8 operand is used
                                                      --   and sign-extended if the given bit is
                                                      --   set
      , legacyFPUDest         :: Maybe Int            -- ^ Opcode bit: register destination (0 if ST0, 1 if ST(i))
                                                      --   only if both operands are registers!
      , legacyFPUPop          :: Maybe Int            -- ^ Opcode bit: pop the FPU register,
                                                      --   only if destination is (ST(i))
      , legacyFPUSizable      :: Maybe Int            -- ^ Opcode bit: change the FPU size (only if memory operand)
      , legacyProperties      :: [EncodingProperties] -- ^ Encoding properties
      , legacyParams          :: [OperandSpec]        -- ^ Operand encoding
      }
   | VexEncoding
      { vexMandatoryPrefix :: Maybe Word8          -- ^ Mandatory prefix
      , vexOpcodeMap       :: OpcodeMap            -- ^ Map
      , vexOpcode          :: Word8                -- ^ Opcode
      , vexOpcodeExt       :: Maybe Word8          -- ^ Opcode extension in ModRM.reg
      , vexReversable      :: Maybe Int            -- ^ Args are reversed if the given bit is
                                                   --   set in the opcode.
      , vexLW              :: VexLW
      , vexProperties      :: [EncodingProperties] -- ^ Encoding properties
      , vexParams          :: [OperandSpec]        -- ^ Operand encoding
      }
   deriving (Show)

-- | VEX.(L/W) spec
data VexLW
   = W0     -- ^ Vex.W set to 0
   | W1     -- ^ Vex.W set to 1
   | WIG    -- ^ Vex.W ignored
   | L0_WIG -- ^ Vex.L set to 0, ignore Vex.W
   | L1_WIG -- ^ Vex.L set to 1, ignore Vex.W
   | L0     -- ^ Vex.L set to 0
   | LIG    -- ^ Vex.L ignored
   | LWIG   -- ^ Ignore Vex.W and Vex.L
   deriving (Show)

-- | Instruction properties
data Properties
   = FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   | MemAlign Int             -- ^ Memory alignment constraint in bytes
   | MemAlignDefault          -- ^ Memory alignment constraint
   deriving (Show,Eq)

-- | Encoding properties
data EncodingProperties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | Repeatable               -- ^ Allow repeat prefix
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | NoOperandSize64          -- ^ 64-bit operand size not supported

   | DefaultAddressSize64     -- ^ Default address size is 64-bits for this
                              --   instruction in LongMode
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | RequireRexW              -- ^ Require REX.W
   | DefaultSegment Register  -- ^ Default register
   deriving (Show,Eq)

-- | Instruction variant encoding
data EncodingVariant
   = Locked        -- ^ Locked memory access
   | Reversed      -- ^ Parameters are reversed (useful when some instructions have two valid encodings, e.g. CMP reg8, reg8)
   | ExplicitParam -- ^ A variant exists with an implicit parameter, but the explicit variant is used
   deriving (Show,Eq)


-------------------------------------------------------------------
-- Helper methods
-------------------------------------------------------------------

-- | Instruction
insn :: X86Insn
insn = X86Insn
   { insnDesc        = ""
   , insnMnemonic    = ""
   , insnProperties  = []
   , insnFlags       = []
   , insnEncodings   = []
   }

-- | Flags
allFlags :: [Flag]
allFlags = [CF,PF,AF,ZF,SF,TF,OF]

-- | Legacy encoding
leg :: Encoding
leg = LegacyEncoding
   { legacyMandatoryPrefix = Nothing
   , legacyOpcodeMap       = MapPrimary
   , legacyOpcode          = 0
   , legacyOpcodeExt       = Nothing
   , legacyOpcodeFullExt   = Nothing
   , legacyReversable      = Nothing
   , legacyNoForce8bit     = Nothing
   , legacySignExtendable  = Nothing
   , legacyFPUDest         = Nothing
   , legacyFPUPop          = Nothing
   , legacyFPUSizable      = Nothing
   , legacyProperties      = []
   , legacyParams          = []
   }

-- | Vex encoding
vex :: Encoding
vex = VexEncoding
   { vexMandatoryPrefix = Nothing
   , vexOpcodeMap       = MapVex 0
   , vexOpcode          = 0
   , vexOpcodeExt       = Nothing
   , vexReversable      = Nothing
   , vexLW              = LWIG
   , vexProperties      = []
   , vexParams          = []
   }

-- | Operand
op :: AccessMode -> OperandType -> OperandEnc -> OperandSpec
op = OperandSpec


-- | Immediate helpers. Immediate operands are always read-only and encoded in
-- the same way.
imm :: ImmType -> OperandSpec
imm s = op RO (T_Imm s) Imm

-- | 8-bit immediate operand
imm8 :: OperandSpec
imm8 = imm ImmSize8

-- | 16-bit immediate operand
imm16 :: OperandSpec
imm16 = imm ImmSize16

-- | operand-sized immediate operand
immOp :: OperandSpec
immOp = imm ImmSizeOp

-- | operand-sized immediate operand, sign-extended 32-bit for 64-bit
immSE :: OperandSpec
immSE = imm ImmSizeSE

-- | 256-bit memory
mem256 :: AccessMode -> OperandSpec
mem256 m = op m (T_Mem Mem256) RM

-- | 128-bit memory
mem128 :: AccessMode -> OperandSpec
mem128 m = op m (T_Mem Mem128) RM

-- | 64-bit memory
mem64 :: AccessMode -> OperandSpec
mem64 m = op m (T_Mem Mem64) RM

-- | 64-bit vector or memory
mvec64 :: AccessMode -> OperandSpec
mvec64 m = op m (TME (T_Reg RegVec64) (T_Mem Mem64)) RM

-- | 256-bit vector or memory
mvec256 :: AccessMode -> OperandSpec
mvec256 m = op m (TME (T_Reg RegVec256) (T_Mem Mem256)) RM

-- | 128-bit vector or memory
mvec128 :: AccessMode -> OperandSpec
mvec128 m = op m (TME (T_Reg RegVec128) (T_Mem Mem128)) RM

-- | 128-bit or 256-bit vector or memory
mvec128o256 :: AccessMode -> OperandSpec
mvec128o256 m = op m (TLE
   (TME (T_Reg RegVec128) (T_Mem Mem128))
   (TME (T_Reg RegVec256) (T_Mem Mem256)))
   RM

-- | low 64-bit of 128-bit vector or 64-bit memory
mvec128low64 :: AccessMode -> OperandSpec
mvec128low64 m = op m (TME (T_SubReg SubLow64 RegVec128) (T_Mem Mem64)) RM

-- | low 32-bit of 128-bit vector or 32-bit memory
mvec128low32 :: AccessMode -> OperandSpec
mvec128low32 m = op m (TME (T_SubReg SubLow32 RegVec128) (T_Mem Mem32)) RM

-- | 64-bit even positioned values in vector register or memory
--    * low 64-bit of 128-bit vector
--    * [63,0] and [191,128] of 256-bit vector
mvecEven64 :: AccessMode -> OperandSpec
mvecEven64 m = op m (TLE
   (TME (T_SubReg SubLow64 RegVec128) (T_Mem Mem64))
   (TME (T_SubReg SubEven64 RegVec256) (T_Mem Mem256))
   ) RM

-- | low bytes of a vector or memory
mveclow :: AccessMode -> OperandSpec
mveclow m = op m (TLE 
   (TME (T_SubReg SubLow64 RegVec128) (T_Mem Mem64))
   (TME (T_Reg RegVec128) (T_Mem Mem128))) RM

-- | 128-bit or 256-bit memory depending on Vex.L
m128o256 :: AccessMode -> OperandSpec
m128o256 m = op m (TLE (T_Mem Mem128) (T_Mem Mem256)) RM

-- | 256-bit vector
vec256 :: AccessMode -> OperandEnc -> OperandSpec
vec256 m e = op m (T_Reg RegVec256) e

-- | 128-bit vector
vec128 :: AccessMode -> OperandEnc -> OperandSpec
vec128 m e = op m (T_Reg RegVec128) e

-- | 64-bit vector
vec64 :: AccessMode -> OperandEnc -> OperandSpec
vec64 m e = op m (T_Reg RegVec64) e

-- | Low 64-bit of 128-bit vector
vec128low64 :: AccessMode -> OperandEnc -> OperandSpec
vec128low64 m e = op m (T_SubReg SubLow64 RegVec128) e

-- | High 64-bit of 128-bit vector
vec128high64 :: AccessMode -> OperandEnc -> OperandSpec
vec128high64 m e = op m (T_SubReg SubHigh64 RegVec128) e

-- | Low 32-bit of 128-bit vector
vec128low32 :: AccessMode -> OperandEnc -> OperandSpec
vec128low32 m e = op m (T_SubReg SubLow32 RegVec128) e

-- | 128-bit or 256-bit vector depending on Vex.L
vec128o256 :: AccessMode -> OperandEnc -> OperandSpec
vec128o256 m e = op m (TLE (T_Reg RegVec128) (T_Reg RegVec256)) e

-- | 32-bit or 64-bit general purpose register (depending on Rex.W)
reg32o64 :: AccessMode -> OperandEnc -> OperandSpec
reg32o64 m e = op m (TWE (T_Reg Reg32) (T_Reg Reg64)) e

-- | 32-bit or 64-bit general purpose register (depending on Rex.W) or memory
rm32o64 :: AccessMode -> OperandSpec
rm32o64 m = op m (TME
   (TWE (T_Reg Reg32) (T_Reg Reg64))
   (TWE (T_Mem Mem32) (T_Mem Mem64)))
   RM

-- | 16-bit general purpose register
reg16 :: AccessMode -> OperandEnc -> OperandSpec
reg16 m e = op m (T_Reg Reg16) e

-- | 16-bit memory
mem16 :: AccessMode -> OperandSpec
mem16 m = op m (T_Mem Mem16) RM

-- | 8-bit general purpose register or memory
rm8 :: AccessMode -> OperandSpec
rm8 m = op m (TME (T_Reg Reg8) (T_Mem Mem8)) RM

-- | 16-bit general purpose register or memory
rm16 :: AccessMode -> OperandSpec
rm16 m = op m (TME (T_Reg Reg16) (T_Mem Mem16)) RM

-- | 32-bit memory
mem32 :: AccessMode -> OperandSpec
mem32 m = op m (T_Mem Mem32) RM

-- | 512-bit memory
mem512 :: AccessMode -> OperandSpec
mem512 m = op m (T_Mem Mem512) RM

-- | 32-bit general purpose register or memory
rm32 :: AccessMode -> OperandSpec
rm32 m = op m (TME (T_Reg Reg32) (T_Mem Mem32)) RM

-- | 64-bit general purpose register or memory
rm64 :: AccessMode -> OperandSpec
rm64 m = op m (TME (T_Reg Reg64) (T_Mem Mem64)) RM

-- | 128-bit or 256-bit memory (depending on Rex.W)
mem128o256 :: AccessMode -> OperandSpec
mem128o256 m = op m (TWE (T_Mem Mem128) (T_Mem Mem256)) RM

-- | 64-bit or 128-bit memory (depending on Rex.W)
mem64o128 :: AccessMode -> OperandSpec
mem64o128 m = op m (TWE (T_Mem Mem64) (T_Mem Mem128)) RM

-- | 32-bit or 64-bit memory (depending on Rex.W)
mem32o64 :: AccessMode -> OperandSpec
mem32o64 m = op m (TWE (T_Mem Mem32) (T_Mem Mem64)) RM

-- | General purpose register with the operand-size
gpr :: AccessMode -> OperandEnc -> OperandSpec
gpr m e = op m (T_Reg RegOpSize) e

-- | General purpose register with the operand-size or memory
mgpr :: AccessMode -> OperandSpec
mgpr m = op m (TME (T_Reg RegOpSize) (T_Mem MemOpSize)) RM

-- | Memory with the operand-size
mem :: AccessMode -> OperandSpec
mem m = op m (T_Mem MemOpSize) RM

-- | Any memory address (the pointed type doesn't matter)
mvoid :: OperandSpec
mvoid = op NA (T_Mem MemVoid) RM

-- | Fixed register
reg :: Register -> AccessMode -> OperandEnc -> OperandSpec
reg r m e = op m (T_Reg (RegFixed r)) e

-- | EDX or RDX
rDX :: AccessMode -> OperandSpec
rDX m = op m (TWE (T_Reg (RegFixed R_EDX)) (T_Reg (RegFixed R_RDX))) Implicit

-- | EDX:EAX or RDX:RAX pair
rDXrAX :: AccessMode -> OperandSpec
rDXrAX m = op m (TWE
   (T_Pair (T_Reg (RegFixed R_EDX)) (T_Reg (RegFixed R_EAX)))
   (T_Pair (T_Reg (RegFixed R_RDX)) (T_Reg (RegFixed R_RAX))))
   Implicit

-- | ECX:EBX or RCX:RBX pair
rCXrBX :: AccessMode -> OperandSpec
rCXrBX m = op m (TWE
   (T_Pair (T_Reg (RegFixed R_ECX)) (T_Reg (RegFixed R_EBX)))
   (T_Pair (T_Reg (RegFixed R_RCX)) (T_Reg (RegFixed R_RBX))))
   Implicit

-- | 8-bit relative offset
rel8 :: OperandSpec
rel8 = op RO (T_Rel Rel8) Imm

-- | 16-bit or 32-bit relative offset (16-bit invalid in 64-bit mode)
rel16o32 :: OperandSpec
rel16o32 = op RO (T_Rel Rel16o32) Imm

-- | Immediate pointer: 16:16 or 16:32
ptr16x :: OperandSpec
ptr16x = op RO (T_Pair (T_Imm ImmSize16) (T_Imm ImmSizeOp)) Imm

-- | Implicit immediate constant
constImm :: Int -> OperandSpec
constImm x = op RO (T_Imm (ImmConst x)) Implicit

-- | Address of a pointer
m16x :: OperandSpec
m16x = op RO (T_Mem MemPtr) RM

-- | x87 register (there are all in RM)
st :: AccessMode -> OperandSpec
st m = op m (T_Reg RegST) RM

-- | real memory or x87 register
mst :: AccessMode -> OperandSpec
mst m = op m (TME (T_Reg RegST) (T_Mem MemFP)) RM

-- | x87 int memory
mint :: AccessMode -> OperandSpec
mint m = op m (T_Mem MemInt) RM

-- | x87 int64 memory
mint64 :: AccessMode -> OperandSpec
mint64 m = op m (T_Mem MemInt64) RM

-- | x87 m80real memory
mfp80 :: AccessMode -> OperandSpec
mfp80 m = op m (T_Mem MemFP80) RM

-- | x87 m80dec memory
mdec80 :: AccessMode -> OperandSpec
mdec80 m = op m (T_Mem MemDec80) RM

-- | x87 14/28 env memory
menv :: AccessMode -> OperandSpec
menv m = op m (T_Mem MemEnv) RM

-- | x87 14/28 state memory
mstate :: AccessMode -> OperandSpec
mstate m = op m (T_Mem MemState) RM

-- | descriptor table memory
mdt :: AccessMode -> OperandSpec
mdt m = op m (T_Mem MemDescTable) RM

-- | Counter register
regCounter :: AccessMode -> OperandSpec
regCounter m = op m (T_Reg RegCounter) Implicit

-- | Accumulator register
regAccu :: AccessMode -> OperandSpec
regAccu m = op m (T_Reg RegAccu) Implicit

-- | Stack pointer register
regStackPtr :: AccessMode -> OperandEnc -> OperandSpec
regStackPtr m e = op m (T_Reg RegStackPtr) e

-- | Base pointer register
regBasePtr :: AccessMode -> OperandEnc -> OperandSpec
regBasePtr m e = op m (T_Reg RegBasePtr) e

-- | Register family
regFam :: RegFamilies -> AccessMode -> OperandEnc -> OperandSpec
regFam x m e = op m (T_Reg (RegFam x)) e

-- | Memory at DS:rSI
mDSrSI :: AccessMode -> OperandSpec
mDSrSI m = op m (T_Mem MemDSrSI) Implicit

-- | Memory at ES:rDI
mESrDI :: AccessMode -> OperandSpec
mESrDI m = op m (T_Mem MemESrDI) Implicit

-- | Memory at DS:rDI (DS is overridable)
mDSrDI :: AccessMode -> OperandSpec
mDSrDI m = op m (T_Mem MemDSrDI) Implicit

-- | Mask
mask :: OperandEnc -> OperandSpec
mask = op RO T_Mask

-- We use a dummy encoding for 3DNow: because all the instructions use the same
amd3DNowEncoding :: Encoding
amd3DNowEncoding = leg
   { legacyOpcodeMap = Map3DNow
   , legacyParams    = [ vec64 RW Reg
                       , mvec64 RO
                       ]
   }

isImmediate :: OperandEnc -> Bool
isImmediate = \case
   Imm    -> True
   Imm8h  -> True
   Imm8l  -> True
   _      -> False

hasImmediate :: Encoding -> Bool
hasImmediate e = any (isImmediate . opEnc) (encOperands e)

isLegacyEncoding :: Encoding -> Bool
isLegacyEncoding LegacyEncoding {} = True
isLegacyEncoding _                 = False

isVexEncoding :: Encoding -> Bool
isVexEncoding VexEncoding {} = True
isVexEncoding _              = False

encOpcode :: Encoding -> Word8
encOpcode e@LegacyEncoding {} = legacyOpcode e
encOpcode e@VexEncoding    {} = vexOpcode e

encOpcodeExt :: Encoding -> Maybe Word8
encOpcodeExt e@LegacyEncoding {} = legacyOpcodeExt e
encOpcodeExt e@VexEncoding    {} = vexOpcodeExt e

encOpcodeFullExt :: Encoding -> Maybe Word8
encOpcodeFullExt e@LegacyEncoding {} = legacyOpcodeFullExt e
encOpcodeFullExt VexEncoding    {}   = Nothing

encOpcodeMap :: Encoding -> OpcodeMap
encOpcodeMap e@LegacyEncoding {} = MapLegacy (legacyOpcodeMap e)
encOpcodeMap e@VexEncoding    {} = vexOpcodeMap e

encOperands :: Encoding -> [OperandSpec]
encOperands e@LegacyEncoding {}  = legacyParams e
encOperands e@VexEncoding    {}  = vexParams e

encMandatoryPrefix :: Encoding -> Maybe Word8
encMandatoryPrefix e@LegacyEncoding {} = legacyMandatoryPrefix e
encMandatoryPrefix e@VexEncoding    {} = vexMandatoryPrefix e

encProperties :: Encoding -> [EncodingProperties]
encProperties e@LegacyEncoding {} = legacyProperties e
encProperties e@VexEncoding    {} = vexProperties e

encParams :: Encoding -> [OperandSpec]
encParams e@LegacyEncoding {} = legacyParams e
encParams e@VexEncoding    {} = vexParams e

encNoForce8Bit :: Encoding -> Maybe Int
encNoForce8Bit e@LegacyEncoding {} = legacyNoForce8bit e
encNoForce8Bit _                   = Nothing

encSignExtendImmBit :: Encoding -> Maybe Int
encSignExtendImmBit e@LegacyEncoding {} = legacySignExtendable e
encSignExtendImmBit _                   = Nothing

encReversableBit :: Encoding -> Maybe Int
encReversableBit e@LegacyEncoding {} = legacyReversable e
encReversableBit e@VexEncoding {}    = vexReversable e

encFPUSizableBit :: Encoding -> Maybe Int
encFPUSizableBit e@LegacyEncoding {} = legacyFPUSizable e
encFPUSizableBit _                   = Nothing

encFPUDestBit :: Encoding -> Maybe Int
encFPUDestBit e@LegacyEncoding {} = legacyFPUDest e
encFPUDestBit _                   = Nothing

encFPUPopBit :: Encoding -> Maybe Int
encFPUPopBit e@LegacyEncoding {} = legacyFPUPop e
encFPUPopBit _                   = Nothing

-- | Indicate if LOCK prefix is allowed
encLockable :: Encoding -> Bool
encLockable e = Lockable `elem` encProperties e

encRequireModRM :: Encoding -> Bool
encRequireModRM e = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg 
      hasOpExt = isJust (encOpcodeExt e) || isJust (encOpcodeFullExt e)

      -- has operands in ModRM
      hasOps   = any matchEnc (encOperands e)
      matchEnc x = case opEnc x of
         RM         -> True
         Reg        -> True
         Imm        -> False
         Imm8h      -> False
         Imm8l      -> False
         Implicit   -> False
         Vvvv       -> False
         OpcodeLow3 -> False

-------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------

instructions :: [X86Insn]
instructions =
   [ i_aaa
   , i_aad 
   , i_aam 
   , i_aas 
   , i_adc 
   , i_adcx 
   , i_add 
   , i_addpd 
   , i_vaddpd 
   , i_addps 
   , i_vaddps 
   , i_addsd 
   , i_vaddsd 
   , i_addss 
   , i_vaddss 
   , i_addsubpd 
   , i_vaddsubpd 
   , i_addsubps 
   , i_vaddsubps 
   , i_adox 
   , i_aesdec 
   , i_vaesdec 
   , i_aesdeclast 
   , i_vaesdeclast 
   , i_aesenc 
   , i_vaesenc 
   , i_aesenclast 
   , i_vaesenclast 
   , i_aesimc 
   , i_vaesimc 
   , i_aeskeygenassist 
   , i_vaeskeygenassist 
   , i_and 
   , i_andn 
   , i_andpd 
   , i_vandpd 
   , i_andps 
   , i_vandps 
   , i_andnpd 
   , i_vandnpd 
   , i_andnps 
   , i_vandnps 
   , i_arpl 
   , i_blendpd 
   , i_vblendpd 
   , i_bextr 
   , i_blendps 
   , i_vblendps 
   , i_blendvpd 
   , i_vblendvpd 
   , i_blendvps 
   , i_vblendvps 
   , i_blsi 
   , i_blsmsk 
   , i_blsr 
   , i_bound 
   , i_bsf 
   , i_bsr 
   , i_bswap 
   , i_bt 
   , i_btc 
   , i_btr 
   , i_bts 
   , i_bzhi 
   , i_call 
   , i_extend_signed 
   , i_clac
   , i_clc
   , i_cld
   , i_clflush
   , i_cli
   , i_clts
   , i_cmc
   , i_cmovo
   , i_cmovno
   , i_cmovc
   , i_cmovnc
   , i_cmovz
   , i_cmovnz
   , i_cmovbe
   , i_cmova
   , i_cmovs
   , i_cmovns
   , i_cmovp
   , i_cmovnp
   , i_cmovl
   , i_cmovge
   , i_cmovle
   , i_cmovg
   , i_cmp 
   , i_cmppd 
   , i_vcmppd 
   , i_cmpps 
   , i_vcmpps 
   , i_cmps 
   , i_cmpsd 
   , i_vcmpsd 
   , i_cmpss 
   , i_vcmpss 
   , i_cmpxchg 
   , i_cmpxch8b 
   , i_comisd 
   , i_vcomisd 
   , i_comiss 
   , i_vcomiss 
   , i_cpuid 
   , i_crc32 
   , i_cvtdq2pd 
   , i_vcvtdq2pd 
   , i_cvtdq2ps 
   , i_vcvtdq2ps 
   , i_cvtpd2dq 
   , i_vcvtpd2dq 
   , i_cvtpd2di 
   , i_cvtpd2ps 
   , i_vcvtpd2ps 
   , i_cvtpi2pd 
   , i_cvtpi2ps 
   , i_cvtps2dq 
   , i_vcvtps2dq 
   , i_cvtps2pd 
   , i_vcvtps2pd 
   , i_cvtps2pi 
   , i_cvtsd2si 
   , i_vcvtsd2si 
   , i_cvtsd2ss 
   , i_vcvtsd2ss 
   , i_cvtsi2sd 
   , i_vcvtsi2sd 
   , i_cvtsi2ss 
   , i_vcvtsi2ss 
   , i_cvtss2sd 
   , i_vcvtss2sd 
   , i_cvtss2si 
   , i_vcvtss2si 
   , i_cvttpd2dq 
   , i_vcvttpd2dq 
   , i_cvttpd2pi 
   , i_cvttps2dq 
   , i_vcvttps2dq 
   , i_cvttps2pi 
   , i_cvttsd2si 
   , i_vcvttsd2si 
   , i_cvttss2si 
   , i_vcvttss2si 
   , i_cwd 
   , i_daa 
   , i_das 
   , i_dec 
   , i_div 
   , i_divpd 
   , i_vdivpd 
   , i_divps 
   , i_vdivps 
   , i_divsd 
   , i_vdivsd 
   , i_divss 
   , i_vdivss 
   , i_dppd 
   , i_vdppd 
   , i_dpps 
   , i_vdpps 
   , i_emms 
   , i_enter 
   , i_extractps 
   , i_vextractps 
   , i_f2xm1
   , i_fabs
   , i_fadd
   , i_fiadd
   , i_fbld
   , i_fbstp
   , i_fchs
   , i_fnclex
   , i_fcmovb
   , i_fcmove
   , i_fcmovbe
   , i_fcmovu
   , i_fcmovnb
   , i_fcmovne
   , i_fcmovnbe
   , i_fcmovnu
   , i_fcom
   , i_fcomp
   , i_fcompp
   , i_fcomi
   , i_fucomi
   , i_fcos
   , i_fdecstp
   , i_fdiv
   , i_fidiv
   , i_fdivr
   , i_fidivr
   , i_ffree
   , i_ficom
   , i_ficomp
   , i_fild
   , i_fincstp
   , i_finit
   , i_fist
   , i_fistp
   , i_fisttp
   , i_fld
   , i_fld1
   , i_fldl2t
   , i_fldl2e
   , i_fldpi
   , i_fldlg2
   , i_fldln2
   , i_fldz
   , i_fldcw
   , i_fldenv
   , i_fmul
   , i_fimul
   , i_fnop
   , i_fpatan
   , i_fprem
   , i_fprem1
   , i_fptan
   , i_frndint
   , i_frstor
   , i_fnsave
   , i_fscale
   , i_fsin
   , i_fsincos
   , i_fsqrt
   , i_fst
   , i_fstp
   , i_fnstcw
   , i_fnstenv
   , i_fnstsw
   , i_fsub
   , i_fisub
   , i_fsubr
   , i_fisubr
   , i_ftst
   , i_fucom
   , i_fucomp
   , i_fucompp
   , i_fxam
   , i_fxch
   , i_fxrstor
   , i_fxrstor64
   , i_fxsave
   , i_fxsave64
   , i_fxtract
   , i_fyl2x
   , i_fyl2xp1
   , i_haddpd
   , i_vhaddpd
   , i_haddps
   , i_vhaddps
   , i_hlt
   , i_hsubpd
   , i_vhsubpd
   , i_hsubps
   , i_vhsubps
   , i_idiv
   , i_imul
   , i_in
   , i_inc
   , i_ins
   , i_insertps
   , i_vinsertps
   , i_int
   , i_into
   , i_invd
   , i_invlpg
   , i_invpcid
   , i_iret
   , i_ja
   , i_jae
   , i_jb
   , i_jbe
   , i_jcxz
   , i_je
   , i_jg
   , i_jge
   , i_jl
   , i_jle
   , i_jne
   , i_jno
   , i_jnp
   , i_jns
   , i_jo
   , i_jp
   , i_js
   , i_jmp
   , i_lahf
   , i_lar
   , i_lddqu
   , i_vlddqu
   , i_ldmxcsr
   , i_vldmxcsr
   , i_ldfarptr
   , i_lea
   , i_leave
   , i_lfence
   , i_lgdt
   , i_lidt
   , i_lldt
   , i_lmsw
   , i_lods
   , i_loop
   , i_loope
   , i_loopne
   , i_lsl
   , i_ltr
   , i_maskmovdqu
   , i_vmaskmovdqu
   , i_maskmovq
   , i_maxpd
   , i_vmaxpd
   , i_maxps
   , i_vmaxps
   , i_maxsd
   , i_vmaxsd
   , i_maxss
   , i_vmaxss
   , i_mfence
   , i_minpd
   , i_vminpd
   , i_minps
   , i_vminps
   , i_minsd
   , i_vminsd
   , i_minss
   , i_vminss
   , i_monitor
   , i_mov
   , i_movcr
   , i_movdr
   , i_movapd
   , i_vmovapd
   , i_movaps
   , i_vmovaps
   , i_movbe
   , i_movdq
   , i_movddup
   , i_movdqa
   , i_movdqu
   , i_movdq2q
   , i_movhlps
   , i_vmovhlps
   , i_movhpd
   , i_vmovhpd
   , i_movhps
   , i_vmovhps
   , i_movlhps
   , i_vmovlhps
   , i_movlpd
   , i_vmovlpd
   , i_movlps
   , i_vmovlps
   , i_movmskpd
   , i_vmovmskpd
   , i_movmskps
   , i_vmovmskps
   , i_movntdqa
   , i_movntdq
   , i_movnti
   , i_movntpd
   , i_movntps
   , i_movntq
   , i_movq
   , i_movq2dq
   , i_movs
   , i_movsd
   , i_vmovsd
   , i_movshdup
   , i_movsldup
   , i_movss
   , i_vmovss
   , i_movsx
   , i_movupd
   , i_vmovupd
   , i_movups
   , i_vmovups
   , i_movzx
   , i_mpsadbw
   , i_mul
   , i_mulpd
   , i_vmulpd
   , i_mulps
   , i_vmulps
   , i_mulsd
   , i_vmulsd
   , i_mulss
   , i_vmulss
   , i_mulx
   , i_mwait
   ]

i_aaa :: X86Insn
i_aaa = insn
   { insnDesc        = "ASCII adjust AL after addition"
   , insnMnemonic    = "AAA"
   , insnFlags       = [ Modified  [AF,CF]
                       , Undefined [OF,SF,ZF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x37
                           , legacyProperties      = [LegacyModeSupport]
                           , legacyParams          = [ reg R_AX RW Implicit ]
                           }
                        ]
   }

i_aad :: X86Insn
i_aad = insn
   { insnDesc        = "ASCII adjust AX before division"
   , insnMnemonic    = "AAD"
   , insnFlags       = [ Modified  [SF,ZF,PF]
                       , Undefined [OF,AF,CF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xD5
                           , legacyProperties      = [LegacyModeSupport]
                           , legacyParams          = [ reg R_AX RW Implicit
                                                     , imm8
                                                     ]
                           }
                        ]
   }

i_aam :: X86Insn
i_aam = insn
   { insnDesc        = "ASCII adjust AX after multiply"
   , insnMnemonic    = "AAM"
   , insnProperties  = [FailOnZero 0]
   , insnFlags       = [ Modified  [SF,ZF,PF]
                       , Undefined [OF,AF,CF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xD4
                           , legacyProperties      = [LegacyModeSupport]
                           , legacyParams          = [ reg R_AX RW Implicit
                                                     , imm8
                                                     ]
                           }
                        ]
   }


i_aas :: X86Insn
i_aas = insn
   { insnDesc        = "ASCII adjust AL after subtraction"
   , insnMnemonic    = "AAS"
   , insnFlags       = [ Modified  [AF,CF]
                       , Undefined [OF,SF,ZF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x3F
                           , legacyProperties      = [LegacyModeSupport]
                           , legacyParams          = [ reg R_AX RW Implicit ]
                           }
                       ]
   }

i_adc :: X86Insn
i_adc = insn
   { insnDesc        = "Add with carry"
   , insnMnemonic    = "ADC"
   , insnFlags       = [ Read     [CF]
                       , Modified [OF,SF,ZF,AF,CF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x14
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regAccu RW
                                                     , immSE 
                                                     ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x10
                           , legacyNoForce8bit     = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , gpr RO Reg
                                                     ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 2
                           , legacyNoForce8bit     = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , immSE
                                                     ]
                           }
                        ]
   }

i_adcx :: X86Insn
i_adcx = insn
   { insnDesc        = "Unsigned integer addition with carry flags"
   , insnMnemonic    = "ADCX"
   , insnFlags       = [ Read     [CF]
                       , Modified [CF]
                       ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xF6
                           , legacyProperties      = [Extension ADX]
                           , legacyParams          = [ reg32o64 RW Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                        ]
   }

i_add :: X86Insn
i_add = insn
   { insnDesc        = "Add"
   , insnMnemonic    = "ADD"
   , insnFlags       = [Modified [OF,SF,ZF,AF,CF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x04
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regAccu RW
                                                     , immSE
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x00
                           , legacyNoForce8bit     = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , gpr RO Reg
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 0
                           , legacyNoForce8bit     = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , immSE
                                                     ]
                           }
                       ]
   }

i_addpd :: X86Insn
i_addpd = insn
   { insnDesc        = "Add packed double-precision floating-point values"
   , insnMnemonic    = "ADDPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x58
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaddpd :: X86Insn
i_vaddpd = insn
   { insnDesc        = "Add packed double-precision floating-point values"
   , insnMnemonic    = "VADDPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x58
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_addps :: X86Insn
i_addps = insn
   { insnDesc        = "Add packed single-precision floating-point values"
   , insnMnemonic    = "ADDPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x58
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128  RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaddps :: X86Insn
i_vaddps = insn
   { insnDesc        = "Add packed single-precision floating-point values"
   , insnMnemonic    = "VADDPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x58
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_addsd :: X86Insn
i_addsd = insn
   { insnDesc        = "Add scalar double-precision floating-point values"
   , insnMnemonic    = "ADDSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x58
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vaddsd :: X86Insn
i_vaddsd = insn
   { insnDesc        = "Add scalar double-precision floating-point values"
   , insnMnemonic    = "VADDSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x58
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_addss :: X86Insn
i_addss = insn
   { insnDesc        = "Add scalar single-precision floating-point values"
   , insnMnemonic    = "ADDSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x58
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vaddss :: X86Insn
i_vaddss = insn
   { insnDesc        = "Add scalar single-precision floating-point values"
   , insnMnemonic    = "VADDSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x58
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_addsubpd :: X86Insn
i_addsubpd = insn
   { insnDesc        = "Packed double-FP add/subtract"
   , insnMnemonic    = "ADDSUBPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xD0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaddsubpd :: X86Insn
i_vaddsubpd = insn
   { insnDesc        = "Packed double-FP add/subtract"
   , insnMnemonic    = "VADDSUBPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xD0
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_addsubps :: X86Insn
i_addsubps = insn
   { insnDesc        = "Packed single-FP add/subtract"
   , insnMnemonic    = "ADDSUBPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xD0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaddsubps :: X86Insn
i_vaddsubps = insn
   { insnDesc        = "Packed single-FP add/subtract"
   , insnMnemonic    = "VADDSUBPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0xD0
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_adox :: X86Insn
i_adox = insn
   { insnDesc        = "Unsigned integer addition of two operands with overflow flag"
   , insnMnemonic    = "ADOX"
   , insnFlags       = [ Read     [OF]
                       , Modified [OF]
                       ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xF6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension ADX
                                                     ]
                           , legacyParams          = [ reg32o64 RW Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       ]
   }

i_aesdec :: X86Insn
i_aesdec = insn
   { insnDesc        = "Perform one round of an AES decryption flow"
   , insnMnemonic    = "AESDEC"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xDE
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaesdec :: X86Insn
i_vaesdec = insn
   { insnDesc        = "Perform one round of an AES decryption flow"
   , insnMnemonic    = "VAESDEC"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x02
                           , vexOpcode          = 0xDE
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_aesdeclast :: X86Insn
i_aesdeclast = insn
   { insnDesc        = "Perform last round of an AES decryption flow"
   , insnMnemonic    = "AESDECLAST"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xDF
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaesdeclast :: X86Insn
i_vaesdeclast = insn
   { insnDesc        = "Perform last round of an AES decryption flow"
   , insnMnemonic    = "VAESDECLAST"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x02
                           , vexOpcode          = 0xDF
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_aesenc :: X86Insn
i_aesenc = insn
   { insnDesc        = "Perform one round of an AES encryption flow"
   , insnMnemonic    = "AESENC"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xDC
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaesenc :: X86Insn
i_vaesenc = insn
   { insnDesc        = "Perform one round of an AES encryption flow"
   , insnMnemonic    = "VAESENC"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x02
                           , vexOpcode          = 0xDC
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_aesenclast :: X86Insn
i_aesenclast = insn
   { insnDesc        = "Perform last round of an AES encryption flow"
   , insnMnemonic    = "AESENCLAST"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xDD
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaesenclast :: X86Insn
i_vaesenclast = insn
   { insnDesc        = "Perform last round of an AES encryption flow"
   , insnMnemonic    = "VAESENCLAST"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x02
                           , vexOpcode          = 0xDD
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_aesimc :: X86Insn
i_aesimc = insn
   { insnDesc        = "Perform the AES InvMixColumn transformation"
   , insnMnemonic    = "AESIMC"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xDB
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vaesimc :: X86Insn
i_vaesimc = insn
   { insnDesc        = "Perform the AES InvMixColumn transformation"
   , insnMnemonic    = "VAESIMC"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x02
                           , vexOpcode          = 0xDB
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_aeskeygenassist :: X86Insn
i_aeskeygenassist = insn
   { insnDesc        = "AES round key generation assist"
   , insnMnemonic    = "AESKEYGENASSIST"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0xDF
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AES
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vaeskeygenassist :: X86Insn
i_vaeskeygenassist = insn
   { insnDesc        = "AES round key generation assist"
   , insnMnemonic    = "VAESKEYGENASSIST"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0xDF
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AES
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , mvec128 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_and :: X86Insn
i_and = insn
   { insnDesc        = "Logical AND"
   , insnMnemonic    = "AND"
   , insnFlags       = [ Unset     [OF,CF]
                       , Modified  [SF,ZF,PF]
                       , Undefined [AF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x24
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regAccu RW
                                                  , immSE
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x20
                           , legacyNoForce8bit  = Just 0
                           , legacyReversable   = Just 1
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 4
                           , legacyNoForce8bit     = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , immSE
                                                     ]
                           }
                       ]
   }

i_andn :: X86Insn
i_andn = insn
   { insnDesc        = "Logical AND NOT"
   , insnMnemonic    = "ANDN"
   , insnFlags       = [ Modified  [SF,ZF]
                       , Unset     [OF,CF]
                       , Undefined [AF,PF]
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x02
                           , vexOpcode       = 0xF2
                           , vexLW           = L0
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension BMI1
                                               ]
                           , vexParams       = [ reg32o64 WO Reg
                                               , reg32o64 RO Vvvv
                                               , rm32o64 RO
                                               ]
                           }
                       ]
   }

i_andpd :: X86Insn
i_andpd = insn
   { insnDesc        = "Bitwise logical AND of packed double-precision floating-point values"
   , insnMnemonic    = "ANDPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x54
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vandpd :: X86Insn
i_vandpd = insn
   { insnDesc        = "Bitwise logical AND of packed double-precision floating-point values"
   , insnMnemonic    = "VANDPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x54
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_andps :: X86Insn
i_andps = insn
   { insnDesc        = "Bitwise logical AND of packed float-precision floating-point values"
   , insnMnemonic    = "ANDPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x54
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE
                                                  ]
                           , legacyParams       = [ vec128 RW Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_vandps :: X86Insn
i_vandps = insn
   { insnDesc        = "Bitwise logical AND of packed float-precision floating-point values"
   , insnMnemonic    = "VANDPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x54
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128o256 WO Reg
                                               , vec128o256 RO Vvvv
                                               , mvec128o256 RO
                                               ]
                           }
                       ]
   }

i_andnpd :: X86Insn
i_andnpd = insn
   { insnDesc        = "Bitwise logical AND NOT of packed double-precision floating-point values"
   , insnMnemonic    = "ANDNPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x55
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vandnpd :: X86Insn
i_vandnpd = insn
   { insnDesc        = "Bitwise logical AND NOT of packed double-precision floating-point values"
   , insnMnemonic    = "VANDNPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x55
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_andnps :: X86Insn
i_andnps = insn
   { insnDesc        = "Bitwise logical AND of packed float-precision floating-point values"
   , insnMnemonic    = "ANDNPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x55
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE
                                                  ]
                           , legacyParams       = [ vec128 RW Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_vandnps :: X86Insn
i_vandnps = insn
   { insnDesc        = "Bitwise logical AND of packed float-precision floating-point values"
   , insnMnemonic    = "VANDNPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x55
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128o256 WO Reg
                                               , vec128o256 RO Vvvv
                                               , mvec128o256 RO
                                               ]
                           }
                       ]
   }

i_arpl :: X86Insn
i_arpl = insn
   { insnDesc        = "Adjust RPL field of segment selector"
   , insnMnemonic    = "ARPL"
   , insnFlags       = [Modified [ZF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x63
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ rm16 RW
                                                  , reg16 RO Reg
                                                  ]
                           }
                       ]
   }

i_blendpd :: X86Insn
i_blendpd = insn
   { insnDesc        = "Blend packed double-precision floating-point values"
   , insnMnemonic    = "BLENDPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x0D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vblendpd :: X86Insn
i_vblendpd = insn
   { insnDesc        = "Blend packed double-precision floating-point values"
   , insnMnemonic    = "VBLENDPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x0D
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  , mask Imm8h
                                                  ]
                           }
                       ]
   }

i_bextr :: X86Insn
i_bextr = insn
   { insnDesc        = "Bit field extract"
   , insnMnemonic    = "BEXTR"
   , insnFlags       = [ Modified [ZF]
                       , Undefined [AF,SF,PF]
                       , Unset (allFlags \\ [ZF,AF,SF,PF])
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x02
                           , vexOpcode       = 0xF7
                           , vexLW           = L0
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension BMI1
                                               ]
                           , vexParams       = [ reg32o64 WO Reg
                                               , rm32o64 RO
                                               , reg32o64 RO Vvvv
                                               ]
                           }
                       ]
   }

i_blendps :: X86Insn
i_blendps = insn
   { insnDesc        = "Blend packed single-precision floating-point values"
   , insnMnemonic    = "BLENDPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x0C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vblendps :: X86Insn
i_vblendps = insn
   { insnDesc        = "Blend packed single-precision floating-point values"
   , insnMnemonic    = "VBLENDPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x0C
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_blendvpd :: X86Insn
i_blendvpd = insn
   { insnDesc        = "Variable blend packed double-precision floating-point values"
   , insnMnemonic    = "BLENDVPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0x15
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , reg (R_XMM 0) RO Implicit
                                                     ]
                           }
                       ]
   }

i_vblendvpd :: X86Insn
i_vblendvpd = insn
   { insnDesc        = "Variable blend packed double-precision floating-point values"
   , insnMnemonic    = "VBLENDVPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x4B
                           , vexLW              = W0
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  , vec128o256 RO Imm8h
                                                  ]
                           }
                       ]
   }

i_blendvps :: X86Insn
i_blendvps = insn
   { insnDesc        = "Variable blend packed single-precision floating-point values"
   , insnMnemonic    = "BLENDVPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0x14
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , reg (R_XMM 0) RO Implicit
                                                     ]
                           }
                       ]
   }

i_vblendvps :: X86Insn
i_vblendvps = insn
   { insnDesc        = "Variable blend packed single-precision floating-point values"
   , insnMnemonic    = "VBLENDVPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x4A
                           , vexLW              = W0
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  , vec128o256 RO Imm8h
                                                  ]
                           }
                       ]
   }

i_blsi :: X86Insn
i_blsi = insn
   { insnDesc        = "Extract lowest set isolated bit"
   , insnMnemonic    = "BLSI"
   , insnFlags       = [ Modified  [ZF,SF,CF]
                       , Unset     [OF]
                       , Undefined [AF,PF]
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x02
                           , vexOpcode       = 0xF3
                           , vexOpcodeExt    = Just 3
                           , vexLW           = L0
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension BMI1
                                               ]
                           , vexParams       = [ reg32o64 WO Vvvv
                                               , rm32o64 RO
                                               ]
                           }
                       ]
   }

i_blsmsk :: X86Insn
i_blsmsk = insn
   { insnDesc        = "Get mask up to lowest set bit"
   , insnMnemonic    = "BLSMSK"
   , insnFlags       = [ Modified  [SF,CF]
                       , Unset     [ZF,OF]
                       , Undefined [AF,PF]
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x02
                           , vexOpcode       = 0xF3
                           , vexOpcodeExt    = Just 2
                           , vexLW           = L0
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension BMI1
                                               ]
                           , vexParams       = [ reg32o64 WO Vvvv
                                               , rm32o64 RO
                                               ]
                           }
                       ]
   }

i_blsr :: X86Insn
i_blsr = insn
   { insnDesc        = "Reset lowest set bit"
   , insnMnemonic    = "BLSR"
   , insnFlags       = [ Modified  [ZF,SF,CF]
                       , Unset     [OF]
                       , Undefined [AF,PF]
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x02
                           , vexOpcode       = 0xF3
                           , vexOpcodeExt    = Just 1
                           , vexLW           = L0
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension BMI1
                                               ]
                           , vexParams       = [ reg32o64 WO Vvvv
                                               , rm32o64 RO
                                               ]
                           }
                       ]
   }

i_bound :: X86Insn
i_bound = insn
   { insnDesc        = "Check array index against bounds"
   , insnMnemonic    = "BOUND"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x62
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ gpr RO Reg
                                                  , op    RO    (T_Mem MemPair16o32) RM
                                                  ]
                           }
                       ]
   }

i_bsf :: X86Insn
i_bsf = insn
   { insnDesc        = "Bit scan forward"
   , insnMnemonic    = "BSF"
   , insnFlags       = [ Modified  [ZF]
                       , Undefined [CF,OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBC
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr WO Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_bsr :: X86Insn
i_bsr = insn
   { insnDesc        = "Bit scan reverse"
   , insnMnemonic    = "BSR"
   , insnFlags       = [ Modified  [ZF]
                       , Undefined [CF,OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBD
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr WO Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_bswap :: X86Insn
i_bswap = insn
   { insnDesc        = "Byte swap"
   , insnMnemonic    = "BSWAP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xC8
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Arch Intel486
                                                  ]
                           , legacyParams       = [ reg32o64 RW OpcodeLow3 ]
                           }
                       ]
   }

i_bt :: X86Insn
i_bt = insn
   { insnDesc        = "Bit test"
   , insnMnemonic    = "BT"
   , insnFlags       = [ Modified  [CF]
                       , Undefined [OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xA3
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RO
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBA
                           , legacyOpcodeExt    = Just 4
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_btc :: X86Insn
i_btc = insn
   { insnDesc        = "Bit test and complement"
   , insnMnemonic    = "BTC"
   , insnFlags       = [ Modified  [CF]
                       , Undefined [OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBB
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBA
                           , legacyOpcodeExt    = Just 7
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_btr :: X86Insn
i_btr = insn
   { insnDesc        = "Bit test and reset"
   , insnMnemonic    = "BTR"
   , insnFlags       = [ Modified  [CF]
                       , Undefined [OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xB3
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBA
                           , legacyOpcodeExt    = Just 6
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_bts :: X86Insn
i_bts = insn
   { insnDesc        = "Bit test and set"
   , insnMnemonic    = "BTS"
   , insnFlags       = [ Modified  [CF]
                       , Undefined [OF,SF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xAB
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBA
                           , legacyOpcodeExt    = Just 5
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_bzhi :: X86Insn
i_bzhi = insn
   { insnDesc           = "Zero high bits starting with specified bit position"
   , insnMnemonic       = "BZHI"
   , insnFlags          = [ Modified  [ZF,CF,SF]
                          , Unset     [OF]
                          , Undefined [AF,PF]
                          ]
   , insnEncodings      = [ vex
                              { vexOpcodeMap    = MapVex 0x02
                              , vexOpcode       = 0xF5
                              , vexLW           = L0
                              , vexProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension BMI2
                                                  ]
                              , vexParams       = [ reg32o64 WO Reg
                                                  , rm32o64 RO
                                                  , reg32o64 RO Vvvv
                                                  ]
                              }
                          ]
   }

i_call :: X86Insn
i_call = insn
   { insnDesc        = "Call procedure"
   , insnMnemonic    = "CALL"
   , insnFlags       = [Undefined allFlags]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xE8
                           , legacyProperties   = [ DefaultOperandSize64
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ rel16o32 ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 2
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ mgpr RO ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 2
                           , legacyProperties   = [ LongModeSupport
                                                  , DefaultOperandSize64
                                                  ]
                           , legacyParams       = [ rm64 RO ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x9A
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ ptr16x ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 3
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ m16x ]
                           }
                       ]
   }

i_extend_signed :: X86Insn
i_extend_signed = insn
   { insnDesc        = "Extend signed word"
   , insnMnemonic    = "CBW/CWDE/CDQE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x98
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regAccu RW ]
                           }
                       ]
   }

i_clac :: X86Insn
i_clac = insn
   { insnDesc        = "Clear AC flag in EFLAGS register"
   , insnMnemonic    = "CLAC"
   , insnFlags       = [Unset [AC]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0x01
                           , legacyOpcodeFullExt = Just 0xCA
                           , legacyProperties    = [ LegacyModeSupport
                                                   , LongModeSupport
                                                   , Extension SMAP
                                                   ]
                           }
                       ]
   }

i_clc :: X86Insn
i_clc = insn
   { insnDesc        = "Clear carry flag"
   , insnMnemonic    = "CLC"
   , insnFlags       = [Unset [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xF8
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_cld :: X86Insn
i_cld = insn
   { insnDesc        = "Clear direction flag"
   , insnMnemonic    = "CLD"
   , insnFlags       = [Unset [DF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFC
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_clflush :: X86Insn
i_clflush = insn
   { insnDesc        = "Flush cache line"
   , insnMnemonic    = "CLFLUSH"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xAE
                           , legacyOpcodeExt    = Just 7
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension CLFLUSH
                                                  ]
                           , legacyParams       = [ mvoid ]
                           }
                       ]
   }


i_cli :: X86Insn
i_cli = insn
   { insnDesc        = "Clear interrupt flag"
   , insnMnemonic    = "CLI"
   , insnFlags       = [Unset [IF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFA
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_clts :: X86Insn
i_clts = insn
   { insnDesc        = "Clear task-switched flag in CR0"
   , insnMnemonic    = "CLTS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x06
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_cmc :: X86Insn
i_cmc = insn
   { insnDesc        = "Complement carry flag"
   , insnMnemonic    = "CMC"
   , insnFlags       = [Modified [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xF5
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_cmovo :: X86Insn
i_cmovo = insn
   { insnDesc        = "Move if overflow (OF=1)"
   , insnMnemonic    = "CMOVO"
   , insnFlags       = [Read [OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x40
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovno :: X86Insn
i_cmovno = insn
   { insnDesc        = "Move if not overflow (OF=0)"
   , insnMnemonic    = "CMOVNO"
   , insnFlags       = [Read [OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x41
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovc :: X86Insn
i_cmovc = insn
   { insnDesc        = "Move if carry (CF=1)"
   , insnMnemonic    = "CMOVC"
   , insnFlags       = [Read [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x42
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovnc :: X86Insn
i_cmovnc = insn
   { insnDesc        = "Move if not carry (CF=0)"
   , insnMnemonic    = "CMOVNC"
   , insnFlags       = [Read [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x43
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovz :: X86Insn
i_cmovz = insn
   { insnDesc        = "Move if zero (ZF=1)"
   , insnMnemonic    = "CMOVZ"
   , insnFlags       = [Read [ZF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x44
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovnz :: X86Insn
i_cmovnz = insn
   { insnDesc        = "Move if not zero (ZF=0)"
   , insnMnemonic    = "CMOVNZ"
   , insnFlags       = [Read [ZF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x45
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovbe :: X86Insn
i_cmovbe = insn
   { insnDesc        = "Move if below or equal (CF=1, ZF=1)"
   , insnMnemonic    = "CMOVBE"
   , insnFlags       = [Read [ZF,CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x46
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmova :: X86Insn
i_cmova = insn
   { insnDesc        = "Move if above (CF=0, ZF=0)"
   , insnMnemonic    = "CMOVA"
   , insnFlags       = [Read [ZF,CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x47
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovs :: X86Insn
i_cmovs = insn
   { insnDesc        = "Move if sign (SF=1)"
   , insnMnemonic    = "CMOVS"
   , insnFlags       = [Read [SF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x48
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovns :: X86Insn
i_cmovns = insn
   { insnDesc        = "Move if not sign (SF=0)"
   , insnMnemonic    = "CMOVNS"
   , insnFlags       = [Read [SF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x49
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovp :: X86Insn
i_cmovp = insn
   { insnDesc        = "Move if parity even (PF=1)"
   , insnMnemonic    = "CMOVP"
   , insnFlags       = [Read [PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4a
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovnp :: X86Insn
i_cmovnp = insn
   { insnDesc        = "Move if parity odd (PF=0)"
   , insnMnemonic    = "CMOVNP"
   , insnFlags       = [Read [PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4b
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovl :: X86Insn
i_cmovl = insn
   { insnDesc        = "Move if less (SF /= OF)"
   , insnMnemonic    = "CMOVL"
   , insnFlags       = [Read [SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4c
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovge :: X86Insn
i_cmovge = insn
   { insnDesc        = "Move if greater or equal (SF = OF)"
   , insnMnemonic    = "CMOVGE"
   , insnFlags       = [Read [SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4d
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovle :: X86Insn
i_cmovle = insn
   { insnDesc        = "Move if less or equal (ZF = 1 or SF <> OF)"
   , insnMnemonic    = "CMOVLE"
   , insnFlags       = [Read [ZF,SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4e
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_cmovg :: X86Insn
i_cmovg = insn
   { insnDesc        = "Move if greater (ZF = 0 or SF = OF)"
   , insnMnemonic    = "CMOVG"
   , insnFlags       = [Read [ZF,SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x4f
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ gpr RW Reg
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }


i_cmp :: X86Insn
i_cmp = insn
   { insnDesc        = "Compare"
   , insnMnemonic    = "CMP"
   , insnFlags       = [Modified [OF,SF,ZF,AF,CF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x3C
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regAccu RO
                                                  , immSE
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x38
                           , legacyNoForce8bit  = Just 0
                           , legacyReversable   = Just 1
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RO
                                                  , gpr RO Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 7
                           , legacyNoForce8bit     = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RO
                                                     , immSE
                                                     ]
                           }
                       ]
   }

i_cmppd :: X86Insn
i_cmppd = insn
   { insnDesc        = "Compare packed double-precision floating-point values"
   , insnMnemonic    = "CMPPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0x66
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xC2
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE2
                                                        ]
                           , legacyParams             = [ vec128 RW Reg
                                                        , mvec128 RO
                                                        , imm8
                                                        ]
                           }
                       ]
   }

i_vcmppd :: X86Insn
i_vcmppd = insn
   { insnDesc        = "Compare packed double-precision floating-point values"
   , insnMnemonic    = "VCMPPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0xC2
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_cmpps :: X86Insn
i_cmpps = insn
   { insnDesc        = "Compare packed single-precision floating-point values"
   , insnMnemonic    = "CMPPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xC2
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE
                                                  ]
                           , legacyParams       = [ vec128 RW Reg
                                                  , mvec128 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_vcmpps :: X86Insn
i_vcmpps = insn
   { insnDesc        = "Compare packed single-precision floating-point values"
   , insnMnemonic    = "VCMPPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0xC2
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128o256 WO Reg
                                               , vec128o256 RO Vvvv
                                               , mvec128o256 RO
                                               , imm8
                                               ]
                           }
                       ]
   }

i_cmps :: X86Insn
i_cmps = insn
   { insnDesc        = "Compare string operands"
   , insnMnemonic    = "CMPS"
   , insnFlags       = [Modified [CF,OF,SF,ZF,AF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xA6
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mDSrSI RO
                                                  , mESrDI RO
                                                  ]
                           }
                       ]
   }

i_cmpsd :: X86Insn
i_cmpsd = insn
   { insnDesc        = "Compare scalar double-precision floating-point values"
   , insnMnemonic    = "CMPSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0xF2
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xC2
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE2
                                                        ]
                           , legacyParams             = [ vec128 RW Reg
                                                        , mvec128 RO
                                                        , imm8
                                                        ]
                           }
                       ]
   }

i_vcmpsd :: X86Insn
i_vcmpsd = insn
   { insnDesc        = "Compare scalar double-precision floating-point values"
   , insnMnemonic    = "VCMPSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0xC2
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128 RO Vvvv
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_cmpss :: X86Insn
i_cmpss = insn
   { insnDesc        = "Compare scalar single-precision floating-point values"
   , insnMnemonic    = "CMPSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0xF3
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xC2
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE
                                                        ]
                           , legacyParams             = [ vec128 RW Reg
                                                        , mvec128 RO
                                                        , imm8
                                                        ]
                           }
                       ]
   }

i_vcmpss :: X86Insn
i_vcmpss = insn
   { insnDesc        = "Compare scalar single-precision floating-point values"
   , insnMnemonic    = "VCMPSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0xC2
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128 RO Vvvv
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_cmpxchg :: X86Insn
i_cmpxchg = insn
   { insnDesc        = "Compare and exchange"
   , insnMnemonic    = "CMPXCHG"
   , insnFlags       = [Modified [ZF,CF,PF,AF,SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xB0
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  , Arch Intel486
                                                  ]
                           , legacyParams       = [ mgpr RW
                                                  , regAccu RO
                                                  , gpr RO Reg
                                                  ]
                           }
                       ]
   }

i_cmpxch8b :: X86Insn
i_cmpxch8b = insn
   { insnDesc        = "Compare and exchange bytes"
   , insnMnemonic    = "CMPXCHG8B/CMPXCHG16B"
   , insnFlags       = [Modified [ZF,CF,PF,AF,SF,OF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xC7
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  , Arch IntelPentium
                                                  , Extension CX8
                                                  ]
                           , legacyParams       = [ rDXrAX RW
                                                  , rCXrBX RO
                                                  , mem64o128 RW
                                                  ]
                           }
                       ]
   }


i_comisd :: X86Insn
i_comisd = insn
   { insnDesc        = "Compare scalar ordered double-precision floating-point values and set EFLAGS"
   , insnMnemonic    = "COMISD"
   , insnFlags       = [ Modified [ZF,PF,CF]
                       , Unset    [OF,SF,AF]
                       ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0x66
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0x2F
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE2
                                                        ]
                           , legacyParams             = [ vec128low64 RO Reg
                                                        , mvec128low64 RO
                                                        ]
                           }
                       ]
   }

i_vcomisd :: X86Insn
i_vcomisd = insn
   { insnDesc        = "Compare scalar ordered double-precision floating-point values and set EFLAGS"
   , insnMnemonic    = "VCOMISD"
   , insnFlags       = [ Modified [ZF,PF,CF]
                       , Unset    [OF,SF,AF]
                       ]
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2F
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128low64 RO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_comiss :: X86Insn
i_comiss = insn
   { insnDesc        = "Compare scalar ordered single-precision floating-point values and set EFLAGS"
   , insnMnemonic    = "COMISS"
   , insnFlags       = [ Modified [ZF,PF,CF]
                       , Unset    [OF,SF,AF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x2F
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE
                                                  ]
                           , legacyParams       = [ vec128low32 RO Reg
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_vcomiss :: X86Insn
i_vcomiss = insn
   { insnDesc        = "Compare scalar ordered single-precision floating-point values and set EFLAGS"
   , insnMnemonic    = "VCOMISS"
   , insnFlags       = [ Modified [ZF,PF,CF]
                       , Unset    [OF,SF,AF]
                       ]
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x2F
                           , vexLW           = LWIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128low32 RO Reg
                                               , mvec128low32 RO
                                               ]
                           }
                       ]
   }

i_cpuid :: X86Insn
i_cpuid = insn
   { insnDesc        = "CPU identification"
   , insnMnemonic    = "CPUID"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xA2
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ reg R_EAX RW Implicit
                                                  , reg R_ECX RW Implicit
                                                  , reg R_EBX WO Implicit
                                                  , reg R_EDX WO Implicit
                                                  ]
                           }
                       ]
   }

i_crc32 :: X86Insn
i_crc32 = insn
   { insnDesc        = "Accumulate CRC32 value"
   , insnMnemonic    = "CRC32"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xF0
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr RW Reg
                                                     , mgpr RO
                                                     ]
                           }
                       ]
   }

i_cvtdq2pd :: X86Insn
i_cvtdq2pd = insn
   { insnDesc        = "Convert packed Int32 to packed double-precision floating-point values"
   , insnMnemonic    = "CVTDQ2PD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xE6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vcvtdq2pd :: X86Insn
i_vcvtdq2pd = insn
   { insnDesc        = "Convert packed Int32 to packed double-precision floating-point values"
   , insnMnemonic    = "VCVTDQ2PD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xE6
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mveclow RO
                                                  ]
                           }
                       ]
   }

i_cvtdq2ps :: X86Insn
i_cvtdq2ps = insn
   { insnDesc        = "Convert packed Int32 to packed single-precision floating-point values"
   , insnMnemonic    = "CVTDQ2PS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x5B
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE2
                                                  ]
                           , legacyParams       = [ vec128 WO Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_vcvtdq2ps :: X86Insn
i_vcvtdq2ps = insn
   { insnDesc        = "Convert packed Int32 to packed single-precision floating-point values"
   , insnMnemonic    = "VCVTDQ2PS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x5B
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128o256 WO Reg
                                               , mvec128o256 RO
                                               ]
                           }
                       ]
   }

i_cvtpd2dq :: X86Insn
i_cvtpd2dq = insn
   { insnDesc        = "Convert packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTPD2DQ"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xE6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vcvtpd2dq :: X86Insn
i_vcvtpd2dq = insn
   { insnDesc        = "Convert packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "VCVTPD2DQ"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xE6
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_cvtpd2di :: X86Insn
i_cvtpd2di = insn
   { insnDesc        = "Convert packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTPD2DI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ vec64 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_cvtpd2ps :: X86Insn
i_cvtpd2ps = insn
   { insnDesc        = "Convert packed double-precision floating-point values to packed single-precision floating-point values"
   , insnMnemonic    = "CVTPD2PS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vcvtpd2ps :: X86Insn
i_vcvtpd2ps = insn
   { insnDesc        = "Convert packed double-precision floating-point values to packed single-precision floating-point values"
   , insnMnemonic    = "VCVTPD2PS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5A
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_cvtpi2pd :: X86Insn
i_cvtpi2pd = insn
   { insnDesc        = "Convert packed Int32 to packed double-precision floating-point values"
   , insnMnemonic    = "CVTPI2PD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec64 RO
                                                     ]
                           }
                       ]
   }

i_cvtpi2ps :: X86Insn
i_cvtpi2ps = insn
   { insnDesc        = "Convert packed Int32 to packed single-precision floating-point values"
   , insnMnemonic    = "CVTPI2PS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x2A
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ vec128 WO Reg
                                                  , mvec64 RO
                                                  ]
                           }
                       ]
   }

i_cvtps2dq :: X86Insn
i_cvtps2dq = insn
   { insnDesc        = "Convert packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTPS2DQ"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vcvtps2dq :: X86Insn
i_vcvtps2dq = insn
   { insnDesc        = "Convert packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "VCVTPS2DQ"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5B
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_cvtps2pd :: X86Insn
i_cvtps2pd = insn
   { insnDesc        = "Convert packed single-precision floating-point values to packed double-precision floating-point values"
   , insnMnemonic    = "CVTPS2PD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x5A
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE2
                                                  ]
                           , legacyParams       = [ vec128 WO Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_vcvtps2pd :: X86Insn
i_vcvtps2pd = insn
   { insnDesc        = "Convert packed single-precision floating-point values to packed double-precision floating-point values"
   , insnMnemonic    = "VCVTPS2PD"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x5A
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128 WO Reg
                                               , mvec128o256 RO
                                               ]
                           }
                       ]
   }

i_cvtps2pi :: X86Insn
i_cvtps2pi = insn
   { insnDesc        = "Convert packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTPS2PI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x2D
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ vec64 WO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_cvtsd2si :: X86Insn
i_cvtsd2si = insn
   { insnDesc        = "Convert scalar double-precision floating-point value to integer"
   , insnMnemonic    = "CVTSD2SI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ reg32o64 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vcvtsd2si :: X86Insn
i_vcvtsd2si = insn
   { insnDesc        = "Convert scalar double-precision floating-point value to integer"
   , insnMnemonic    = "VCVTSD2SI"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2D
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ reg32o64 WO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_cvtsd2ss :: X86Insn
i_cvtsd2ss = insn
   { insnDesc        = "Convert scalar double-precision floating-point value to scalar single-precision floating-point value"
   , insnMnemonic    = "CVTSD2SS"
   , insnEncodings   = [leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vcvtsd2ss :: X86Insn
i_vcvtsd2ss = insn
   { insnDesc        = "Convert scalar double-precision floating-point value to scalar single-precision floating-point value"
   , insnMnemonic    = "VCVTSD2SS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5A
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_cvtsi2sd :: X86Insn
i_cvtsi2sd = insn
   { insnDesc        = "Convert Int32 to scalar double-precision floating-point value"
   , insnMnemonic    = "CVTSI2SD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       ]
   }

i_vcvtsi2sd :: X86Insn
i_vcvtsi2sd = insn
   { insnDesc        = "Convert Int32 to scalar double-precision floating-point value"
   , insnMnemonic    = "VCVTSI2SD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2A
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , rm32o64 RO
                                                  ]
                           }
                       ]
   }


i_cvtsi2ss :: X86Insn
i_cvtsi2ss = insn
   { insnDesc        = "Convert Int32 to scalar single-precision floating-point value"
   , insnMnemonic    = "CVTSI2SS"
   , insnEncodings   = [leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       ]
   }

i_vcvtsi2ss :: X86Insn
i_vcvtsi2ss = insn
   { insnDesc        = "Convert Int32 to scalar single-precision floating-point value"
   , insnMnemonic    = "VCVTSI2SS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2A
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , rm32o64 RO
                                                  ]
                           }
                       ]
   }

i_cvtss2sd :: X86Insn
i_cvtss2sd = insn
   { insnDesc        = "Convert scalar single-precision floating-point value to scalar double-precision floating-point value"
   , insnMnemonic    = "CVTSS2SD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vcvtss2sd :: X86Insn
i_vcvtss2sd = insn
   { insnDesc        = "Convert scalar single-precision floating-point value to scalar double-precision floating-point value"
   , insnMnemonic    = "VCVTSS2SD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5A
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_cvtss2si :: X86Insn
i_cvtss2si = insn
   { insnDesc        = "Convert scalar single-precision floating-point value to Int32"
   , insnMnemonic    = "CVTSS2SI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ reg32o64 WO Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vcvtss2si :: X86Insn
i_vcvtss2si = insn
   { insnDesc        = "Convert scalar single-precision floating-point value to Int32"
   , insnMnemonic    = "VCVTSS2SI"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2D
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ reg32o64 WO Reg
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_cvttpd2dq :: X86Insn
i_cvttpd2dq = insn
   { insnDesc        = "Convert with truncation packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTTPD2DQ"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xE6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vcvttpd2dq :: X86Insn
i_vcvttpd2dq = insn
   { insnDesc        = "Convert with truncation packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "VCVTTPD2DQ"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xE6
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_cvttpd2pi :: X86Insn
i_cvttpd2pi = insn
   { insnDesc        = "Convert with truncation packed double-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTTPD2PI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ vec64 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_cvttps2dq :: X86Insn
i_cvttps2dq = insn
   { insnDesc        = "Convert with truncation packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTTPS2DQ"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vcvttps2dq :: X86Insn
i_vcvttps2dq = insn
   { insnDesc        = "Convert with truncation packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "VCVTTPS2DQ"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5B
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_cvttps2pi :: X86Insn
i_cvttps2pi = insn
   { insnDesc        = "Convert with truncation packed single-precision floating-point values to packed Int32"
   , insnMnemonic    = "CVTTPS2PI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x2C
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ vec64 WO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_cvttsd2si :: X86Insn
i_cvttsd2si = insn
   { insnDesc        = "Convert with truncation scalar double-precision floating-point value to integer"
   , insnMnemonic    = "CVTTSD2SI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ reg32o64 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vcvttsd2si :: X86Insn
i_vcvttsd2si = insn
   { insnDesc        = "Convert with truncation scalar double-precision floating-point value to integer"
   , insnMnemonic    = "VCVTTSD2SI"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2C
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ reg32o64 WO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_cvttss2si :: X86Insn
i_cvttss2si = insn
   { insnDesc        = "Convert with truncation scalar single-precision floating-point value to Int32"
   , insnMnemonic    = "CVTTSS2SI"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ reg32o64 WO Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vcvttss2si :: X86Insn
i_vcvttss2si = insn
   { insnDesc        = "Convert with truncation scalar single-precision floating-point value to Int32"
   , insnMnemonic    = "VCVTTSS2SI"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2C
                           , vexLW              = LIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ reg32o64 WO Reg
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_cwd :: X86Insn
i_cwd = insn
   { insnDesc        = "Convert between words (sign-extend)"
   , insnMnemonic    = "CWD/CDQ/CQO"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x99
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regFam RegFamDX WO Implicit
                                                  , regFam RegFamAX RW Implicit
                                                  ]
                           }
                       ]
   }

i_daa :: X86Insn
i_daa = insn
   { insnDesc        = "Decimal adjust AL after addition"
   , insnMnemonic    = "DAA"
   , insnFlags       = [ Modified  [AF,CF,SF,ZF,PF]
                       , Undefined [OF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x27
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ reg R_AL RW Implicit ]
                           }
                       ]
   }

i_das :: X86Insn
i_das = insn
   { insnDesc        = "Decimal adjust AL after subtraction"
   , insnMnemonic    = "DAS"
   , insnFlags       = [ Modified  [AF,CF,SF,ZF,PF]
                       , Undefined [OF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x2F
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ reg R_AL RW Implicit ]
                           }
                       ]
   }


i_dec :: X86Insn
i_dec = insn
   { insnDesc        = "Decrement by 1"
   , insnMnemonic    = "DEC"
   , insnFlags       = [Modified [OF,SF,ZF,AF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFE
                           , legacyOpcodeExt    = Just 1
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mgpr RW ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x48
                           , legacyProperties   = [ LegacyModeSupport
                                                  , Lockable
                                                  ]
                           , legacyParams       = [ gpr RW OpcodeLow3 ]
                           }
                       ]
   }

i_div :: X86Insn
i_div = insn
   { insnDesc        = "Unsigned divide"
   , insnMnemonic    = "DIV"
   , insnProperties  = [FailOnZero 0]
   , insnFlags       = [Undefined [CF,OF,SF,ZF,AF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xF6
                           , legacyOpcodeExt    = Just 6
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regFam RegFamDXAX RW Implicit
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_divpd :: X86Insn
i_divpd = insn
   { insnDesc        = "Divide packed double-precision floating-point values"
   , insnMnemonic    = "DIVPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vdivpd :: X86Insn
i_vdivpd = insn
   { insnDesc        = "Divide packed double-precision floating-point values"
   , insnMnemonic    = "VDIVPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5E
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_divps :: X86Insn
i_divps = insn
   { insnDesc        = "Divide packed float-precision floating-point values"
   , insnMnemonic    = "DIVPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x5E
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension SSE
                                                  ]
                           , legacyParams       = [ vec128 RW Reg
                                                  , mvec128 RO
                                                  ]
                           }
                       ]
   }

i_vdivps :: X86Insn
i_vdivps = insn
   { insnDesc        = "Divide packed float-precision floating-point values"
   , insnMnemonic    = "VDIVPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap    = MapVex 0x01
                           , vexOpcode       = 0x5E
                           , vexLW           = WIG
                           , vexProperties   = [ LegacyModeSupport
                                               , LongModeSupport
                                               , Extension AVX
                                               ]
                           , vexParams       = [ vec128o256 WO Reg
                                               , vec128o256 RO Vvvv
                                               , mvec128o256 RO
                                               ]
                           }
                       ]
   }

i_divsd :: X86Insn
i_divsd = insn
   { insnDesc        = "Divide scalar double-precision floating-point values"
   , insnMnemonic    = "DIVSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vdivsd :: X86Insn
i_vdivsd = insn
   { insnDesc        = "Divide scalar double-precision floating-point values"
   , insnMnemonic    = "VDIVSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5E
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_divss :: X86Insn
i_divss = insn
   { insnDesc        = "Divide scalar single-precision floating-point values"
   , insnMnemonic    = "DIVSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vdivss :: X86Insn
i_vdivss = insn
   { insnDesc        = "Divide scalar single-precision floating-point values"
   , insnMnemonic    = "VDIVSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5E
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_dppd :: X86Insn
i_dppd = insn
   { insnDesc        = "Dot product of packed double precision floating-point values"
   , insnMnemonic    = "DPPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x41
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vdppd :: X86Insn
i_vdppd = insn
   { insnDesc        = "Dot product of packed double precision floating-point values"
   , insnMnemonic    = "VDPPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x41
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_dpps :: X86Insn
i_dpps = insn
   { insnDesc        = "Dot product of packed single precision floating-point values"
   , insnMnemonic    = "DPPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x40
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vdpps :: X86Insn
i_vdpps = insn
   { insnDesc        = "Dot product of packed single precision floating-point values"
   , insnMnemonic    = "VDPPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x40
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_emms :: X86Insn
i_emms = insn
   { insnDesc        = "Empty MMX technology state"
   , insnMnemonic    = "EMMS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0x77
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_enter :: X86Insn
i_enter = insn
   { insnDesc        = "Make stack frame for procedure parameters"
   , insnMnemonic    = "ENTER"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xC8
                           , legacyProperties   = [ DefaultOperandSize64
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ imm16
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_extractps :: X86Insn
i_extractps = insn
   { insnDesc        = "Extract packed single precision floating-point value"
   , insnMnemonic    = "EXTRACTPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x17
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ rm32 RW
                                                     , vec128 RO Reg
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vextractps :: X86Insn
i_vextractps = insn
   { insnDesc        = "Extract packed single precision floating-point value"
   , insnMnemonic    = "VEXTRACTPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x17
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ rm32 WO
                                                  , vec128 RO Vvvv
                                                  , imm8
                                                  ]
                           }
                       ]
   }


i_f2xm1 :: X86Insn
i_f2xm1 = insn
   { insnDesc        = "Compute 2^x - 1"
   , insnMnemonic    = "F2XM1"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }
                                       
i_fabs :: X86Insn
i_fabs = insn
   { insnDesc        = "Absolute value"
   , insnMnemonic    = "FABS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_fadd :: X86Insn
i_fadd = insn
   { insnDesc        = "Add"
   , insnMnemonic    = "FADD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 0
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fiadd :: X86Insn
i_fiadd = insn
   { insnDesc        = "Add"
   , insnMnemonic    = "FIADD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 0
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_fbld :: X86Insn
i_fbld = insn
   { insnDesc        = "Load binary coded decimal"
   , insnMnemonic    = "FBLD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 4
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mdec80 RO
                                                   ]
                           }
                       ]
   }

i_fbstp :: X86Insn
i_fbstp = insn
   { insnDesc        = "Store BCD integer and pop"
   , insnMnemonic    = "FBSTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 6
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mdec80 RW
                                                   ]
                           }
                       ]
   }

i_fchs :: X86Insn
i_fchs = insn
   { insnDesc        = "Change sign"
   , insnMnemonic    = "FCHS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_fnclex :: X86Insn
i_fnclex = insn
   { insnDesc        = "Clear exceptions"
   , insnMnemonic    = "FNCLEX"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeFullExt = Just 0xE2
                           , legacyProperties    = [ Extension FPU ]
                           }
                       ]
   }

i_fcmovb :: X86Insn
i_fcmovb = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVB"
   , insnFlags       = [ Read [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmove :: X86Insn
i_fcmove = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVE"
   , insnFlags       = [ Read [ZF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovbe :: X86Insn
i_fcmovbe = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVBE"
   , insnFlags       = [ Read [ZF,CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 2
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovu :: X86Insn
i_fcmovu = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVU"
   , insnFlags       = [ Read [PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 3
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovnb :: X86Insn
i_fcmovnb = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVNB"
   , insnFlags       = [ Read [CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovne :: X86Insn
i_fcmovne = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVNE"
   , insnFlags       = [ Read [ZF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovnbe :: X86Insn
i_fcmovnbe = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVNBE"
   , insnFlags       = [ Read [ZF,CF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 2
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcmovnu :: X86Insn
i_fcmovnu = insn
   { insnDesc        = "Floating-point conditional move"
   , insnMnemonic    = "FCMOVNU"
   , insnFlags       = [ Read [PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 3
                           , legacyProperties    = [ Extension FPU
                                                   , Extension CMOV
                                                   ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcom :: X86Insn
i_fcom = insn
   { insnDesc        = "Compare floating point values"
   , insnMnemonic    = "FCOM"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 2
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fcomp :: X86Insn
i_fcomp = insn
   { insnDesc        = "Compare floating point values and pop"
   , insnMnemonic    = "FCOMP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 3
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fcompp :: X86Insn
i_fcompp = insn
   { insnDesc        = "Compare floating point values and pop twice"
   , insnMnemonic    = "FCOMPP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDE
                           , legacyOpcodeFullExt = Just 0xD9
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fcomi :: X86Insn
i_fcomi = insn
   { insnDesc        = "Compare floating point values and set eflags"
   , insnMnemonic    = "FCOMI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 6
                           , legacyFPUPop        = Just 2   -- FCOMIP
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fucomi :: X86Insn
i_fucomi = insn
   { insnDesc        = "Compare floating point values and set eflags"
   , insnMnemonic    = "FUCOMI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 5
                           , legacyFPUPop        = Just 2   -- FUCOMIP
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fcos :: X86Insn
i_fcos = insn
   { insnDesc        = "Cosine"
   , insnMnemonic    = "FCOS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xff
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_fdecstp :: X86Insn
i_fdecstp = insn
   { insnDesc        = "Decrement stack-top pointer"
   , insnMnemonic    = "FDECSTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xf6
                           , legacyProperties    = [ Extension FPU ]
                           }
                       ]
   }

i_fdiv :: X86Insn
i_fdiv = insn
   { insnDesc        = "Divide ST(O)"
   , insnMnemonic    = "FDIV"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 6
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fidiv :: X86Insn
i_fidiv = insn
   { insnDesc        = "Divide ST(0)"
   , insnMnemonic    = "FIDIV"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 6
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }


i_fdivr :: X86Insn
i_fdivr = insn
   { insnDesc        = "Divide by ST(0)"
   , insnMnemonic    = "FDIVR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 7
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fidivr :: X86Insn
i_fidivr = insn
   { insnDesc        = "Divide by ST(0)"
   , insnMnemonic    = "FIDIVR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 7
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_ffree :: X86Insn
i_ffree = insn
   { insnDesc        = "Free floating-point register"
   , insnMnemonic    = "FFREE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ st NA ]
                           }
                       ]
   }

i_ficom :: X86Insn
i_ficom = insn
   { insnDesc        = "Compare integer"
   , insnMnemonic    = "FICOM"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 2
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_ficomp :: X86Insn
i_ficomp = insn
   { insnDesc        = "Compare integer and pop"
   , insnMnemonic    = "FICOMP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 3
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_fild :: X86Insn
i_fild = insn
   { insnDesc        = "Load integer"
   , insnMnemonic    = "FILD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 0
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mint RO ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mint64 RO ]
                           }
                       ]
   }

i_fincstp :: X86Insn
i_fincstp = insn
   { insnDesc        = "Increment stack-top pointer"
   , insnMnemonic    = "FINCSTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xf7
                           , legacyProperties    = [ Extension FPU ]
                           }
                       ]
   }

i_finit :: X86Insn
i_finit = insn
   { insnDesc        = "Initialize floating-point unit"
   , insnMnemonic    = "FINIT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeFullExt = Just 0xE3
                           , legacyProperties    = [ Extension FPU ]
                           }
                       ]
   }

i_fist :: X86Insn
i_fist = insn
   { insnDesc        = "Store integer"
   , insnMnemonic    = "FIST"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 2
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint WO
                                                   ]
                           }
                       ]
   }

i_fistp :: X86Insn
i_fistp = insn
   { insnDesc        = "Store integer and pop"
   , insnMnemonic    = "FISTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 3
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint WO
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint64 WO
                                                   ]
                           }
                       ]
   }

i_fisttp :: X86Insn
i_fisttp = insn
   { insnDesc        = "Store integer with truncation and pop"
   , insnMnemonic    = "FISTTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 1
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint WO
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mint64 WO
                                                   ]
                           }
                       ]
   }

i_fld :: X86Insn
i_fld = insn
   { insnDesc        = "Load floating-point value"
   , insnMnemonic    = "FLD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 0
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , mst RO
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit
                                                   , mfp80 RO
                                                   ]
                           }
                       ]
   }

i_fld1 :: X86Insn
i_fld1 = insn
   { insnDesc        = "Load constant +1.0"
   , insnMnemonic    = "FLD1"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE8
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }

i_fldl2t :: X86Insn
i_fldl2t = insn
   { insnDesc        = "Load constant log2(10)"
   , insnMnemonic    = "FLDL2T"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE9
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }

i_fldl2e :: X86Insn
i_fldl2e = insn
   { insnDesc        = "Load constant log2(e)"
   , insnMnemonic    = "FLDL2E"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xEA
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }

i_fldpi :: X86Insn
i_fldpi = insn
   { insnDesc        = "Load constant pi"
   , insnMnemonic    = "FLDPI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xEB
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }

i_fldlg2 :: X86Insn
i_fldlg2 = insn
   { insnDesc        = "Load constant log10(2)"
   , insnMnemonic    = "FLDLG2"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xEC
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }


i_fldln2 :: X86Insn
i_fldln2 = insn
   { insnDesc        = "Load constant log_e(2)"
   , insnMnemonic    = "FLDLN2"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xED
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }


i_fldz :: X86Insn
i_fldz = insn
   { insnDesc        = "Load constant +0.0"
   , insnMnemonic    = "FLDZ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xEE
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) WO Implicit ]
                           }
                       ]
   }

i_fldcw :: X86Insn
i_fldcw = insn
   { insnDesc        = "Load x87 FPU control word"
   , insnMnemonic    = "FLDCW"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mem16 RO ]
                           }
                       ]
   }


i_fldenv :: X86Insn
i_fldenv = insn
   { insnDesc        = "Load x87 FPU environment"
   , insnMnemonic    = "FLDENV"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 4
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ menv RO ]
                           }
                       ]
   }

i_fmul :: X86Insn
i_fmul = insn
   { insnDesc        = "Multiply"
   , insnMnemonic    = "FMUL"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 1
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fimul :: X86Insn
i_fimul = insn
   { insnDesc        = "Multiply"
   , insnMnemonic    = "FIMUL"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 1
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }


i_fnop :: X86Insn
i_fnop = insn
   { insnDesc        = "No operation"
   , insnMnemonic    = "FNOP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xD0
                           , legacyProperties    = [ Extension FPU ]
                           }
                       ]
   }

i_fpatan :: X86Insn
i_fpatan = insn
   { insnDesc        = "Partial arctangent"
   , insnMnemonic    = "FPATAN"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF3
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fprem :: X86Insn
i_fprem = insn
   { insnDesc        = "Partial remainder"
   , insnMnemonic    = "FPREM"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF8
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fprem1 :: X86Insn
i_fprem1 = insn
   { insnDesc        = "Partial remainder"
   , insnMnemonic    = "FPREM1"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fptan :: X86Insn
i_fptan = insn
   { insnDesc        = "Partial tangent"
   , insnMnemonic    = "FPTAN"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RW Implicit
                                                   ]
                           }
                       ]
   }

i_frndint :: X86Insn
i_frndint = insn
   { insnDesc        = "Round to integer"
   , insnMnemonic    = "FRNDINT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xFC
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_frstor :: X86Insn
i_frstor = insn
   { insnDesc        = "Restore x87 FPU state"
   , insnMnemonic    = "FRSTOR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 4
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mstate RO ]
                           }
                       ]
   }

i_fnsave :: X86Insn
i_fnsave = insn
   { insnDesc        = "Store x87 FPU state"
   , insnMnemonic    = "FNSAVE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 6
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mstate WO ]
                           }
                       ]
   }

i_fscale :: X86Insn
i_fscale = insn
   { insnDesc        = "Scale"
   , insnMnemonic    = "FSCALE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xFD
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fsin :: X86Insn
i_fsin = insn
   { insnDesc        = "Sine"
   , insnMnemonic    = "FSIN"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xfe
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_fsincos :: X86Insn
i_fsincos = insn
   { insnDesc        = "Sine and cosine"
   , insnMnemonic    = "FSINCOS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xfb
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) WO Implicit
                                                   ]
                           }
                       ]
   }

i_fsqrt :: X86Insn
i_fsqrt = insn
   { insnDesc        = "Square root"
   , insnMnemonic    = "FSQRT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xfa
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit ]
                           }
                       ]
   }

i_fst :: X86Insn
i_fst = insn
   { insnDesc        = "Store floating-point value"
   , insnMnemonic    = "FST"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 2
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mst WO
                                                   ]
                           }
                       ]
   }

i_fstp :: X86Insn
i_fstp = insn
   { insnDesc        = "Store floating-point value and pop"
   , insnMnemonic    = "FSTP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 3
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mst WO
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , mfp80 WO
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 3
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , st WO
                                                   ]
                           }
                       ]
   }

i_fnstcw :: X86Insn
i_fnstcw = insn
   { insnDesc        = "Store x87 FPU control word"
   , insnMnemonic    = "FNSTCW"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mem16 WO ]
                           }
                       ]
   }


i_fnstenv :: X86Insn
i_fnstenv = insn
   { insnDesc        = "Store x87 FPU environment"
   , insnMnemonic    = "FNSTENV"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 6
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ menv WO ]
                           }
                       ]
   }


i_fnstsw :: X86Insn
i_fnstsw = insn
   { insnDesc        = "Store x87 FPU status word"
   , insnMnemonic    = "FNSTSW"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mem16 WO ]
                           }
                        , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeFullExt = Just 0xE0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg R_AX RO Implicit ]
                           }
                       ]
   }

i_fsub :: X86Insn
i_fsub = insn
   { insnDesc        = "Subtract from ST(O)"
   , insnMnemonic    = "FSUB"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 4
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fisub :: X86Insn
i_fisub = insn
   { insnDesc        = "Subtract from ST(0)"
   , insnMnemonic    = "FISUB"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 4
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_fsubr :: X86Insn
i_fsubr = insn
   { insnDesc        = "Subtract ST(0)"
   , insnMnemonic    = "FSUBR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD8
                           , legacyOpcodeExt     = Just 5
                           , legacyFPUSizable    = Just 2
                           , legacyFPUDest       = Just 2
                           , legacyFPUPop        = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mst RO
                                                   ]
                           }
                       ]
   }

i_fisubr :: X86Insn
i_fisubr = insn
   { insnDesc        = "Subtract ST(0)"
   , insnMnemonic    = "FISUBR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeExt     = Just 5
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , mint RO
                                                   ]
                           }
                       ]
   }

i_ftst :: X86Insn
i_ftst = insn
   { insnDesc        = "Test"
   , insnMnemonic    = "FTST"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE4
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit ]
                           }
                       ]
   }

i_fucom :: X86Insn
i_fucom = insn
   { insnDesc        = "Unordered compare floating point values"
   , insnMnemonic    = "FUCOM"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 4
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fucomp :: X86Insn
i_fucomp = insn
   { insnDesc        = "Unordered compare floating point values and pop"
   , insnMnemonic    = "FUCOMP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 5
                           , legacyFPUSizable    = Just 2
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , st RO
                                                   ]
                           }
                       ]
   }

i_fucompp :: X86Insn
i_fucompp = insn
   { insnDesc        = "Unorderd compare floating point values and pop twice"
   , insnMnemonic    = "FUCOMPP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDA
                           , legacyOpcodeFullExt = Just 0xE9
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }


i_fxam :: X86Insn
i_fxam = insn
   { insnDesc        = "Examine (classify)"
   , insnMnemonic    = "FXAM"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xE5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RO Implicit ]
                           }
                       ]
   }


i_fxch :: X86Insn
i_fxch = insn
   { insnDesc        = "Exchange register contents"
   , insnMnemonic    = "FXCH"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , st RW
                                                   ]
                           }
                       ]
   }

i_fxrstor :: X86Insn
i_fxrstor = insn
   { insnDesc        = "Restore x87 FPU, MMX, XMM, and MXCSR state"
   , insnMnemonic    = "FXRSTOR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0xAE
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mem512 RO ]
                           }
                       ]
   }

i_fxrstor64 :: X86Insn
i_fxrstor64 = insn
   { insnDesc        = "Restore x87 FPU, MMX, XMM, and MXCSR state"
   , insnMnemonic    = "FXRSTOR64"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0xAE
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU
                                                   , RequireRexW
                                                   ]
                           , legacyParams        = [ mem512 RO ]
                           }
                       ]
   }

i_fxsave :: X86Insn
i_fxsave = insn
   { insnDesc        = "Save x87 FPU, MMX, XMM, and MXCSR state"
   , insnMnemonic    = "FXSAVE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0xAE
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ mem512 WO ]
                           }
                       ]
   }

i_fxsave64 :: X86Insn
i_fxsave64 = insn
   { insnDesc        = "Restore x87 FPU, MMX, XMM, and MXCSR state"
   , insnMnemonic    = "FXSAVE64"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0xAE
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU
                                                   , RequireRexW
                                                   ]
                           , legacyParams        = [ mem512 WO ]
                           }
                       ]
   }


i_fxtract :: X86Insn
i_fxtract = insn
   { insnDesc        = "Extract exponent and significand"
   , insnMnemonic    = "FXTRACT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF4
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) WO Implicit
                                                   ]
                           }
                       ]
   }

i_fyl2x :: X86Insn
i_fyl2x = insn
   { insnDesc        = "Compute y * log_2(x)"
   , insnMnemonic    = "FYL2X"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_fyl2xp1 :: X86Insn
i_fyl2xp1 = insn
   { insnDesc        = "Compute y * log_2(x+1)"
   , insnMnemonic    = "FYL2XP1"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xD9
                           , legacyOpcodeFullExt = Just 0xF9
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ reg (R_ST 0) RW Implicit
                                                   , reg (R_ST 1) RO Implicit
                                                   ]
                           }
                       ]
   }

i_haddpd :: X86Insn
i_haddpd = insn
   { insnDesc        = "Packed double-FP horizontal add"
   , insnMnemonic    = "HADDPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x7C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vhaddpd :: X86Insn
i_vhaddpd = insn
   { insnDesc        = "Packed double-FP horizontal add"
   , insnMnemonic    = "VHADDPD"
   , insnProperties  = [ MemAlign 16 ]
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x7C
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }


i_haddps :: X86Insn
i_haddps = insn
   { insnDesc        = "Packed single-FP horizontal add"
   , insnMnemonic    = "HADDPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x7C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vhaddps :: X86Insn
i_vhaddps = insn
   { insnDesc        = "Packed single-FP horizontal add"
   , insnMnemonic    = "VHADDPS"
   , insnProperties  = [ MemAlign 16 ]
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x7C
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_hlt :: X86Insn
i_hlt = insn
   { insnDesc        = "Halt"
   , insnMnemonic    = "HLT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xF4
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           }
                       ]
   }

i_hsubpd :: X86Insn
i_hsubpd = insn
   { insnDesc        = "Packed double-FP horizontal subtract"
   , insnMnemonic    = "HSUBPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x7D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vhsubpd :: X86Insn
i_vhsubpd = insn
   { insnDesc        = "Packed double-FP horizontal subtract"
   , insnMnemonic    = "VHSUBPD"
   , insnProperties  = [ MemAlign 16 ]
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x7D
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }


i_hsubps :: X86Insn
i_hsubps = insn
   { insnDesc        = "Packed single-FP horizontal subtract"
   , insnMnemonic    = "HSUBPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x7D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vhsubps :: X86Insn
i_vhsubps = insn
   { insnDesc        = "Packed single-FP horizontal subtract"
   , insnMnemonic    = "VHSUBPS"
   , insnProperties  = [ MemAlign 16 ]
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x7D
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_idiv :: X86Insn
i_idiv = insn
   { insnDesc        = "Signed divide"
   , insnMnemonic    = "IDIV"
   , insnProperties  = [FailOnZero 0]
   , insnFlags       = [Undefined [CF,OF,SF,ZF,AF,PF]]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xF6
                           , legacyOpcodeExt       = Just 7
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regFam RegFamDXAX RW Implicit
                                                     , mgpr RO
                                                     ]
                           }
                       ]
   }

i_imul :: X86Insn
i_imul = insn
   { insnDesc        = "Signed multiply"
   , insnMnemonic    = "IMUL"
   , insnFlags       = [ Modified  [OF,CF]
                       , Undefined [SF,ZF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xF6
                           , legacyOpcodeExt       = Just 5
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regFam RegFamDXAX WO Implicit
                                                     , regAccu RO
                                                     , mgpr RO
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xAF
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr RW Reg
                                                     , mgpr RO
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x69
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr RW Reg
                                                     , mgpr RO
                                                     , immSE
                                                     ]
                           }
                       ]
   }

i_in :: X86Insn
i_in = insn
   { insnDesc        = "Input from port"
   , insnMnemonic    = "IN"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE4
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , NoOperandSize64
                                                     ]
                           , legacyParams          = [ regAccu WO
                                                     , imm8
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xEC
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , NoOperandSize64
                                                     ]
                           , legacyParams          = [ regAccu WO
                                                     , reg R_DX RO Implicit
                                                     ]
                           }

                       ]
   }

i_inc :: X86Insn
i_inc = insn
   { insnDesc        = "Increment by 1"
   , insnMnemonic    = "INC"
   , insnFlags       = [ Modified [OF,SF,ZF,AF,PF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xFE
                           , legacyOpcodeExt       = Just 0
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x40
                           , legacyProperties      = [ LegacyModeSupport
                                                     , Lockable
                                                     ]
                           , legacyParams          = [ gpr RW OpcodeLow3 ]
                           }
                       ]
   }

i_ins :: X86Insn
i_ins = insn
   { insnDesc        = "Input from port to string"
   , insnMnemonic    = "IN"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x6C
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , DefaultAddressSize64
                                                     , NoOperandSize64
                                                     , Repeatable
                                                     ]
                           , legacyParams          = [ mESrDI WO  
                                                     , reg R_DX RO Implicit
                                                     ]
                           }
                       ]
   }

i_insertps :: X86Insn
i_insertps = insn
   { insnDesc        = "Insert packed single precision floating-point value"
   , insnMnemonic    = "INSERTPS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x21
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       ]
   }

i_vinsertps :: X86Insn
i_vinsertps = insn
   { insnDesc        = "Insert packed single precision floating-point value"
   , insnMnemonic    = "VINSERTPS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x21
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_int :: X86Insn
i_int = insn
   { insnDesc        = "Call to interrupt procedure"
   , insnMnemonic    = "INT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xCC
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ constImm 3 ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xCD
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ imm8 ]
                           }
                       ]
   }

i_into :: X86Insn
i_into = insn
   { insnDesc        = "Call to interrupt procedure if overflow"
   , insnMnemonic    = "INTO"
   , insnFlags       = [ Read [OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xCE
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ constImm 3 ]
                           }
                       ]
   }

i_invd :: X86Insn
i_invd = insn
   { insnDesc        = "Invalid internal caches"
   , insnMnemonic    = "INVD"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x08
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Arch Intel486
                                                     ]
                           }
                       ]
   }

i_invlpg :: X86Insn
i_invlpg = insn
   { insnDesc        = "Invalid TLB entry"
   , insnMnemonic    = "INVLPG"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x01
                           , legacyOpcodeExt       = Just 7
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Arch Intel486
                                                     ]
                           , legacyParams          = [ mvoid ]
                           }
                       ]
   }

i_invpcid :: X86Insn
i_invpcid = insn
   { insnDesc        = "Invalid process-context identifier"
   , insnMnemonic    = "INVPCID"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0x82
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension INVPCID
                                                     ]
                           , legacyParams          = [ reg32o64 WO Reg
                                                     , mem128 RO
                                                     ]
                           }
                       ]
   }

i_iret :: X86Insn
i_iret = insn
   { insnDesc        = "Interrupt return"
   , insnMnemonic    = "IRET"
   , insnFlags       = [ Undefined allFlags ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xCF
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           }
                       ]
   }

i_ja :: X86Insn
i_ja = insn
   { insnDesc        = "Jump if above"
   , insnMnemonic    = "JA/JNBE"
   , insnFlags       = [ Read [CF,ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x77
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x87
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jae :: X86Insn
i_jae = insn
   { insnDesc        = "Jump if above or equal"
   , insnMnemonic    = "JAE/JNB/JNC"
   , insnFlags       = [ Read [CF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x73
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x83
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jb :: X86Insn
i_jb = insn
   { insnDesc        = "Jump if below"
   , insnMnemonic    = "JB/JC/JNAE"
   , insnFlags       = [ Read [CF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x72
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x82
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jbe :: X86Insn
i_jbe = insn
   { insnDesc        = "Jump if below or equal"
   , insnMnemonic    = "JBE/JNA"
   , insnFlags       = [ Read [CF,ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x76
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x86
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }


i_jcxz :: X86Insn
i_jcxz = insn
   { insnDesc        = "Jump if rCX is 0"
   , insnMnemonic    = "JCXZ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE3
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regCounter RO
                                                     , rel8
                                                     ]
                           }
                       ]
   }

i_je :: X86Insn
i_je = insn
   { insnDesc        = "Jump if equal"
   , insnMnemonic    = "JE/JZ"
   , insnFlags       = [ Read [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x74
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x84
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jg :: X86Insn
i_jg = insn
   { insnDesc        = "Jump if greater"
   , insnMnemonic    = "JG/JNLE"
   , insnFlags       = [ Read [ZF,SF,OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jge :: X86Insn
i_jge = insn
   { insnDesc        = "Jump if greater or equal"
   , insnMnemonic    = "JGE/JNL"
   , insnFlags       = [ Read [SF,OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jl :: X86Insn
i_jl = insn
   { insnDesc        = "Jump if less"
   , insnMnemonic    = "JL/JNGE"
   , insnFlags       = [ Read [SF,OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jle :: X86Insn
i_jle = insn
   { insnDesc        = "Jump if less or equal"
   , insnMnemonic    = "JLE/JNG"
   , insnFlags       = [ Read [ZF,SF,OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jne :: X86Insn
i_jne = insn
   { insnDesc        = "Jump if not equal"
   , insnMnemonic    = "JNE/JNZ"
   , insnFlags       = [ Read [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x75
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x85
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jno :: X86Insn
i_jno = insn
   { insnDesc        = "Jump if not overflow"
   , insnMnemonic    = "JNO"
   , insnFlags       = [ Read [OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x71
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x81
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jnp :: X86Insn
i_jnp = insn
   { insnDesc        = "Jump if not parity"
   , insnMnemonic    = "JNP/JPO"
   , insnFlags       = [ Read [PF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jns :: X86Insn
i_jns = insn
   { insnDesc        = "Jump if not sign"
   , insnMnemonic    = "JNS"
   , insnFlags       = [ Read [SF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x79
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x89
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jo :: X86Insn
i_jo = insn
   { insnDesc        = "Jump if overflow"
   , insnMnemonic    = "JO"
   , insnFlags       = [ Read [OF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x70
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x80
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jp :: X86Insn
i_jp = insn
   { insnDesc        = "Jump if parity"
   , insnMnemonic    = "JP/JPE"
   , insnFlags       = [ Read [PF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x7A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_js :: X86Insn
i_js = insn
   { insnDesc        = "Jump if sign"
   , insnMnemonic    = "JS"
   , insnFlags       = [ Read [SF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x78
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x88
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                       ]
   }

i_jmp :: X86Insn
i_jmp = insn
   { insnDesc        = "Jump"
   , insnMnemonic    = "JMP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xEB
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel8 ]
                           }
                        , leg
                           { legacyOpcode          = 0xE9
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rel16o32 ]
                           }
                        , leg
                           { legacyOpcode          = 0xFF
                           , legacyOpcodeExt       = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RO ]
                           }
                        , leg
                           { legacyOpcode          = 0xEA
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ ptr16x ]
                           }
                        , leg
                           { legacyOpcode          = 0xFF
                           , legacyOpcodeExt       = Just 5
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ m16x ]
                           }
                       ]
   }

i_lahf :: X86Insn
i_lahf = insn
   { insnDesc        = "Load status flags into AH register"
   , insnMnemonic    = "LAHF"
   , insnFlags       = [ Read [SF,ZF,AF,PF,CF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x9F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension LAHF
                                                     ]
                           , legacyParams          = [ reg R_AH WO Implicit ]
                           }
                       ]
   }

i_lar :: X86Insn
i_lar = insn
   { insnDesc        = "Load access rights"
   , insnMnemonic    = "LAR"
   , insnFlags       = [ Modified [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x0F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr WO Reg
                                                     , mgpr RO
                                                     ]
                           }
                       ]
   }

i_lddqu :: X86Insn
i_lddqu = insn
   { insnDesc        = "Load unaligned integer 128 bits"
   , insnMnemonic    = "LDDQU"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xF0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mem128 RO
                                                     ]
                           }
                       ]
   }

i_vlddqu :: X86Insn
i_vlddqu = insn
   { insnDesc        = "Load unaligned integer 128 bits"
   , insnMnemonic    = "VLDDQU"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xF0
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , m128o256 RO
                                                  ]
                           }
                       ]
   }

i_ldmxcsr :: X86Insn
i_ldmxcsr = insn
   { insnDesc        = "Load MXCSR register"
   , insnMnemonic    = "LDMXCSR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xAE
                           , legacyOpcodeExt       = Just 2
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ mem32 RO ]
                           }
                       ]
   }

i_vldmxcsr :: X86Insn
i_vldmxcsr = insn
   { insnDesc        = "Load MXCSR register"
   , insnMnemonic    = "VLDMXCSR"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xAE
                           , vexOpcodeExt       = Just 2
                           , vexLW              = L0_WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ mem32 RO ]
                           }
                       ]
   }


i_ldfarptr :: X86Insn
i_ldfarptr = insn
   { insnDesc        = "Load far pointer"
   , insnMnemonic    = "LDS/LES/LFS/LGS/LSS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xC5
                           , legacyProperties      = [ LegacyModeSupport
                                                     , DefaultSegment R_DS
                                                     ]
                           , legacyParams          = [ gpr RO Reg
                                                     , m16x
                                                     ]
                           }
                       ,  leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xC4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , DefaultSegment R_ES
                                                     ]
                           , legacyParams          = [ gpr RO Reg
                                                     , m16x
                                                     ]
                           }
                       ,  leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xB2
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , DefaultSegment R_SS
                                                     ]
                           , legacyParams          = [ gpr RO Reg
                                                     , m16x
                                                     ]
                           }
                       ,  leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xB4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , DefaultSegment R_FS
                                                     ]
                           , legacyParams          = [ gpr RO Reg
                                                     , m16x
                                                     ]
                           }
                       ,  leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xB5
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , DefaultSegment R_GS
                                                     ]
                           , legacyParams          = [ gpr RO Reg
                                                     , m16x
                                                     ]
                           }
                       ]
   }

i_lea :: X86Insn
i_lea = insn
   { insnDesc        = "Load effective address"
   , insnMnemonic    = "LEA"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x8D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr WO Reg
                                                     , mvoid
                                                     ]
                           }
                       ]
   }

i_leave :: X86Insn
i_leave = insn
   { insnDesc        = "High level procedure exit"
   , insnMnemonic    = "LEAVE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xC9
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regStackPtr WO Implicit
                                                     , regBasePtr  RW Implicit
                                                     ]
                           }
                       ]
   }

i_lfence :: X86Insn
i_lfence = insn
   { insnDesc        = "Load fence"
   , insnMnemonic    = "LFENCE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xAE
                           , legacyOpcodeExt       = Just 5
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           }
                       ]
   }

i_lgdt :: X86Insn
i_lgdt = insn
   { insnDesc        = "Load global descriptor table register"
   , insnMnemonic    = "LGDT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x01
                           , legacyOpcodeExt       = Just 2
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mdt RO ]
                           }
                       ]
   }

i_lidt :: X86Insn
i_lidt = insn
   { insnDesc        = "Load interrupt descriptor table register"
   , insnMnemonic    = "LIDT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x01
                           , legacyOpcodeExt       = Just 3
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mdt RO ]
                           }
                       ]
   }

i_lldt :: X86Insn
i_lldt = insn
   { insnDesc        = "Load local descriptor table register"
   , insnMnemonic    = "LLDT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x00
                           , legacyOpcodeExt       = Just 2
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     -- TODO: not supported in
                                                     -- real/virtual mode
                                                     ]
                           , legacyParams          = [ rm16 RO ]
                           }
                       ]
   }

i_lmsw :: X86Insn
i_lmsw = insn
   { insnDesc        = "Load machine status word"
   , insnMnemonic    = "LMSW"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x01
                           , legacyOpcodeExt       = Just 6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ rm16 RO ]
                           }
                       ]
   }

i_lods :: X86Insn
i_lods = insn
   { insnDesc        = "Load string"
   , insnMnemonic    = "LODS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xAC
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regAccu WO
                                                     , mDSrSI RO
                                                     ]
                           }
                       ]
   }

i_loop :: X86Insn
i_loop = insn
   { insnDesc        = "Loop according to rCX counter"
   , insnMnemonic    = "LOOP"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE2
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regCounter RW
                                                     , rel8
                                                     ]
                           }
                       ]
   }


i_loope :: X86Insn
i_loope = insn
   { insnDesc        = "Loop according to rCX counter and ZF"
   , insnMnemonic    = "LOOPE"
   , insnFlags       = [ Read [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regCounter RW
                                                     , rel8
                                                     ]
                           }
                       ]
   }


i_loopne :: X86Insn
i_loopne = insn
   { insnDesc        = "Loop according to rCX counter and ZF"
   , insnMnemonic    = "LOOPNE"
   , insnFlags       = [ Read [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regCounter RW
                                                     , rel8
                                                     ]
                           }
                       ]
   }


i_lsl :: X86Insn
i_lsl = insn
   { insnDesc        = "Load segment limit"
   , insnMnemonic    = "LSL"
   , insnFlags       = [ Modified [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xE0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr RW Reg
                                                     , mgpr RO
                                                     ]
                           }
                       ]
   }


i_ltr :: X86Insn
i_ltr = insn
   { insnDesc        = "Load task register"
   , insnMnemonic    = "LTR"
   , insnFlags       = [ Modified [ZF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x00
                           , legacyOpcodeExt       = Just 3
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     -- TODO: invalid in
                                                     -- real/virtual modes
                                                     ]
                           , legacyParams          = [ rm16 RO ]
                           }
                       ]
   }

i_maskmovdqu :: X86Insn
i_maskmovdqu = insn
   { insnDesc        = "Store selected bytes of double quadword"
   , insnMnemonic    = "MASKMOVDQU"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xF7
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , vec128 RO RM
                                                     , mDSrDI RO
                                                     ]
                           }
                       ]
   }

i_vmaskmovdqu :: X86Insn
i_vmaskmovdqu = insn
   { insnDesc        = "Store selected bytes of double quadword"
   , insnMnemonic    = "VMASKMOVDQU"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0xF7
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128 RO RM
                                                     , mDSrDI RO
                                                     ]
                           }
                       ]
   }


i_maskmovq :: X86Insn
i_maskmovq = insn
   { insnDesc        = "Store selected bytes of quadword"
   , insnMnemonic    = "MASKMOVQ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xF7
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ vec64 RW Reg
                                                     , vec64 RO RM
                                                     , mDSrDI RO
                                                     ]
                           }
                       ]
   }

i_maxpd :: X86Insn
i_maxpd = insn
   { insnDesc        = "Return maximum packed double-precision floating-point values"
   , insnMnemonic    = "MAXPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmaxpd :: X86Insn
i_vmaxpd = insn
   { insnDesc        = "Return maximum packed double-precision floating-point values"
   , insnMnemonic    = "VMAXPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x5F
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_maxps :: X86Insn
i_maxps = insn
   { insnDesc        = "Return maximum packed single-precision floating-point values"
   , insnMnemonic    = "MAXPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmaxps :: X86Insn
i_vmaxps = insn
   { insnDesc        = "Return maximum packed single-precision floating-point values"
   , insnMnemonic    = "VMAXPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5F
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_maxsd :: X86Insn
i_maxsd = insn
   { insnDesc        = "Return maximum scalar double-precision floating-point values"
   , insnMnemonic    = "MAXSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vmaxsd :: X86Insn
i_vmaxsd = insn
   { insnDesc        = "Return maximum scalar double-precision floating-point values"
   , insnMnemonic    = "VMAXSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5F
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_maxss :: X86Insn
i_maxss = insn
   { insnDesc        = "Return maximum scalar single-precision floating-point values"
   , insnMnemonic    = "MAXSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vmaxss :: X86Insn
i_vmaxss = insn
   { insnDesc        = "Return maximum scalar single-precision floating-point values"
   , insnMnemonic    = "VMAXSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5F
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_mfence :: X86Insn
i_mfence = insn
   { insnDesc        = "Memory fence"
   , insnMnemonic    = "MFENCE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xAE
                           , legacyOpcodeExt       = Just 6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           }
                       ]
   }

i_minpd :: X86Insn
i_minpd = insn
   { insnDesc        = "Return minimum packed double-precision floating-point values"
   , insnMnemonic    = "MINPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vminpd :: X86Insn
i_vminpd = insn
   { insnDesc        = "Return minimum packed double-precision floating-point values"
   , insnMnemonic    = "VMINPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x5D
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_minps :: X86Insn
i_minps = insn
   { insnDesc        = "Return minimum packed single-precision floating-point values"
   , insnMnemonic    = "MINPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vminps :: X86Insn
i_vminps = insn
   { insnDesc        = "Return minimum packed single-precision floating-point values"
   , insnMnemonic    = "VMINPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5D
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_minsd :: X86Insn
i_minsd = insn
   { insnDesc        = "Return minimum scalar double-precision floating-point values"
   , insnMnemonic    = "MINSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vminsd :: X86Insn
i_vminsd = insn
   { insnDesc        = "Return minimum scalar double-precision floating-point values"
   , insnMnemonic    = "VMINSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5D
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_minss :: X86Insn
i_minss = insn
   { insnDesc        = "Return minimum scalar single-precision floating-point values"
   , insnMnemonic    = "MINSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x5D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vminss :: X86Insn
i_vminss = insn
   { insnDesc        = "Return minimum scalar single-precision floating-point values"
   , insnMnemonic    = "VMINSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x5D
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_monitor :: X86Insn
i_monitor = insn
   { insnDesc        = "Set up monitor address"
   , insnMnemonic    = "MONITOR"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0x01
                           , legacyOpcodeFullExt = Just 0xC8
                           , legacyProperties    = [ LegacyModeSupport
                                                   , LongModeSupport
                                                   , Extension MONITOR
                                                   ]
                           , legacyParams          = [ reg R_ECX RO Implicit
                                                     , reg R_EDX RO Implicit
                                                     , regFam RegFamDSrAX RO Implicit
                                                     ]
                           }
                       ]
   }

i_mov :: X86Insn
i_mov = insn
   { insnDesc        = "Move"
   , insnMnemonic    = "MOV"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xA0
                           , legacyNoForce8bit     = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ regAccu RW
                                                     , op    RO    T_MOffs  Imm
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x88
                           , legacyNoForce8bit     = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr WO
                                                     , gpr RO Reg
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x8C
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr WO
                                                     , op RO (T_Reg RegSegment) Reg
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xB0
                           , legacyNoForce8bit     = Just 3
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr RW OpcodeLow3
                                                     , immOp
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xC6
                           , legacyOpcodeExt       = Just 0
                           , legacyNoForce8bit     = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mgpr RW
                                                     , immSE
                                                     ]
                           }
                       ]
   }

i_movcr :: X86Insn
i_movcr = insn
   { insnDesc        = "Move control register"
   , insnMnemonic    = "MOV"
   , insnFlags       = [ Undefined [OF,SF,ZF,AF,PF,CF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x20
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op WO (T_Reg Reg32o64)   RM
                                                     , op RO (T_Reg RegControl) Reg
                                                     ]
                           }
                       ]
   }


i_movdr :: X86Insn
i_movdr = insn
   { insnDesc        = "Move debug register"
   , insnMnemonic    = "MOV"
   , insnFlags       = [ Undefined [OF,SF,ZF,AF,PF,CF] ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x21
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    WO (T_Reg Reg32o64) RM
                                                     , op    RO (T_Reg RegDebug) Reg
                                                     ]
                           }
                       ]
   }



i_movapd :: X86Insn
i_movapd = insn
   { insnDesc        = "Move aligned packed double-precision floating-point values"
   , insnMnemonic    = "MOVAPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x28
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmovapd :: X86Insn
i_vmovapd = insn
   { insnDesc        = "Move aligned packed double-precision floating-point values"
   , insnMnemonic    = "VMOVAPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x28
                           , vexReversable         = Just 0
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_movaps :: X86Insn
i_movaps = insn
   { insnDesc        = "Move aligned packed single-precision floating-point values"
   , insnMnemonic    = "MOVAPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x28
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmovaps :: X86Insn
i_vmovaps = insn
   { insnDesc        = "Move aligned packed single-precision floating-point values"
   , insnMnemonic    = "VMOVAPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x28
                           , vexLW              = WIG
                           , vexReversable      = Just 0
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_movbe :: X86Insn
i_movbe = insn
   { insnDesc        = "Move data after swapping bytes"
   , insnMnemonic    = "MOVBE"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0xF0
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ gpr WO Reg
                                                     , mem RO
                                                     ]
                           }
                       ]
   }

i_movdq :: X86Insn
i_movdq = insn
   { insnDesc        = "Move doubleword/quadword"
   , insnMnemonic    = "MOVD/MOVQ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x6E
                           , legacyReversable      = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ vec64 WO Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       , leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x6E
                           , legacyReversable      = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x6E
                           , vexReversable         = Just 4
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , rm32o64 RO
                                                     ]
                           }
                       ]
   }

i_movddup :: X86Insn
i_movddup = insn
   { insnDesc        = "Move one double-FP and duplicate"
   , insnMnemonic    = "MOVDDUP"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x12
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x12
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvecEven64 RO
                                                     ]
                           }
                       ]
   }

i_movdqa :: X86Insn
i_movdqa = insn
   { insnDesc        = "Move aligned doubleword/quadword"
   , insnMnemonic    = "MOVDQA/VMOVDQA"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x6F
                           , legacyReversable      = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x6F
                           , vexReversable         = Just 4
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_movdqu :: X86Insn
i_movdqu = insn
   { insnDesc        = "Move unaligned doubleword/quadword"
   , insnMnemonic    = "MOVDQU/VMOVDQU"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x6F
                           , legacyReversable      = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x6F
                           , vexReversable         = Just 4
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_movdq2q :: X86Insn
i_movdq2q = insn
   { insnDesc        = "Move quadword from XMM to MMX register"
   , insnMnemonic    = "MOVDQ2Q"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xD6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ vec64 WO Reg
                                                     , vec128low64 RO RM
                                                     ]
                           }
                       ]
   }

i_movhlps :: X86Insn
i_movhlps = insn
   { insnDesc        = "Move packed single-precision FP values high to low"
   , insnMnemonic    = "MOVHLPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x12
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128low64 WO Reg
                                                     , vec128high64 RO RM
                                                     ]
                           }
                        ]
   }

i_vmovhlps :: X86Insn
i_vmovhlps = insn
   { insnDesc        = "Move packed single-precision FP values high to low"
   , insnMnemonic    = "VMOVHLPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x12
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128low64 RO Vvvv
                                                     , vec128high64 RO RM
                                                     ]
                           }
                       ]
   }



i_movhpd :: X86Insn
i_movhpd = insn
   { insnDesc        = "Move high packed double-precision FP value"
   , insnMnemonic    = "MOVHPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x16
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128high64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x17
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem64 WO
                                                     , vec128high64 RO Reg
                                                     ]
                           }
                       ]
   }

i_vmovhpd :: X86Insn
i_vmovhpd = insn
   { insnDesc        = "Move high packed double-precision FP value"
   , insnMnemonic    = "VMOVHPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x16
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128low64 RO Vvvv
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }


i_movhps :: X86Insn
i_movhps = insn
   { insnDesc        = "Move high packed single-precision FP values"
   , insnMnemonic    = "MOVHPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x16
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128high64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       , vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x17
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem64 WO
                                                     , vec128high64 RO Reg
                                                     ]
                           }
                       ]
   }

i_vmovhps :: X86Insn
i_vmovhps = insn
   { insnDesc        = "Move high packed singke-precision FP values"
   , insnMnemonic    = "VMOVHPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x16
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128low64 RO Vvvv
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }


i_movlpd :: X86Insn
i_movlpd = insn
   { insnDesc        = "Move low packed double-precision FP value"
   , insnMnemonic    = "MOVLPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x12
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128low64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x13
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem64 WO
                                                     , vec128low64 RO Reg
                                                     ]
                           }
                       ]
   }

i_vmovlpd :: X86Insn
i_vmovlpd = insn
   { insnDesc        = "Move low packed double-precision FP value"
   , insnMnemonic    = "VMOVLPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x12
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128high64 RO Vvvv
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }


i_movlps :: X86Insn
i_movlps = insn
   { insnDesc        = "Move low packed single-precision FP values"
   , insnMnemonic    = "MOVLPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x12
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128low64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       , vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x13
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem64 WO
                                                     , vec128low64 RO Reg
                                                     ]
                           }
                       ]
   }

i_vmovlps :: X86Insn
i_vmovlps = insn
   { insnDesc        = "Move low packed singke-precision FP values"
   , insnMnemonic    = "VMOVLPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x12
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128high64 RO Vvvv
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }

i_movlhps :: X86Insn
i_movlhps = insn
   { insnDesc        = "Move packed single-precision FP values low ti high"
   , insnMnemonic    = "MOVLHPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x16
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128high64 WO Reg
                                                     , vec128low64 RO RM
                                                     ]
                           }
                        ]
   }

i_vmovlhps :: X86Insn
i_vmovlhps = insn
   { insnDesc        = "Move packed single-precision FP values low to high"
   , insnMnemonic    = "VMOVLHPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x16
                           , vexLW                 = L0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128high64 RO Vvvv
                                                     , vec128low64 RO RM
                                                     ]
                           }
                       ]
   }


i_movmskpd :: X86Insn
i_movmskpd = insn
   { insnDesc        = "Move packed double-precision FP sign mask"
   , insnMnemonic    = "MOVMSKPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x50
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     , DefaultOperandSize64
                                                     ]
                           , legacyParams          = [ gpr WO Reg
                                                     , vec128 RO RM
                                                     ]
                           }
                        ]
   }

i_vmovmskpd :: X86Insn
i_vmovmskpd = insn
   { insnDesc        = "Move packed double-precision FP sign mask"
   , insnMnemonic    = "VMOVMSKPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x50
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     , DefaultOperandSize64
                                                     ]
                           , vexParams             = [ gpr WO Reg
                                                     , vec128o256 RO RM
                                                     ]
                           }
                       ]
   }


i_movmskps :: X86Insn
i_movmskps = insn
   { insnDesc        = "Move packed single-precision FP sign mask"
   , insnMnemonic    = "MOVMSKPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x50
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     , DefaultOperandSize64
                                                     ]
                           , legacyParams          = [ gpr WO Reg
                                                     , vec128 RO RM
                                                     ]
                           }
                        ]
   }

i_vmovmskps :: X86Insn
i_vmovmskps = insn
   { insnDesc        = "Move packed single-precision FP sign mask"
   , insnMnemonic    = "VMOVMSKPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x50
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     , DefaultOperandSize64
                                                     ]
                           , vexParams             = [ gpr WO Reg
                                                     , vec128o256 RO RM
                                                     ]
                           }
                       ]
   }

i_movntdqa :: X86Insn
i_movntdqa = insn
   { insnDesc        = "Move aligned doubleword/quadword non temporal"
   , insnMnemonic    = "MOVNTDQA/VMOVNTDQA"
   , insnProperties  = [ MemAlignDefault ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F38
                           , legacyOpcode          = 0x2A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mem128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 2
                           , vexOpcode             = 0x2A
                           , vexLW                 = L0_WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , mem128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 2
                           , vexOpcode             = 0x2A
                           , vexLW                 = L1_WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX2
                                                     ]
                           , vexParams             = [ vec256 WO Reg
                                                     , mem256 RO
                                                     ]
                           }
                       ]
   }


i_movntdq :: X86Insn
i_movntdq = insn
   { insnDesc        = "Store double quadword non temporal"
   , insnMnemonic    = "MOVNTDQ/VMOVNTDQ"
   , insnProperties  = [ MemAlignDefault ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xE7
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ mem128 WO
                                                     , vec128 RO Reg
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0xE7
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem128o256 WO
                                                     , vec128o256 RO Reg
                                                     ]
                           }
                       ]
   }


i_movnti :: X86Insn
i_movnti = insn
   { insnDesc        = "Store doubleword non temporal"
   , insnMnemonic    = "MOVNTI"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xC3
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ mem32o64 WO
                                                     , reg32o64 RO Reg
                                                     ]
                           }
                       ]
   }

i_movntpd :: X86Insn
i_movntpd = insn
   { insnDesc        = "Move aligned packed double-precision FP values non temporal"
   , insnMnemonic    = "(V)MOVNTPD"
   , insnProperties  = [ MemAlignDefault ]
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ mem128 WO
                                                     , vec128 RO Reg
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x2B
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ mem128o256 WO
                                                     , vec128o256 RO Reg
                                                     ]
                           }
                       ]
   }

i_movntps :: X86Insn
i_movntps = insn
   { insnDesc        = "Move aligned packed single-precision FP values non temporal"
   , insnMnemonic    = "MOVNTPS"
   , insnProperties  = [ MemAlignDefault ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x2B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ mem128 WO
                                                     , vec128 RO Reg
                                                     ]
                           }
                       , vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x2B
                           , vexLW              = WIG
                           , vexReversable      = Just 0
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ mem128o256 WO
                                                  , vec128o256 RO Reg
                                                  ]
                           }
                       ]
   }

i_movntq :: X86Insn
i_movntq = insn
   { insnDesc        = "Store quadword non temporal"
   , insnMnemonic    = "MOVNTQ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xE7
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ mem64 WO
                                                     , vec64 RO Reg
                                                     ]
                           }
                       ]
   }

i_movq :: X86Insn
i_movq = insn
   { insnDesc        = "Move quadword"
   , insnMnemonic    = "MOVQ"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x6F
                           , legacyReversable      = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ vec64 WO Reg
                                                     , mvec64 RO
                                                     ]
                           }
                       , leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x7E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128low64 WO Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       , leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xD6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ mvec128low64 WO
                                                     , vec128low64 RO Reg
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x7E
                           , vexLW              = L0_WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128low64 WO Reg
                                                  , mvec128low64 RO
                                                  ]
                           }
                       , vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0xD6
                           , vexLW              = L0_WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ mvec128low64 WO
                                                  , vec128low64 RO Reg
                                                  ]
                           }
                       ]
   }

i_movq2dq :: X86Insn
i_movq2dq = insn
   { insnDesc        = "Move quadword from MMX to XMM register"
   , insnMnemonic    = "MOVQ2DQ"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xD6
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension MMX
                                                     ]
                           , legacyParams          = [ vec128low64 WO Reg
                                                     , vec64 RO RM
                                                     ]
                           }
                       ]
   }

i_movs :: X86Insn
i_movs = insn
   { insnDesc        = "Move string"
   , insnMnemonic    = "MOVS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xA4
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ mDSrSI RO
                                                  , mESrDI WO
                                                  ]
                           }
                       ]
   }

i_movsd :: X86Insn
i_movsd = insn
   { insnDesc        = "Move scalar double-precision floating-point values"
   , insnMnemonic    = "MOVSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0xF2
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0x10
                           , legacyReversable         = Just 0
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE2
                                                        ]
                           , legacyParams             = [ vec128low64 WO Reg
                                                        , mvec128low64 RO
                                                        ]
                           }
                       ]
   }

i_vmovsd :: X86Insn
i_vmovsd = insn
   { insnDesc        = "Move scalar double-precision floating-point values"
   , insnMnemonic    = "VMOVSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x10
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , vec128o256 RO RM
                                                     ]
                           }
                       ,  vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x11
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128high64 RO Vvvv
                                                     , vec128low64 RO RM
                                                     ]
                           }
                       ,  vex
                           { vexMandatoryPrefix    = Just 0xF2
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x10
                           , vexLW                 = LWIG
                           , vexReversable         = Just 0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128low64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }

i_movshdup :: X86Insn
i_movshdup = insn
   { insnDesc        = "Move packed single-FP high and duplicate"
   , insnMnemonic    = "MOVSHDUP"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x16
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x16
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }


i_movsldup :: X86Insn
i_movsldup = insn
   { insnDesc        = "Move packed single-FP low and duplicate"
   , insnMnemonic    = "MOVSLDUP"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x12
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE3
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 1
                           , vexOpcode             = 0x12
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_movss :: X86Insn
i_movss = insn
   { insnDesc        = "Move scalar single-precision floating-point values"
   , insnMnemonic    = "MOVSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix    = Just 0xF3
                           , legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0x10
                           , legacyReversable         = Just 0
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        , Extension SSE
                                                        ]
                           , legacyParams             = [ vec128low32 WO Reg
                                                        , mvec128low32 RO
                                                        ]
                           }
                       ]
   }

i_vmovss :: X86Insn
i_vmovss = insn
   { insnDesc        = "Move scalar single-precision floating-point values"
   , insnMnemonic    = "VMOVSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x10
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , vec128o256 RO RM
                                                     ]
                           }
                       ,  vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x11
                           , vexLW                 = LWIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128 WO Reg
                                                     , vec128high64 RO Vvvv
                                                     , vec128low64 RO RM
                                                     ]
                           }
                       ,  vex
                           { vexMandatoryPrefix    = Just 0xF3
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x10
                           , vexLW                 = LWIG
                           , vexReversable         = Just 0
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128low64 WO Reg
                                                     , mem64 RO
                                                     ]
                           }
                       ]
   }

i_movsx :: X86Insn
i_movsx = insn
   { insnDesc        = "Move with sign-extension"
   , insnMnemonic    = "MOVSX"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xBE
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        ]
                           , legacyParams             = [ gpr WO Reg
                                                        , rm8 RO
                                                        ]
                           }
                       , leg
                           { legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xBF
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        ]
                           , legacyParams             = [ reg32o64 WO Reg
                                                        , rm16 RO
                                                        ]
                           }
                       , leg
                           { legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0x63
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        ]
                           , legacyParams             = [ reg32o64 WO Reg
                                                        , rm32 RO
                                                        ]
                           }
                       ]
   }


i_movupd :: X86Insn
i_movupd = insn
   { insnDesc        = "Move unaligned packed double-precision floating-point values"
   , insnMnemonic    = "MOVUPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x10
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmovupd :: X86Insn
i_vmovupd = insn
   { insnDesc        = "Move unaligned packed double-precision floating-point values"
   , insnMnemonic    = "VMOVUPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x10
                           , vexReversable         = Just 0
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_movups :: X86Insn
i_movups = insn
   { insnDesc        = "Move unaligned packed single-precision floating-point values"
   , insnMnemonic    = "MOVUPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x10
                           , legacyReversable      = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmovups :: X86Insn
i_vmovups = insn
   { insnDesc        = "Move unaligned packed single-precision floating-point values"
   , insnMnemonic    = "VMOVUPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x10
                           , vexLW              = WIG
                           , vexReversable      = Just 0
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_movzx :: X86Insn
i_movzx = insn
   { insnDesc        = "Move with zerp-extend"
   , insnMnemonic    = "MOVZX"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xB6
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        ]
                           , legacyParams             = [ gpr WO Reg
                                                        , rm8 RO
                                                        ]
                           }
                       , leg
                           { legacyOpcodeMap          = Map0F
                           , legacyOpcode             = 0xB7
                           , legacyProperties         = [ LegacyModeSupport
                                                        , LongModeSupport
                                                        ]
                           , legacyParams             = [ reg32o64 WO Reg
                                                        , rm16 RO
                                                        ]
                           }
                       ]
   }

i_mpsadbw :: X86Insn
i_mpsadbw = insn
   { insnDesc        = "Compute multiple packed sums of absolute difference"
   , insnMnemonic    = "MPSADBW"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F3A
                           , legacyOpcode          = 0x42
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE4_1
                                                     ]
                           , legacyParams          = [ vec128 WO Reg
                                                     , mvec128 RO
                                                     , imm8
                                                     ]
                           }
                       , vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x42
                           , vexLW              = L0_WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128 RO
                                                  , imm8
                                                  ]
                           }
                       , vex
                           { vexMandatoryPrefix = Just 0x66
                           , vexOpcodeMap       = MapVex 0x03
                           , vexOpcode          = 0x42
                           , vexLW              = L1_WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX2
                                                  ]
                           , vexParams          = [ vec256 WO Reg
                                                  , vec256 RO Vvvv
                                                  , mvec256 RO
                                                  , imm8
                                                  ]
                           }
                       ]
   }

i_mul :: X86Insn
i_mul = insn
   { insnDesc        = "Unsigned multiply"
   , insnMnemonic    = "MUL"
   , insnFlags       = [ Modified [OF,CF]
                       , Undefined [SF,ZF,AF,PF]
                       ]
   , insnEncodings   = [ leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xF6
                           , legacyNoForce8bit  = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ regFam RegFamDXAX RW Implicit
                                                  , regAccu RO
                                                  , mgpr RO
                                                  ]
                           }
                       ]
   }

i_mulpd :: X86Insn
i_mulpd = insn
   { insnDesc        = "Multiply packed double-precision floating-point values"
   , insnMnemonic    = "MULPD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0x66
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x59
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmulpd :: X86Insn
i_vmulpd = insn
   { insnDesc        = "Multiply packed double-precision floating-point values"
   , insnMnemonic    = "VMULPD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix    = Just 0x66
                           , vexOpcodeMap          = MapVex 0x01
                           , vexOpcode             = 0x59
                           , vexLW                 = WIG
                           , vexProperties         = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension AVX
                                                     ]
                           , vexParams             = [ vec128o256 WO Reg
                                                     , vec128o256 RO Vvvv
                                                     , mvec128o256 RO
                                                     ]
                           }
                       ]
   }

i_mulps :: X86Insn
i_mulps = insn
   { insnDesc        = "Multiply packed single-precision floating-point values"
   , insnMnemonic    = "MULPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x59
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128  RW Reg
                                                     , mvec128 RO
                                                     ]
                           }
                       ]
   }

i_vmulps :: X86Insn
i_vmulps = insn
   { insnDesc        = "Multiply packed single-precision floating-point values"
   , insnMnemonic    = "VMULPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x59
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128o256 WO Reg
                                                  , vec128o256 RO Vvvv
                                                  , mvec128o256 RO
                                                  ]
                           }
                       ]
   }

i_mulsd :: X86Insn
i_mulsd = insn
   { insnDesc        = "Multiply scalar double-precision floating-point values"
   , insnMnemonic    = "MULSD"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF2
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x59
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE2
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low64 RO
                                                     ]
                           }
                       ]
   }

i_vmulsd :: X86Insn
i_vmulsd = insn
   { insnDesc        = "Multiply scalar double-precision floating-point values"
   , insnMnemonic    = "VMULSD"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF2
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x59
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low64 RO
                                                  ]
                           }
                       ]
   }

i_mulss :: X86Insn
i_mulss = insn
   { insnDesc        = "Multiply scalar single-precision floating-point values"
   , insnMnemonic    = "MULSS"
   , insnEncodings   = [ leg
                           { legacyMandatoryPrefix = Just 0xF3
                           , legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x59
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ vec128 RW Reg
                                                     , mvec128low32 RO
                                                     ]
                           }
                       ]
   }

i_vmulss :: X86Insn
i_vmulss = insn
   { insnDesc        = "Multiply scalar single-precision floating-point values"
   , insnMnemonic    = "VMULSS"
   , insnEncodings   = [ vex
                           { vexMandatoryPrefix = Just 0xF3
                           , vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x59
                           , vexLW              = LWIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ vec128 WO Reg
                                                  , vec128 RO Vvvv
                                                  , mvec128low32 RO
                                                  ]
                           }
                       ]
   }

i_mulx :: X86Insn
i_mulx = insn
   { insnDesc           = "Unsigned multiply without affecting flags"
   , insnMnemonic       = "MULX"
   , insnEncodings      = [ vex
                              { vexMandatoryPrefix = Just 0xF2
                              , vexOpcodeMap       = MapVex 0x02
                              , vexOpcode          = 0xF6
                              , vexLW              = L0
                              , vexProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension BMI2
                                                     ]
                              , vexParams          = [ reg32o64 WO Reg
                                                     , reg32o64 WO Vvvv
                                                     , rm32o64 RO
                                                     , rDX RO
                                                     ]
                              }
                          ]
   }

i_mwait :: X86Insn
i_mwait = insn
   { insnDesc        = "Monitor wait"
   , insnMnemonic    = "MWAIT"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap     = Map0F
                           , legacyOpcode        = 0x01
                           , legacyOpcodeFullExt = Just 0xC9
                           , legacyProperties    = [ LegacyModeSupport
                                                   , LongModeSupport
                                                   , Extension MONITOR
                                                   ]
                           , legacyParams          = [ reg R_ECX RO Implicit
                                                     , reg R_EAX RO Implicit
                                                     ]
                           }
                       ]
   }
