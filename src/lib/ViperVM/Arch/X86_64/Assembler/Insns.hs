module ViperVM.Arch.X86_64.Assembler.Insns
   ( X86Insn(..)
   , X86Arch(..)
   , X86Extension(..)
   , Properties(..)
   , Flag(..)
   , FlagOp(..)
   , OperandType(..)
   , OperandEnc(..)
   , Operand(..)
   , AccessMode(..)
   , Encoding(..)
   , LegEnc(..)
   , isLegacyEncoding
   , instructions
   , requireModRM
   , OpcodeMap(..)
   , Encoding (..)
   , LegEnc (..)
   , LegacyOpcodeFields(..)
   )
where

import Data.Word
import Data.Bits
import Data.List (groupBy)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List ((\\), foldl')
import Data.Tree
import qualified Data.Vector as V

data X86Insn = X86Insn
   { iDesc        :: String
   , iMnemonic    :: String
   , iProperties  :: [Properties]
   , iFlags       :: [FlagOp Flag]
   , iEncoding    :: [Encoding]
   } deriving (Show)

data Properties
   = LongMode                 -- ^ Supported in 64 bit mode
   | Legacy                   -- ^ Supported in legacy/compatibility mode
   | FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | FPUPop Int               -- ^ Pop the FPU register if the bit is set (valid if
                              --   the target is not ST0). If >= 8, then bit is in OpExt
   | FPUMemFormat             -- ^ Bits 1 and 2 of the opcode indicate the memory format of the memory operand
                              --       00 -> 32-bit FP
                              --       10 -> 64-bit FP
                              --       01 -> 32-bit int
                              --       11 -> 16-bit int
   -- encoding
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand in used)
   | DoubleSizable            -- ^ Default size is 32+32 (a pair of registers is used). Can be extended to 64+64 with Rex.W
   | MandatoryPrefix Word8    -- ^ Mandatory prefix
   | OpExt Word8              -- ^ Opcode extension in the ModRM.reg field
   | SignExtendableImm8 Int   -- ^ Used in conjunction with a set Sizable bit.
                              --   Imm8 operand is used and sign-extended if the
                              --   given bit is set
   | Sizable Int              -- ^ Operand size is 8 if the given bit is unset in the
                              --   opcode. Otherwise, the size is defined by operand-size
                              --   prefix and REX.W bit
   | Reversable Int           -- ^ Args are reversed if the given bit is set in the opcode.
                              --   For x87 instructions, it is only valid if both operands are registers
   deriving (Show,Eq)

data X86Arch
   = Intel486
   | IntelPentium
   | IntelP6
   deriving (Show,Eq)

data X86Extension
   = ADX             -- ^ ADX extension
   | AVX             -- ^ AVX extension
   | SSE             -- ^ SSE extension
   | SSE2            -- ^ SSE2 extension
   | SSE3            -- ^ SSE3 extension
   | SSE4_1          -- ^ SSE4.1 extension
   | AES             -- ^ AES extension
   | BMI1            -- ^ BMI1 extension
   | BMI2            -- ^ BMI2 extension
   | SMAP            -- ^ Supervisor Mode Access Prevention (SMAP)
   | CLFLUSH         -- ^ CLFLUSH instruction
   | CX8             -- ^ CMPXCHG8B instruction
   | FPU             -- ^ x87 instructions
   deriving (Show,Eq)

data FlagOp a
   = One   [a]  -- ^ Set flag to 1
   | Zero  [a]  -- ^ Set flag to 0
   | Set   [a]  -- ^ Set flag depending on the result
   | Undef [a]  -- ^ Flag is undefined after the operation
   | Read  [a]  -- ^ Flag read by the instruction
   deriving (Show,Eq)

data Flag
   -- Status flag
   = CF     -- ^ Carry flag
   | PF     -- ^ Parity flag
   | AF     -- ^ Adjust flag
   | ZF     -- ^ Zero flag
   | SF     -- ^ Sign flag
   | TF     -- ^ Trap flag
   | OF     -- ^ Overflow flag

   -- Control flags
   | DF     -- ^ Direction flag
   | IF     -- ^ Interrupt flag
   | AC     -- ^ Alignment check
   deriving (Show,Bounded,Enum,Eq)

allFlags :: [Flag]
allFlags = [minBound .. maxBound] \\ [AC,DF,IF]

data OperandEnc
   = E_ModRM      -- ^ Operand stored in ModRM.rm
   | E_ModReg     -- ^ Operand stored in ModRM.reg
   | E_Imm        -- ^ Operand stored in immediate bytes
   | E_Imm8_7_4   -- ^ Operand stored in bits [7:4] of the immediate byte
   | E_Imm8_3_0   -- ^ Operand stored in bits [3:0] of the immediate byte
   | E_Implicit   -- ^ Implicit
   | E_VexV       -- ^ Operand stored in Vex.vvvv field
   | E_OpReg      -- ^ Operand stored in opcode 3 last bits
   deriving (Show)

data OperandType
   -- Immediates
   = T_Imm8       -- ^ Word8 immediate
   | T_Imm16      -- ^ Word16 immediate
   | T_Imm        -- ^ Variable sized immediate
   | T_REL_16_32  -- ^ Relative displacement (16-bit invalid in 64-bit mode)
   | T_PTR_16_16  -- ^ Absolute address
   | T_PTR_16_32  -- ^ Absolute address

   -- General purpose registers
   | T_R          -- ^ General purpose register
   | T_R16        -- ^ 16-bit general purpose register
   | T_R32        -- ^ 32-bit general purpose register
   | T_RM         -- ^ Register or memory
   | T_RM16       -- ^ 16-bit general purpose register or memory
   | T_RM32       -- ^ 32-bit general purpose register or memory
   | T_RM16_32    -- ^ 16- or 32-bit general purpose register or memory
   | T_RM32_64    -- ^ 32- or 64-bit general purpose register or memory
   | T_RM16_32_64 -- ^ 16-, 32- or 64-bit general purpose register or memory
   | T_RM64       -- ^ 64-bit general purpose register or memory
   | T_R16_32     -- ^ 16- or 32-bit general purpose register
   | T_R32_64     -- ^ 32- or 64-bit general purpose register
   | T_R16_32_64  -- ^ 16-, 32- or 64-bit general purpose register

   -- Memory
   | T_M_PAIR     -- ^ Pair of words in memory (words are operand-size large)
   | T_M16_XX     -- ^ Pair of words in memory: m16:XX where XX can be 16, 32 or 64
   | T_M64_128    -- ^ 64- or 128-bit memory
   | T_M          -- ^ Any memory address
   | T_MFP        -- ^ Floating-point value in memory
   | T_M80dec     -- ^ Binary-coded decimal

   -- Vector registers
   | T_Vec           -- ^ Vector register (XMM, YMM, ZMM)
   | T_V64           -- ^ MMX Vector register
   | T_VM64          -- ^ MMX Vector register or 64-bit memory
   | T_V128          -- ^ XMM Vector register
   | T_VM128         -- ^ XMM Vector register or memory
   | T_V128_Low32    -- ^ Low 32-bits of a XMM Vector register
   | T_VM128_Low32   -- ^ Low 32-bits of a XMM Vector register or 32-bit memory
   | T_V128_Low64    -- ^ Low 64-bits of a XMM Vector register
   | T_VM128_Low64   -- ^ Low 64-bits of a XMM Vector register or 64-bit memory
   | T_V128_256      -- ^ XMM/YMM Vector register
   | T_VM128_256     -- ^ XMM/YMM Vector register or memory

   -- Specific registers
   | T_Accu       -- ^ Accumulator register (xAX)
   | T_AX_EAX_RAX -- ^ Accumulator registers except AL
   | T_xDX_xAX    -- ^ The pair (DX:AX), (EDX:EAX) or (RDX:RAX). If 8-bit mode is supported, it is only AX
   | T_xCX_xBX    -- ^ The pair (CX:BX), (ECX:EBX) or (RCX:RBX)
   | T_xAX        -- ^ EAX or RAX
   | T_xBX        -- ^ EBX or RBX
   | T_xCX        -- ^ ECX or RCX
   | T_xDX        -- ^ EDX or RDX
   | T_AL         -- ^ AL register
   | T_AX         -- ^ AX register
   | T_XMM0       -- ^ XMM0 register
   | T_rSI        -- ^ DS:rSI
   | T_rDI        -- ^ ES:rDI

   -- x87
   | T_ST0        -- ^ ST(0)
   | T_ST         -- ^ ST(i)
   | T_STMem      -- ^ ST(i) register or memory
   deriving (Show)

data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
   deriving (Show)

data Operand = Operand
   { opMode :: AccessMode
   , opType :: OperandType
   , opEnc  :: OperandEnc
   } deriving (Show)

data OpcodeMap
   = MapPrimary
   | Map0F
   | Map0F01
   | Map0F38
   | Map0F3A
   | Map3DNow
   | MapVex Int
   | MapXop Int
   deriving (Show,Eq)

data VexLW
   = W0     -- ^ Vex.W set to 0
   | W1     -- ^ Vex.W set to 1
   | WIG    -- ^ Vex.W ignored
   | L0     -- ^ Vex.L set to 0
   | L1     -- ^ Vex.L set to 1
   | LIG    -- ^ Vex.L ignored
   | LWIG   -- ^ Ignore Vex.W and Vex.L
   deriving (Show)

data Encoding
   = LegacyEncoding LegEnc
   | VexEncoding    VexEnc
   deriving (Show)


isLegacyEncoding :: Encoding -> Bool
isLegacyEncoding (LegacyEncoding _) = True
isLegacyEncoding _                  = False

data LegEnc = LegEnc
   { legEncMandatoryPrefix :: Maybe Word8        -- ^ Mandatory prefix
   , legEncOpcodeMap       :: OpcodeMap          -- ^ Map
   , legEncOpcode          :: Word8              -- ^ Opcode
   , legEncOpcodeExt       :: Maybe Word8        -- ^ Opcode extension in ModRM.reg
   , legEncOpcodeFields    :: LegacyOpcodeFields -- ^ Fields in the opcode
   , legEncProperties      :: [Properties]       -- ^ Encoding properties
   , legEncParams          :: [Operand]          -- ^ Operand encoding
   }
   deriving (Show)

data VexEnc = VexEnc
   { vexEncOpcodeMap       :: OpcodeMap
   , vexEncOpcode          :: Word8
   , vexEncLW              :: VexLW
   , vexEncParams          :: [Operand]
   } deriving (Show)

-- | Fields in a legacy opcode
data LegacyOpcodeFields = LegacyOpcodeFields
   { reversable         :: Maybe Int -- ^ Args are reversed if the given bit is
                                     --   set in the opcode.

   , sizable            :: Maybe Int -- ^ Operand size is 8 if the given bit is
                                     --   unset in the opcode. Otherwise, the
                                     --   size is defined by operand-size
                                     --   prefix and REX.W bit

   , signExtendableImm8 :: Maybe Int -- ^ Used in conjunction with a set
                                     --   Sizable bit.  Imm8 operand is used
                                     --   and sign-extended if the given bit is
                                     --   set
   }
   deriving (Show,Eq)

opf :: LegacyOpcodeFields
opf = LegacyOpcodeFields Nothing Nothing Nothing

legacyEncoding :: Maybe Word8 -> OpcodeMap -> Word8 -> Maybe Word8 -> LegacyOpcodeFields -> [Properties] -> [Operand] -> Encoding
legacyEncoding a b c d e f g = LegacyEncoding $ LegEnc a b c d e f g

vexEncoding :: OpcodeMap -> Word8 -> VexLW -> [Operand] -> Encoding
vexEncoding a b c d = VexEncoding $ VexEnc a b c d


getOperandEncodings :: X86Insn -> [OperandEnc]
getOperandEncodings x = do
   enc <- iEncoding x
   case enc of
      LegacyEncoding le  -> fmap opEnc (legEncParams le)
      VexEncoding    ve  -> fmap opEnc (vexEncParams ve)
   

-- | Indicate if an instruction requires ModRM
requireModRM :: X86Insn -> Bool
requireModRM insn = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg 
      hasOpExt = any matchOpExt (iProperties insn)

      -- has operands in ModRM
      hasOps   = any matchLegEnc (iEncoding insn)

      matchOpExt (OpExt _) = True
      matchOpExt _         = False

      matchEnc x = case opEnc x of
         E_ModRM     -> True
         E_ModReg    -> True
         E_Imm       -> False
         E_Imm8_7_4  -> False
         E_Imm8_3_0  -> False
         E_Implicit  -> False
         E_VexV      -> False
         E_OpReg     -> False

      matchLegEnc (LegacyEncoding le) = any matchEnc (legEncParams le)
      matchLegEnc _                   = False

i :: String -> String -> [Properties] -> [FlagOp Flag] -> [Encoding] -> X86Insn
i = X86Insn

op :: AccessMode -> OperandType -> OperandEnc -> Operand
op = Operand

instructions :: [X86Insn]
instructions =
   [ i_aaa
   , i_aad 
   , i_aam 
   , i_aas 
   , i_adc_acc 
   , i_adc 
   , i_adc_imm 
   , i_adcx 
   , i_add_acc 
   , i_add 
   , i_add_imm 
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
   , i_bt_imm 
   , i_btc 
   , i_btc_imm 
   , i_btr 
   , i_btr_imm 
   , i_bts 
   , i_bts_imm 
   , i_bzhi 
   , i_rel_near_call 
   , i_ind_near_call 
   , i_abs_far_call 
   , i_abs_ind_far_call 
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
   ]

i_aaa :: X86Insn
i_aaa = i "ASCII adjust AL after addition" "AAA"
   [Legacy]
   [Set [AF,CF], Undef [OF,SF,ZF,PF]]
   [legacyEncoding Nothing MapPrimary 0x37 Nothing 
      opf
      []
      [ op    RW    T_AX     E_Implicit
      ]]

i_aad :: X86Insn
i_aad = i "ASCII adjust AX before division" "AAD"
   [Legacy]
   [Set [SF,ZF,PF], Undef [OF,AF,CF]]
   [legacyEncoding Nothing MapPrimary 0xD5 Nothing
      opf
      []
      [ op    RW    T_AX     E_Implicit
      , op    RO    T_Imm8   E_Imm
      ]]

i_aam :: X86Insn
i_aam = i "ASCII adjust AX after multiply" "AAM"
   [Legacy, FailOnZero 0]
   [Set [SF,ZF,PF], Undef [OF,AF,CF]]
   [legacyEncoding Nothing MapPrimary 0xD4 Nothing
      opf
      []
      [ op    RW    T_AX     E_Implicit
      , op    RO    T_Imm8   E_Imm
      ]]

i_aas :: X86Insn
i_aas = i "ASCII adjust AL after subtraction" "AAS"
   [Legacy]
   [Set [AF,CF], Undef [OF,SF,ZF,PF]]
   [legacyEncoding Nothing MapPrimary 0x3F Nothing
      opf
      []
      [ op    RW    T_AX     E_Implicit
      ]]

i_adc_acc :: X86Insn
i_adc_acc = i "Add with carry in accumulator" "ADC"
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x14 Nothing
      (opf { sizable = Just 0})
      []
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]]

i_adc :: X86Insn
i_adc = i "Add with carry without immediate" "ADC"
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x10 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]]

i_adc_imm :: X86Insn
i_adc_imm = i "Add with carry with immediate" "ADC"
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x80 (Just 2)
      (opf {sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]]

i_adcx :: X86Insn
i_adcx = i "Unsigned integer addition with carry flags" "ADCX"
   [Extension ADX]
   [Read [CF], Set [CF]]
   [legacyEncoding (Just 0x66) Map0F38 0xF6 Nothing
      opf
      []
      [ op    RW    T_R32_64    E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_add_acc :: X86Insn
i_add_acc = i "Add in accumulator" "ADD"
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x04 Nothing
      (opf { sizable = Just 0})
      []
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]]

i_add :: X86Insn
i_add = i "Add without immediate" "ADD"
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x00 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]]

i_add_imm :: X86Insn
i_add_imm = i "Add with immediate" "ADD"
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x80 (Just 0)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]]

i_addpd :: X86Insn
i_addpd = i "Add packed double-precision floating-point values" "ADDPD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x58 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddpd :: X86Insn
i_vaddpd = i "Add packed double-precision floating-point values" "VADDPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x58 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_addps :: X86Insn
i_addps = i "Add packed float-precision floating-point values" "ADDPS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing Map0F 0x58 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddps :: X86Insn
i_vaddps = i "Add packed float-precision floating-point values" "VADDPS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x58 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_addsd :: X86Insn
i_addsd = i "Add scalar double-precision floating-point values" "ADDSD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x58 Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vaddsd :: X86Insn
i_vaddsd = i "Add scalar double-precision floating-point values" "VADDSD"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x58 LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low64  E_ModRM
      ]]

i_addss :: X86Insn
i_addss = i "Add scalar single-precision floating-point values" "ADDSS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x58 Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vaddss :: X86Insn
i_vaddss = i "Add scalar single-precision floating-point values" "VADDSS"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x58 LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low32  E_ModRM
      ]]

i_addsubpd :: X86Insn
i_addsubpd = i "Packed double-FP add/subtract" "ADDSUBPD"
   [Legacy, LongMode, Extension SSE3]
   []
   [legacyEncoding (Just 0x66) Map0F 0xD0 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddsubpd :: X86Insn
i_vaddsubpd = i "Packed double-FP add/subtract" "VADDSUBPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xD0 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_addsubps :: X86Insn
i_addsubps = i "Packed single-FP add/subtract" "ADDSUBPS"
   [Legacy, LongMode, Extension SSE3]
   []
   [legacyEncoding (Just 0xF2) Map0F 0xD0 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddsubps :: X86Insn
i_vaddsubps = i "Packed single-FP add/subtract" "VADDSUBPS"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xD0 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_adox :: X86Insn
i_adox = i "Unsigned integer addition of two operands with overflow flag" "ADOX"
   [Legacy, LongMode, Extension ADX]
   [Read [OF], Set [OF]]
   [legacyEncoding (Just 0xF3) Map0F38 0xF6 Nothing
      opf
      []
      [ op    RW    T_R32_64       E_ModReg
      , op    RO    T_RM32_64      E_ModRM
      ]]

i_aesdec :: X86Insn
i_aesdec = i "Perform one round of an AES decryption flow" "AESDEC"
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDE Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesdec :: X86Insn
i_vaesdec = i "Perform one round of an AES decryption flow" "VAESDEC"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x02) 0xDE WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128        E_ModRM
      ]]

i_aesdeclast :: X86Insn
i_aesdeclast = i "Perform last round of an AES decryption flow" "AESDECLAST"
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDF Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesdeclast :: X86Insn
i_vaesdeclast = i "Perform last round of an AES decryption flow" "VAESDECLAST"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x02) 0xDF WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128        E_ModRM
      ]]

i_aesenc :: X86Insn
i_aesenc = i "Perform one round of an AES encryption flow" "AESENC"
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDC Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesenc :: X86Insn
i_vaesenc = i "Perform one round of an AES encryption flow" "VAESENC"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x02) 0xDC WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128        E_ModRM
      ]]

i_aesenclast :: X86Insn
i_aesenclast = i "Perform last round of an AES encryption flow" "AESENCLAST"
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDD Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesenclast :: X86Insn
i_vaesenclast = i "Perform last round of an AES encryption flow" "VAESENCLAST"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x02) 0xDD WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128        E_ModRM
      ]]

i_aesimc :: X86Insn
i_aesimc = i "Perform the AES InvMixColumn transformation" "AESIMC"
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDB Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesimc :: X86Insn
i_vaesimc = i "Perform the AES InvMixColumn transformation" "VAESIMC"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x02) 0xDB WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_VM128        E_ModRM
      ]]

i_aeskeygenassist :: X86Insn
i_aeskeygenassist = i "AES round key generation assist" "AESKEYGENASSIST"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0xDF Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vaeskeygenassist :: X86Insn
i_vaeskeygenassist = i "AES round key generation assist" "VAESKEYGENASSIST"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0xDF WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_VM128        E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_and :: X86Insn
i_and = i "Logical AND" "AND"
   [Legacy, LongMode]
   [Zero [OF,CF], Set [SF,ZF,PF], Undef [AF]]
   [legacyEncoding Nothing MapPrimary 0x24 Nothing
      (opf { sizable = Just 0})
      []
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x20 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 4)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]
   ]

i_andn :: X86Insn
i_andn = i "Logical AND NOT" "ANDN"
   [Legacy, LongMode, Extension BMI1]
   [Set [SF,ZF], Zero [OF,CF], Undef [AF,PF]]
   [vexEncoding (MapVex 0x02) 0xF2 L0
      [ op    WO    T_R32_64     E_ModReg
      , op    RO    T_R32_64     E_VexV
      , op    RO    T_RM32_64    E_ModRM
      ]]

i_andpd :: X86Insn
i_andpd = i "Bitwise logical AND of packed double-precision floating-point values" "ANDPD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x54 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandpd :: X86Insn
i_vandpd = i "Bitwise logical AND of packed double-precision floating-point values" "VANDPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x54 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_andps :: X86Insn
i_andps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDPS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing Map0F 0x54 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandps :: X86Insn
i_vandps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDPS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x54 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_andnpd :: X86Insn
i_andnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "ANDNPD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x55 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandnpd :: X86Insn
i_vandnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "VANDNPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x55 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_andnps :: X86Insn
i_andnps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDNPS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing Map0F 0x55 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandnps :: X86Insn
i_vandnps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDNPS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x55 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_arpl :: X86Insn
i_arpl = i "Adjust RPL field of segment selector" "ARPL"
   [Legacy]
   [Set [ZF]]
   [legacyEncoding Nothing MapPrimary 0x63 Nothing
      opf
      []
      [ op    RW    T_RM16   E_ModRM
      , op    RO    T_R16    E_ModReg
      ]]

i_blendpd :: X86Insn
i_blendpd = i "Blend packed double-precision floating-point values" "BLENDPD"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x0D Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vblendpd :: X86Insn
i_vblendpd = i "Blend packed double-precision floating-point values" "VBLENDPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x0D WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_Imm8         E_Imm8_3_0
      ]]

i_bextr :: X86Insn
i_bextr = i "Bit field extract" "BEXTR"
   [Legacy, LongMode, Extension BMI1]
   [Set [ZF], Undef [AF,SF,PF], Zero (allFlags \\ [ZF,AF,SF,PF])]
   [vexEncoding (MapVex 0x02) 0xF7 L0
      [ op    WO    T_R32_64     E_ModReg
      , op    RO    T_RM32_64    E_ModRM
      , op    RO    T_R32_64     E_VexV
      ]]

i_blendps :: X86Insn
i_blendps = i "Blend packed single-precision floating-point values" "BLENDPS"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x0C Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vblendps :: X86Insn
i_vblendps = i "Blend packed single-precision floating-point values" "VBLENDPS"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x0C WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_blendvpd :: X86Insn
i_blendvpd = i "Variable blend packed double-precision floating-point values" "BLENDVPD"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F38 0x15 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_XMM0   E_Implicit
      ]]

i_vblendvpd :: X86Insn
i_vblendvpd = i "Variable blend packed double-precision floating-point values" "VBLENDVPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x4B W0
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_V128_256     E_Imm8_7_4
      ]]

i_blendvps :: X86Insn
i_blendvps = i "Variable blend packed single-precision floating-point values" "BLENDVPS"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F38 0x14 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_XMM0   E_Implicit
      ]]

i_vblendvps :: X86Insn
i_vblendvps = i "Variable blend packed single-precision floating-point values" "VBLENDVPS"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x4A W0
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_V128_256     E_Imm8_7_4
      ]]

i_blsi :: X86Insn
i_blsi = i "Extract lowest set isolated bit" "BLSI"
   [OpExt 3, Legacy, LongMode, Extension BMI1]
   [Set [ZF,SF, CF], Zero [OF], Undef [AF,PF]]
   [vexEncoding (MapVex 0x02) 0xF3 L0
      [ op    WO    T_R32_64     E_VexV
      , op    RO    T_RM32_64    E_ModRM
      ]]

i_blsmsk :: X86Insn
i_blsmsk = i "Get mask up to lowest set bit" "BLSMSK"
   [OpExt 2, Legacy, LongMode, Extension BMI1]
   [Set [SF,CF], Zero [ZF,OF], Undef [AF,PF]]
   [vexEncoding (MapVex 0x02) 0xF3 L0
      [ op    WO    T_R32_64     E_VexV
      , op    RO    T_RM32_64    E_ModRM
      ]]

i_blsr :: X86Insn
i_blsr = i "Reset lowest set bit" "BLSR"
   [OpExt 1, Legacy, LongMode, Extension BMI1]
   [Set [ZF,SF,CF], Zero [OF], Undef [AF,PF]]
   [vexEncoding (MapVex 0x02) 0xF3 L0
      [ op    WO    T_R32_64     E_VexV
      , op    RO    T_RM32_64    E_ModRM
      ]]

i_bound :: X86Insn
i_bound = i "Check array index against bounds" "BOUND"
   [Legacy]
   []
   [legacyEncoding Nothing MapPrimary 0x62 Nothing
      opf
      []
      [ op    RO    T_R16_32 E_ModReg
      , op    RO    T_M_PAIR E_ModRM
      ]]

i_bsf :: X86Insn
i_bsf = i "Bit scan forward" "BSF"
   [Legacy, LongMode]
   [Set [ZF], Undef [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBC Nothing
      opf
      []
      [ op    WO    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_bsr :: X86Insn
i_bsr = i "Bit scan reverse" "BSR"
   [Legacy, LongMode]
   [Set [ZF], Undef [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBD Nothing
      opf
      []
      [ op    WO    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_bswap :: X86Insn
i_bswap = i "Byte swap" "BSWAP"
   [Legacy, LongMode, Arch Intel486]
   []
   [legacyEncoding Nothing Map0F 0xC8 Nothing
      opf
      []
      [ op    RW    T_R32_64 E_OpReg
      ]]

i_bt :: X86Insn
i_bt = i "Bit test" "BT"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xA3 Nothing
      opf
      []
      [ op    RO    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_bt_imm :: X86Insn
i_bt_imm = i "Bit test with immediate index" "BT"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 4)
      opf
      []
      [ op    RO    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_btc :: X86Insn
i_btc = i "Bit test and complement" "BTC"
   [Legacy, LongMode, Lockable]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBB Nothing
      opf
      []
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_btc_imm :: X86Insn
i_btc_imm = i "Bit test with immediate index and complement" "BTC"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 7)
      opf
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_btr :: X86Insn
i_btr = i "Bit test and reset" "BTR"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xB3 Nothing
      opf
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_btr_imm :: X86Insn
i_btr_imm = i "Bit test with immediate index and reset" "BTR"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 6)
      opf
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_bts :: X86Insn
i_bts = i "Bit test and set" "BTS"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xAB Nothing
      opf
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_bts_imm :: X86Insn
i_bts_imm = i "Bit test with immediate index and set" "BTS"
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 5)
      opf
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_bzhi :: X86Insn
i_bzhi = i "Zero high bits starting with specified bit position" "BZHI"
   [Legacy, LongMode, Extension BMI2]
   [Set [ZF,CF,SF], Zero [OF], Undef [AF,PF]]
   [vexEncoding (MapVex 0x02) 0xF5 L0
      [ op    WO    T_R32_64     E_ModReg
      , op    RO    T_RM32_64    E_ModRM
      , op    RO    T_R32_64     E_VexV
      ]]

i_rel_near_call :: X86Insn
i_rel_near_call = i "Relative near call" "CALL"
   [Legacy, LongMode]
   [Undef allFlags]
   [legacyEncoding Nothing MapPrimary 0xE8 Nothing
      opf
      []
      [ op    RO    T_REL_16_32    E_ModRM ]]

i_ind_near_call :: X86Insn
i_ind_near_call = i "Indirect near call" "CALL"
   []
   [Undef allFlags]
   [legacyEncoding Nothing MapPrimary 0xFF (Just 2)
      opf
      [Legacy]
      [ op    RO    T_RM16_32      E_ModRM 
      ]
   ,legacyEncoding Nothing MapPrimary 0xFF (Just 2)
      opf
      [LongMode]
      [ op    RO    T_RM64         E_ModRM
      ]
   ]

i_abs_far_call :: X86Insn
i_abs_far_call = i "Absolute far call" "CALL"
   [Legacy]
   [Undef allFlags]
   [legacyEncoding Nothing MapPrimary 0x9A Nothing
      opf
      []
      [ op    RO    T_PTR_16_16    E_Imm ]
   ,legacyEncoding Nothing MapPrimary 0x9A Nothing
      opf
      []
      [ op    RO    T_PTR_16_32    E_Imm ]
   ]

i_abs_ind_far_call :: X86Insn
i_abs_ind_far_call = i "Absolute indirect far call" "CALL"
   [Legacy, LongMode]
   [Undef allFlags]
   [legacyEncoding Nothing MapPrimary 0xFF (Just 3)
      opf
      []
      [ op    RO    T_M16_XX       E_ModRM     ]]

i_extend_signed :: X86Insn
i_extend_signed = i "Extend signed word" "CBW/CWDE/CDQE"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing MapPrimary 0x98 Nothing
      opf
      []
      [ op    RW    T_Accu         E_Implicit  ]]

i_clac :: X86Insn
i_clac = i "Clear AC flag in EFLAGS register" "CLAC"
   [Legacy, LongMode, Extension SMAP]
   [Zero [AC]]
   [legacyEncoding Nothing Map0F01 0xCA Nothing opf [] []]

i_clc :: X86Insn
i_clc = i "Clear carry flag" "CLC"
   [Legacy, LongMode]
   [Zero [CF]]
   [legacyEncoding Nothing MapPrimary 0xF8 Nothing opf [] []]

i_cld :: X86Insn
i_cld = i "Clear direction flag" "CLD"
   [Legacy, LongMode]
   [Zero [DF]]
   [legacyEncoding Nothing MapPrimary 0xFC Nothing opf [] []]

i_clflush :: X86Insn
i_clflush = i "Flush cache line" "CLFLUSH"
   [Legacy, LongMode, Extension CLFLUSH]
   []
   [legacyEncoding Nothing Map0F 0xAE (Just 7)
      opf
      []
      [ op    RO    T_M      E_ModRM  ]]

i_cli :: X86Insn
i_cli = i "Clear interrupt flag" "CLI"
   [Legacy, LongMode]
   [Zero [IF]]
   [legacyEncoding Nothing MapPrimary 0xFA Nothing opf [] []]

i_clts :: X86Insn
i_clts = i "Clear task-switched flag in CR0" "CLTS"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0x06 Nothing opf [] []]

i_cmc :: X86Insn
i_cmc = i "Complement carry flag" "CMC"
   [Legacy, LongMode]
   [Set [CF]]
   [legacyEncoding Nothing MapPrimary 0xF5 Nothing opf [] []]

i_cmovo :: X86Insn
i_cmovo = i "Move if overflow (OF=1)" "CMOVO"
   [Legacy, LongMode]
   [Read [OF]]
   [legacyEncoding Nothing Map0F 0x40 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovno :: X86Insn
i_cmovno = i "Move if not overflow (OF=0)" "CMOVNO"
   [Legacy, LongMode]
   [Read [OF]]
   [legacyEncoding Nothing Map0F 0x41 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovc :: X86Insn
i_cmovc = i "Move if carry (CF=1)" "CMOVC"
   [Legacy, LongMode]
   [Read [CF]]
   [legacyEncoding Nothing Map0F 0x42 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovnc :: X86Insn
i_cmovnc = i "Move if not carry (CF=0)" "CMOVNC"
   [Legacy, LongMode]
   [Read [CF]]
   [legacyEncoding Nothing Map0F 0x43 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovz :: X86Insn
i_cmovz = i "Move if zero (ZF=1)" "CMOVZ"
   [Legacy, LongMode]
   [Read [ZF]]
   [legacyEncoding Nothing Map0F 0x44 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovnz :: X86Insn
i_cmovnz = i "Move if not zero (ZF=0)" "CMOVNZ"
   [Legacy, LongMode]
   [Read [ZF]]
   [legacyEncoding Nothing Map0F 0x45 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovbe :: X86Insn
i_cmovbe = i "Move if below or equal (CF=1, ZF=1)" "CMOVBE"
   [Legacy, LongMode]
   [Read [ZF,CF]]
   [legacyEncoding Nothing Map0F 0x46 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmova :: X86Insn
i_cmova = i "Move if above (CF=0, ZF=0)" "CMOVA"
   [Legacy, LongMode]
   [Read [ZF,CF]]
   [legacyEncoding Nothing Map0F 0x47 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovs :: X86Insn
i_cmovs = i "Move if sign (SF=1)" "CMOVS"
   [Legacy, LongMode]
   [Read [SF]]
   [legacyEncoding Nothing Map0F 0x48 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovns :: X86Insn
i_cmovns = i "Move if not sign (SF=0)" "CMOVNS"
   [Legacy, LongMode]
   [Read [SF]]
   [legacyEncoding Nothing Map0F 0x49 Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovp :: X86Insn
i_cmovp = i "Move if parity even (PF=1)" "CMOVP"
   [Legacy, LongMode]
   [Read [PF]]
   [legacyEncoding Nothing Map0F 0x4a Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovnp :: X86Insn
i_cmovnp = i "Move if parity odd (PF=0)" "CMOVNP"
   [Legacy, LongMode]
   [Read [PF]]
   [legacyEncoding Nothing Map0F 0x4b Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovl :: X86Insn
i_cmovl = i "Move if less (SF /= OF)" "CMOVL"
   [Legacy, LongMode]
   [Read [SF,OF]]
   [legacyEncoding Nothing Map0F 0x4c Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovge :: X86Insn
i_cmovge = i "Move if greater or equal (SF = OF)" "CMOVGE"
   [Legacy, LongMode]
   [Read [SF,OF]]
   [legacyEncoding Nothing Map0F 0x4d Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovle :: X86Insn
i_cmovle = i "Move if less or equal (ZF = 1 or SF <> OF)" "CMOVLE"
   [Legacy, LongMode]
   [Read [ZF,SF,OF]]
   [legacyEncoding Nothing Map0F 0x4e Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmovg :: X86Insn
i_cmovg = i "Move if greater (ZF = 0 or SF = OF)" "CMOVG"
   [Legacy, LongMode]
   [Read [ZF,SF,OF]]
   [legacyEncoding Nothing Map0F 0x4f Nothing
      opf
      []
      [ op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]


i_cmp :: X86Insn
i_cmp = i "Compare" "CMP"
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x3C Nothing
      (opf { sizable = Just 0})
      []
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x38 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 7)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]
   ]

i_cmppd :: X86Insn
i_cmppd = i "Compare packed double-precision floating-point values" "CMPPD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0xC2 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmppd :: X86Insn
i_vcmppd = i "Compare packed double-precision floating-point values" "VCMPPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xC2 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_cmpps :: X86Insn
i_cmpps = i "Compare packed single-precision floating-point values" "CMPPS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing Map0F 0xC2 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpps :: X86Insn
i_vcmpps = i "Compare packed single-precision floating-point values" "VCMPPS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xC2 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_cmps :: X86Insn
i_cmps = i "Compare string operands" "CMPS"
   [Legacy, LongMode]
   [Set [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xA6 Nothing
      (opf { sizable = Just 0})
      []
      [ op    RO    T_rSI    E_Implicit
      , op    RO    T_rDI    E_Implicit
      ]]

i_cmpsd :: X86Insn
i_cmpsd = i "Compare scalar double-precision floating-point values" "CMPSD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0xC2 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpsd :: X86Insn
i_vcmpsd = i "Compare scalar double-precision floating-point values" "VCMPSD"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xC2 LWIG
      [ op     WO    T_V128      E_ModReg
      , op     RO    T_V128      E_VexV
      , op     RO    T_VM128     E_ModRM
      , op     RO    T_Imm8      E_Imm
      ]]

i_cmpss :: X86Insn
i_cmpss = i "Compare scalar single-precision floating-point values" "CMPSS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) Map0F 0xC2 Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpss :: X86Insn
i_vcmpss = i "Compare scalar single-precision floating-point values" "VCMPSS"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xC2 LWIG
      [ op     WO    T_V128      E_ModReg
      , op     RO    T_V128      E_VexV
      , op     RO    T_VM128     E_ModRM
      , op     RO    T_Imm8      E_Imm
      ]]

i_cmpxchg :: X86Insn
i_cmpxchg = i "Compare and exchange" "CMPXCHG"
   [Legacy, LongMode, Arch Intel486]
   [Set [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing Map0F 0xB0 Nothing
      (opf {sizable = Just 0})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Accu   E_Implicit
      , op    RO    T_R      E_ModReg
      ]]

i_cmpxch8b :: X86Insn
i_cmpxch8b = i "Compare and exchange bytes" "CMPXCHG8B/CMPXCHG16B"
   [Legacy, LongMode, Arch IntelPentium, Extension CX8]
   [Set [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing Map0F 0xC7 Nothing
      opf
      [DoubleSizable, Lockable]
      [ op    RW    T_M64_128   E_ModRM ]]


i_comisd :: X86Insn
i_comisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "COMISD"
   [Legacy, LongMode, Extension SSE2]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [legacyEncoding (Just 0x66) Map0F 0x2F Nothing
      opf
      []
      [ op    RO    T_V128_Low64      E_ModReg
      , op    RO    T_VM128_Low64     E_ModRM
      ]]

i_vcomisd :: X86Insn
i_vcomisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "VCOMISD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [vexEncoding (MapVex 0x01) 0x2F LWIG
      [ op     RO    T_V128_Low64      E_ModReg
      , op     RO    T_VM128_Low64     E_ModRM
      ]]

i_comiss :: X86Insn
i_comiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "COMISS"
   [Legacy, LongMode, Extension SSE]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [legacyEncoding Nothing Map0F 0x2F Nothing
      opf
      []
      [ op    RO    T_V128_Low32      E_ModReg
      , op    RO    T_VM128_Low32     E_ModRM
      ]]

i_vcomiss :: X86Insn
i_vcomiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "VCOMISS"
   [Legacy, LongMode, Extension AVX]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [vexEncoding (MapVex 0x01) 0x2F LWIG
      [ op     RO    T_V128_Low32      E_ModReg
      , op     RO    T_VM128_Low32     E_ModRM
      ]]

i_cpuid :: X86Insn
i_cpuid = i "CPU identification" "CPUID"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0xA2 Nothing
      opf
      []
      [ op    RW    T_xAX     E_Implicit
      , op    RW    T_xCX     E_Implicit
      , op    WO    T_xBX     E_Implicit
      , op    WO    T_xDX     E_Implicit
      ]]

i_crc32 :: X86Insn
i_crc32 = i "Accumulate CRC32 value" "CRC32"
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0xF2) Map0F38 0xF0 Nothing
      (opf { sizable = Just 0 })
      []
      [ op    RW    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_cvtdq2pd :: X86Insn
i_cvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTDQ2PD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) Map0F 0xE6 Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 
      ]]

i_vcvtdq2pd :: X86Insn
i_vcvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "VCVTDQ2PD"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xE6 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 or xmm/m128
      ]]

i_cvtdq2ps :: X86Insn
i_cvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTDQ2PS"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding Nothing Map0F 0x5B Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtdq2ps :: X86Insn
i_vcvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "VCVTDQ2PS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5B WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvtpd2dq :: X86Insn
i_cvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DQ"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0xE6 Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtpd2dq :: X86Insn
i_vcvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "VCVTPD2DQ"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xE6 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvtpd2di :: X86Insn
i_cvtpd2di = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DI"
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) Map0F 0x2D Nothing
      opf
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_cvtpd2ps :: X86Insn
i_cvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "CVTPD2PS"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x5A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtpd2ps :: X86Insn
i_vcvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "VCVTPD2PS"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5A WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvtpi2pd :: X86Insn
i_cvtpi2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTPI2PD"
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) Map0F 0x2A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM64         E_ModRM
      ]]

i_cvtpi2ps :: X86Insn
i_cvtpi2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTPI2PS"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0x2A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM64         E_ModRM
      ]]

i_cvtps2dq :: X86Insn
i_cvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2DQ"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x5B Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtps2dq :: X86Insn
i_vcvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "VCVTPS2DQ"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5B WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvtps2pd :: X86Insn
i_cvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "CVTPS2PD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding Nothing Map0F 0x5A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtps2pd :: X86Insn
i_vcvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "VCVTPS2PD"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5A WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvtps2pi :: X86Insn
i_cvtps2pi = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2PI"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0x2D Nothing
      opf
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_cvtsd2si :: X86Insn
i_cvtsd2si = i "Convert scalar double-precision floating-point value to integer" "CVTSD2SI"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2D Nothing
      opf
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvtsd2si :: X86Insn
i_vcvtsd2si = i "Convert scalar double-precision floating-point value to integer" "VCVTSD2SI"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2D LIG
      [ op     WO    T_R32_64         E_ModReg
      , op     RO    T_VM128_Low64    E_ModRM
      ]]

i_cvtsd2ss :: X86Insn
i_cvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "CVTSD2SS"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x5A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvtsd2ss :: X86Insn
i_vcvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "VCVTSD2SS"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5A LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low64  E_ModRM
      ]]

i_cvtsi2sd :: X86Insn
i_cvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "CVTSI2SD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2A Nothing
      opf
      []
      [ op    WO    T_V128      E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_vcvtsi2sd :: X86Insn
i_vcvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "VCVTSI2SD"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2A LIG
      [ op     WO    T_V128     E_ModReg
      , op     RO    T_V128     E_VexV
      , op     RO    T_RM32_64  E_ModRM
      ]]


i_cvtsi2ss :: X86Insn
i_cvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "CVTSI2SS"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2A Nothing
      opf
      []
      [ op    WO    T_V128      E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_vcvtsi2ss :: X86Insn
i_vcvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "VCVTSI2SS"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2A LIG
      [ op     WO    T_V128     E_ModReg
      , op     RO    T_V128     E_VexV
      , op     RO    T_RM32_64  E_ModRM
      ]]

i_cvtss2sd :: X86Insn
i_cvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "CVTSS2SD"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5A Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvtss2sd :: X86Insn
i_vcvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "VCVTSS2SD"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5A LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low32  E_ModRM
      ]]

i_cvtss2si :: X86Insn
i_cvtss2si = i "Convert scalar single-precision floating-point value to Int32" "CVTSS2SI"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2D Nothing
      opf
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvtss2si :: X86Insn
i_vcvtss2si = i "Convert scalar single-precision floating-point value to Int32" "VCVTSS2SI"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2D LIG
      [ op     WO    T_R32_64       E_ModReg
      , op     RO    T_VM128_Low32  E_ModRM
      ]]

i_cvttpd2dq :: X86Insn
i_cvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2DQ"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0xE6 Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvttpd2dq :: X86Insn
i_vcvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "VCVTTPD2DQ"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0xE6  WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvttpd2pi :: X86Insn
i_cvttpd2pi = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2PI"
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) Map0F 0x2C Nothing
      opf
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_cvttps2dq :: X86Insn
i_cvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2DQ"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5B Nothing
      opf
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvttps2dq :: X86Insn
i_vcvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "VCVTTPS2DQ"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5B WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_cvttps2pi :: X86Insn
i_cvttps2pi = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2PI"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0x2C Nothing
      opf
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_cvttsd2si :: X86Insn
i_cvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "CVTTSD2SI"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2C Nothing
      opf
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvttsd2si :: X86Insn
i_vcvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "VCVTTSD2SI"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2C LIG
      [ op     WO    T_R32_64         E_ModReg
      , op     RO    T_VM128_Low64    E_ModRM
      ]]

i_cvttss2si :: X86Insn
i_cvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "CVTTSS2SI"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2C Nothing
      opf
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvttss2si :: X86Insn
i_vcvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "VCVTTSS2SI"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x2C LIG
      [ op     WO    T_R32_64       E_ModReg
      , op     RO    T_VM128_Low32  E_ModRM
      ]]

i_cwd :: X86Insn
i_cwd = i "Convert between words (sign-extend)" "CWD/CDQ/CQO"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing MapPrimary 0x99 Nothing
      opf
      []
      [ op    WO    T_xDX_xAX      E_Implicit
      , op    RO    T_AX_EAX_RAX   E_Implicit
      ]]

i_daa :: X86Insn
i_daa = i "Decimal adjust AL after addition" "DAA"
   [Legacy]
   [Set [AF,CF,SF,ZF,PF], Undef [OF]]
   [legacyEncoding Nothing MapPrimary 0x27 Nothing
      opf
      []
      [ op    RW    T_AL     E_Implicit ]]

i_das :: X86Insn
i_das = i "Decimal adjust AL after subtraction" "DAS"
   [Legacy]
   [Set [AF,CF,SF,ZF,PF], Undef [OF]]
   [legacyEncoding Nothing MapPrimary 0x2F Nothing
      opf
      []
      [ op    RW    T_AL     E_Implicit ]]

i_dec :: X86Insn
i_dec = i "Decrement by 1" "DEC"
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xFE (Just 1)
      (opf { sizable = Just 0})
      [Lockable]
      [ op    RW    T_RM     E_ModRM
      ]
   ,legacyEncoding Nothing MapPrimary 0x48 Nothing
      opf
      [Legacy,Lockable]
      [ op    RW    T_R16_32    E_OpReg
      ]
   ]

i_div :: X86Insn
i_div = i "Unsigned divide" "DIV"
   [Legacy, LongMode, FailOnZero 0]
   [Undef [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xF6 (Just 6)
      (opf {sizable = Just 0})
      [Lockable]
      [ op    RO    T_RM        E_ModRM 
      , op    RW    T_xDX_xAX   E_Implicit
      ]]

i_divpd :: X86Insn
i_divpd = i "Divide packed double-precision floating-point values" "DIVPD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) Map0F 0x5E Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vdivpd :: X86Insn
i_vdivpd = i "Divide packed double-precision floating-point values" "VDIVPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5E WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_divps :: X86Insn
i_divps = i "Divide packed float-precision floating-point values" "DIVPS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing Map0F 0x5E Nothing
      opf
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vdivps :: X86Insn
i_vdivps = i "Divide packed float-precision floating-point values" "VDIVPS"
   [Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5E WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      ]]

i_divsd :: X86Insn
i_divsd = i "Divide scalar double-precision floating-point values" "DIVSD"
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) Map0F 0x5E Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vdivsd :: X86Insn
i_vdivsd = i "Divide scalar double-precision floating-point values" "VDIVSD"
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5E LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low64  E_ModRM
      ]]

i_divss :: X86Insn
i_divss = i "Divide scalar single-precision floating-point values" "DIVSS"
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5E Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vdivss :: X86Insn
i_vdivss = i "Divide scalar single-precision floating-point values" "VDIVSS"
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x01) 0x5E LWIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128_Low32  E_ModRM
      ]]

i_dppd :: X86Insn
i_dppd = i "Dot product of packed double precision floating-point values" "DPPD"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x41 Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vdppd :: X86Insn
i_vdppd = i "Dot product of packed double precision floating-point values" "VDPPD"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x41 WIG
      [ op     WO    T_V128         E_ModReg
      , op     RO    T_V128         E_VexV
      , op     RO    T_VM128        E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_dpps :: X86Insn
i_dpps = i "Dot product of packed single precision floating-point values" "DPPS"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x40 Nothing
      opf
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vdpps :: X86Insn
i_vdpps = i "Dot product of packed single precision floating-point values" "VDPPS"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x40 WIG
      [ op     WO    T_V128_256     E_ModReg
      , op     RO    T_V128_256     E_VexV
      , op     RO    T_VM128_256    E_ModRM
      , op     RO    T_Imm8         E_Imm
      ]]

i_emms :: X86Insn
i_emms = i "Empty MMX technology state" "EMMS"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing Map0F 0x77 Nothing opf [] []]

i_enter :: X86Insn
i_enter = i "Make stack frame for procedure parameters" "ENTER"
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing MapPrimary 0xC8 Nothing
      opf
      []
      [ op    RO    T_Imm16     E_Imm
      , op    RO    T_Imm8      E_Imm
      ]]

i_extractps :: X86Insn
i_extractps = i "Extract packed single precision floating-point value" "EXTRACTPS"
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x17 Nothing
      opf
      []
      [ op    RW    T_RM32         E_ModRM
      , op    RO    T_V128         E_ModReg
      , op    RO    T_Imm8         E_Imm
      ]]

i_vextractps :: X86Insn
i_vextractps = i "Extract packed single precision floating-point value" "VEXTRACTPS"
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [vexEncoding (MapVex 0x03) 0x17 WIG
      [ op     WO    T_RM32         E_ModRM
      , op     RO    T_V128         E_VexV
      , op     RO    T_Imm8         E_Imm
      ]]



--makeParser :: [X86Insn] -> Tree PTree
--makeParser insns = makeTree insns
--   where
--      -- generate multiple opcodes from insn properties and encodings
--      multiOpcode :: X86Insn -> [X86Insn]
--      multiOpcode i = fmap (\o' -> i {iOpcode = ini ++ [o']}) ops
--         where
--            ops = o : recProp [] (iProperties i) ++
--                      recEnc [] (getOperandEncodings i)
--
--            o   = last (iOpcode i)
--            ini = init (iOpcode i)
--
--            recEnc os [] = os
--            recEnc os (e:es) = recEnc os' es
--               where
--                  os' = case e of
--                     E_OpReg -> fmap (o+) [1..7] ++ os
--                     _       -> os
--
--            recProp os []     = os
--            recProp os (p:ps) = recProp (os' ++ os) ps
--               where 
--                  os' = case p of
--                     Sizable n -> [setBit o n]
--
--                     SignExtendableImm8 n -> [setBit o' n]
--                        where
--                           --find associated Sizable
--                           isSizable (Sizable _) = True
--                           isSizable _           = False
--                           Sizable szb = head (filter isSizable (iProperties i))
--                           o' = setBit o szb
--
--                     Reversable n -> [setBit o n]
--
--                     _  -> []
--
--      -- drop the first opcode of all the given instructions
--      dropOpcode []     = []
--      dropOpcode (i:is) = 
--         case iOpcode i of
--            [] -> dropOpcode is
--            o' -> (i { iOpcode = tail o'}) : dropOpcode is
--
--      cmp x y = tst x == tst y
--      tst = head . iOpcode
--
--      rec :: [X86Insn] -> [Tree PTree]
--      rec is = (leaves ++ gs)
--         where
--            (ls,ns) = List.partition (null . iOpcode) is
--
--            leaves = fmap (\x -> Node (PInsn x) []) ls
--
--            -- instruction grouped by first opcode
--            gs = fmap f $ groupBy cmp ns
--
--            f :: [X86Insn] -> Tree PTree
--            f xs = Node (POpcode (head (iOpcode (head xs)))) (rec (dropOpcode xs))
--
--      makeTree :: [X86Insn] -> Tree PTree
--      makeTree is = Node PRoot (rec (concatMap multiOpcode is))
--
--
--data PTree
--   = PRoot           -- ^ Root of the parsing tree
--   | POpcode Word8   -- ^ Parse opcode byte
--   | PInsn X86Insn   -- ^ Candidate instruction
--   deriving (Show)


data MapEntry = MapEntry
   { entryHasModRM :: Bool
   , candidates    :: [X86Insn]
   }

getLegacyEncodings :: [X86Insn] -> [(LegEnc,X86Insn)]
getLegacyEncodings = concatMap f 
   where
      f i = g i (iEncoding i)
      g i [] = []
      g i (e:es) = case e of
         LegacyEncoding ec -> (ec,i) : g i es
         _                 -> g i es

-- | Build the opcode maps
--buildPrimaryOpcodeMap :: [X86Insn] -> V.Vector (Maybe MapEntry)
--buildPrimaryOpcodeMap insns = go encs Map.empty
--   where
--      encs = filter (ff . fst) getLegacyEncodings insns
--
--      ff i = init (legEncOpcode i) == []
--
--      go [] rs     = V.generate 256 (Map.lookup rs)
--      go ((e,i):is) rs = let
--            o = last (legEncOpcode e)
--            e = MapEntry (requireModRM i) [o]
--         in
--            
