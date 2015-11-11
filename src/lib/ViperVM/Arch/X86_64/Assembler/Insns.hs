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
   , insnOpcodeMap
   , instructions
   , requireModRM
   , PTree(..)
   , makeParser
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

data X86Insn = X86Insn
   { iDesc        :: String
   , iMnemonic    :: String
   , iOpcode      :: [Word8]
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
   | E_OpCC       -- ^ Operand stored in opcode 4 last bits
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

   -- Special
   | T_CC         -- ^ A condition type (LT, NE, G, etc.)
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
   | Vex VexLW [Operand]
   deriving (Show)

data LegEnc = LegEnc
   { legEncMandatoryPrefix :: Maybe Word8    -- ^ Mandatory prefix
   , legEncOpcode          :: [Word8]        -- ^ Map + opcode
   , legEncOpcodeExt       :: Maybe Word8    -- ^ Opcode extension in ModRM.reg
   , legEncProperties      :: [Properties]   -- ^ Encoding properties
   , legEncParams          :: [Operand]      -- ^ Operand encoding
   }
   deriving (Show)

legacyEncoding :: Maybe Word8 -> [Word8] -> Maybe Word8 -> [Properties] -> [Operand] -> Encoding
legacyEncoding a b c d e = LegacyEncoding $ LegEnc a b c d e


getOperandEncodings :: X86Insn -> [OperandEnc]
getOperandEncodings i = do
   enc <- iEncoding i
   case enc of
      LegacyEncoding le  -> fmap opEnc (legEncParams le)
      Vex _ ops          -> fmap opEnc ops
   

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
         E_OpCC      -> False

      matchLegEnc (LegacyEncoding le) = any matchEnc (legEncParams le)
      matchLegEnc _                   = False

i :: String -> String -> [Word8] -> [Properties] -> [FlagOp Flag] -> [Encoding] -> X86Insn
i = X86Insn

op :: AccessMode -> OperandType -> OperandEnc -> Operand
op = Operand

insnOpcodeMap :: Map [Word8] [X86Insn]
insnOpcodeMap = foldl' g Map.empty instructions
   where
      g xs x = Map.insertWith (++) (iOpcode x) [x] xs

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
   , i_cmov 
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
i_aaa = i "ASCII adjust AL after addition" "AAA" [0x37]
   [Legacy]
   [Set [AF,CF], Undef [OF,SF,ZF,PF]]
   [legacyEncoding Nothing [0x37] Nothing 
      []
      [ op    RW    T_AX     E_Implicit
      ]]

i_aad :: X86Insn
i_aad = i "ASCII adjust AX before division" "AAD" [0xD5]
   [Legacy]
   [Set [SF,ZF,PF], Undef [OF,AF,CF]]
   [legacyEncoding Nothing [0xD5] Nothing
      []
      [ op    RW    T_AX     E_Implicit
      , op    RO    T_Imm8   E_Imm
      ]]

i_aam :: X86Insn
i_aam = i "ASCII adjust AX after multiply" "AAM" [0xD4]
   [Legacy, FailOnZero 0]
   [Set [SF,ZF,PF], Undef [OF,AF,CF]]
   [legacyEncoding Nothing [0xD4] Nothing
      []
      [ op    RW    T_AX     E_Implicit
      , op    RO    T_Imm8   E_Imm
      ]]

i_aas :: X86Insn
i_aas = i "ASCII adjust AL after subtraction" "AAS" [0x3F]
   [Legacy]
   [Set [AF,CF], Undef [OF,SF,ZF,PF]]
   [legacyEncoding Nothing [0x3F] Nothing
      []
      [ op    RW    T_AX     E_Implicit
      ]]

i_adc_acc :: X86Insn
i_adc_acc = i "Add with carry in accumulator" "ADC" [0x14]
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x14] Nothing
      [Sizable 0]
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]]

i_adc :: X86Insn
i_adc = i "Add with carry without immediate" "ADC" [0x10]
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x10] Nothing
      [Sizable 0, Reversable 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]]

i_adc_imm :: X86Insn
i_adc_imm = i "Add with carry with immediate" "ADC" [0x80]
   [Legacy, LongMode]
   [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x80] (Just 2)
      [Sizable 0, SignExtendableImm8 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]]

i_adcx :: X86Insn
i_adcx = i "Unsigned integer addition with carry flags" "ADCX" [0x0F,0x38,0xF6]
   [Extension ADX]
   [Read [CF], Set [CF]]
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xF6] Nothing
      []
      [ op    RW    T_R32_64    E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_add_acc :: X86Insn
i_add_acc = i "Add in accumulator" "ADD" [0x04]
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x04] Nothing
      [Sizable 0]
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]]

i_add :: X86Insn
i_add = i "Add without immediate" "ADD" [0x00]
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x00] Nothing
      [Sizable 0, Reversable 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]]

i_add_imm :: X86Insn
i_add_imm = i "Add with immediate" "ADD" [0x80]
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x80] (Just 0)
      [Sizable 0, SignExtendableImm8 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]]

i_addpd :: X86Insn
i_addpd = i "Add packed double-precision floating-point values" "ADDPD" [0x0F,0x58]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x58] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddpd :: X86Insn
i_vaddpd = i "Add packed double-precision floating-point values" "VADDPD" [0x0F,0x58]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_addps :: X86Insn
i_addps = i "Add packed float-precision floating-point values" "ADDPS" [0x0F,0x58]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing [0x0F,0x58] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddps :: X86Insn
i_vaddps = i "Add packed float-precision floating-point values" "VADDPS" [0x0F,0x58]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_addsd :: X86Insn
i_addsd = i "Add scalar double-precision floating-point values" "ADDSD" [0x0F,0x58]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x58] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vaddsd :: X86Insn
i_vaddsd = i "Add scalar double-precision floating-point values" "VADDSD" [0x0F,0x58]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low64  E_ModRM
                  ]]

i_addss :: X86Insn
i_addss = i "Add scalar single-precision floating-point values" "ADDSS" [0x0F,0x58]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x58] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vaddss :: X86Insn
i_vaddss = i "Add scalar single-precision floating-point values" "VADDSS" [0x0F,0x58]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low32  E_ModRM
                  ]]

i_addsubpd :: X86Insn
i_addsubpd = i "Packed double-FP add/subtract" "ADDSUBPD" [0x0F,0xD0]
   [Legacy, LongMode, Extension SSE3]
   []
   [legacyEncoding (Just 0x66) [0x0F,0xD0] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddsubpd :: X86Insn
i_vaddsubpd = i "Packed double-FP add/subtract" "VADDSUBPD" [0x0F,0xD0]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                        ]]
i_addsubps :: X86Insn
i_addsubps = i "Packed single-FP add/subtract" "ADDSUBPS" [0x0F,0xD0]
   [Legacy, LongMode, Extension SSE3]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0xD0] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vaddsubps :: X86Insn
i_vaddsubps = i "Packed single-FP add/subtract" "VADDSUBPS" [0x0F,0xD0]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_adox :: X86Insn
i_adox = i "Unsigned integer addition of two operands with overflow flag" "ADOX" [0x0F,0x38,0xF6]
   [Legacy, LongMode, Extension ADX]
   [Read [OF], Set [OF]]
   [legacyEncoding (Just 0xF3) [0x0F,0x38,0xF6] Nothing
      []
      [ op    RW    T_R32_64       E_ModReg
      , op    RO    T_RM32_64      E_ModRM
      ]]

i_aesdec :: X86Insn
i_aesdec = i "Perform one round of an AES decryption flow" "AESDEC" [0x0F,0x38,0xDE]
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xDE] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesdec :: X86Insn
i_vaesdec = i "Perform one round of an AES decryption flow" "VAESDEC" [0x0F,0x38,0xDE]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128        E_ModRM
                  ]]

i_aesdeclast :: X86Insn
i_aesdeclast = i "Perform last round of an AES decryption flow" "AESDECLAST" [0x0F,0x38,0xDF]
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xDF] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesdeclast :: X86Insn
i_vaesdeclast = i "Perform last round of an AES decryption flow" "VAESDECLAST" [0x0F,0x38,0xDF]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128        E_ModRM
                  ]]

i_aesenc :: X86Insn
i_aesenc = i "Perform one round of an AES encryption flow" "AESENC" [0x0F,0x38,0xDC]
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xDC] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesenc :: X86Insn
i_vaesenc = i "Perform one round of an AES encryption flow" "VAESENC" [0x0F,0x38,0xDC]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128        E_ModRM
                        ]]

i_aesenclast :: X86Insn
i_aesenclast = i "Perform last round of an AES encryption flow" "AESENCLAST" [0x0F,0x38,0xDD]
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xDD] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesenclast :: X86Insn
i_vaesenclast = i "Perform last round of an AES encryption flow" "VAESENCLAST" [0x0F,0x38,0xDD]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128        E_ModRM
                  ]]

i_aesimc :: X86Insn
i_aesimc = i "Perform the AES InvMixColumn transformation" "AESIMC" [0x0F,0x38,0xDB]
   [Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0xDB] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vaesimc :: X86Insn
i_vaesimc = i "Perform the AES InvMixColumn transformation" "VAESIMC" [0x0F,0x38,0xDB]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_VM128        E_ModRM
                  ]]

i_aeskeygenassist :: X86Insn
i_aeskeygenassist = i "AES round key generation assist" "AESKEYGENASSIST" [0x0F,0x3A,0xDF]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0xDF] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vaeskeygenassist :: X86Insn
i_vaeskeygenassist = i "AES round key generation assist" "VAESKEYGENASSIST" [0x0F,0x3A,0xDF]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_VM128        E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_and :: X86Insn
i_and = i "Logical AND" "AND" [0x20]
   [Legacy, LongMode]
   [Zero [OF,CF], Set [SF,ZF,PF], Undef [AF]]
   [legacyEncoding Nothing [0x24] Nothing
      [Sizable 0]
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]
   ,legacyEncoding Nothing [0x20] Nothing
      [Sizable 0, Reversable 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]
   ,legacyEncoding Nothing [0x80] (Just 4)
      [Sizable 0, SignExtendableImm8 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]
   ]

i_andn :: X86Insn
i_andn = i "Logical AND NOT" "ANDN" [0x0F,0x38,0xF2]
   [Legacy, LongMode, Extension BMI1]
   [Set [SF,ZF], Zero [OF,CF], Undef [AF,PF]]
   [Vex L0        [ op    WO    T_R32_64     E_ModReg
                  , op    RO    T_R32_64     E_VexV
                  , op    RO    T_RM32_64    E_ModRM
                  ]]

i_andpd :: X86Insn
i_andpd = i "Bitwise logical AND of packed double-precision floating-point values" "ANDPD" [0x0F,0x54]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x54] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandpd :: X86Insn
i_vandpd = i "Bitwise logical AND of packed double-precision floating-point values" "VANDPD" [0x0F,0x54]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_andps :: X86Insn
i_andps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDPS" [0x0F,0x54]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing [0x0F,0x54] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandps :: X86Insn
i_vandps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDPS" [0x0F,0x54]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_andnpd :: X86Insn
i_andnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "ANDNPD" [0x0F,0x55]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x55] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandnpd :: X86Insn
i_vandnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "VANDNPD" [0x0F,0x55]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_andnps :: X86Insn
i_andnps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDNPS" [0x0F,0x55]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing [0x0F,0x55] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vandnps :: X86Insn
i_vandnps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDNPS" [0x0F,0x55]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_arpl :: X86Insn
i_arpl = i "Adjust RPL field of segment selector" "ARPL" [0x63]
   [Legacy]
   [Set [ZF]]
   [legacyEncoding Nothing [0x63] Nothing
      []
      [ op    RW    T_RM16   E_ModRM
      , op    RO    T_R16    E_ModReg
      ]]

i_blendpd :: X86Insn
i_blendpd = i "Blend packed double-precision floating-point values" "BLENDPD" [0x0F,0x3A,0x0D]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0x0D] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vblendpd :: X86Insn
i_vblendpd = i "Blend packed double-precision floating-point values" "VBLENDPD" [0x0F,0x3A,0x0D]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_Imm8         E_Imm8_3_0
                  ]]

i_bextr :: X86Insn
i_bextr = i "Bit field extract" "BEXTR" [0x0F,0x38,0xF7]
   [Legacy, LongMode, Extension BMI1]
   [Set [ZF], Undef [AF,SF,PF], Zero (allFlags \\ [ZF,AF,SF,PF])]
   [Vex L0        [ op    WO    T_R32_64     E_ModReg
                  , op    RO    T_RM32_64    E_ModRM
                  , op    RO    T_R32_64     E_VexV
                  ]]

i_blendps :: X86Insn
i_blendps = i "Blend packed single-precision floating-point values" "BLENDPS" [0x0F,0x3A,0x0C]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0x0C] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vblendps :: X86Insn
i_vblendps = i "Blend packed single-precision floating-point values" "VBLENDPS" [0x0F,0x3A,0x0C]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_blendvpd :: X86Insn
i_blendvpd = i "Variable blend packed double-precision floating-point values" "BLENDVPD" [0x0F,0x38,0x15]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0x15] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_XMM0   E_Implicit
      ]]

i_vblendvpd :: X86Insn
i_vblendvpd = i "Variable blend packed double-precision floating-point values" "VBLENDVPD" [0x0F,0x3A,0x4B]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex W0        [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_V128_256     E_Imm8_7_4
                  ]]

i_blendvps :: X86Insn
i_blendvps = i "Variable blend packed single-precision floating-point values" "BLENDVPS" [0x0F,0x38,0x14]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x38,0x14] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_XMM0   E_Implicit
      ]]

i_vblendvps :: X86Insn
i_vblendvps = i "Variable blend packed single-precision floating-point values" "VBLENDVPS" [0x0F,0x3A,0x4A]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex W0        [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_V128_256     E_Imm8_7_4
                  ]]

i_blsi :: X86Insn
i_blsi = i "Extract lowest set isolated bit" "BLSI" [0x0F,0x38,0xF3]
   [OpExt 3, Legacy, LongMode, Extension BMI1]
   [Set [ZF,SF, CF], Zero [OF], Undef [AF,PF]]
   [Vex L0        [ op    WO    T_R32_64     E_VexV
                  , op    RO    T_RM32_64    E_ModRM
                  ]]

i_blsmsk :: X86Insn
i_blsmsk = i "Get mask up to lowest set bit" "BLSMSK" [0x0F,0x38,0xF3]
   [OpExt 2, Legacy, LongMode, Extension BMI1]
   [Set [SF,CF], Zero [ZF,OF], Undef [AF,PF]]
   [Vex L0        [ op    WO    T_R32_64     E_VexV
                  , op    RO    T_RM32_64    E_ModRM
                  ]]

i_blsr :: X86Insn
i_blsr = i "Reset lowest set bit" "BLSR" [0x0F,0x38,0xF3]
   [OpExt 1, Legacy, LongMode, Extension BMI1]
   [Set [ZF,SF,CF], Zero [OF], Undef [AF,PF]]
   [Vex L0        [ op    WO    T_R32_64     E_VexV
                  , op    RO    T_RM32_64    E_ModRM
                  ]]

i_bound :: X86Insn
i_bound = i "Check array index against bounds" "BOUND" [0x62]
   [Legacy]
   []
   [legacyEncoding Nothing [0x62] Nothing
      []
      [ op    RO    T_R16_32 E_ModReg
      , op    RO    T_M_PAIR E_ModRM
      ]]

i_bsf :: X86Insn
i_bsf = i "Bit scan forward" "BSF" [0x0F,0xBC]
   [Legacy, LongMode]
   [Set [ZF], Undef [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBC] Nothing
      []
      [ op    WO    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_bsr :: X86Insn
i_bsr = i "Bit scan reverse" "BSR" [0x0F,0xBD]
   [Legacy, LongMode]
   [Set [ZF], Undef [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBD] Nothing
      []
      [ op    WO    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_bswap :: X86Insn
i_bswap = i "Byte swap" "BSWAP" [0x0F,0xC8]
   [Legacy, LongMode, Arch Intel486]
   []
   [legacyEncoding Nothing [0x0F,0xC8] Nothing
      []
      [ op    RW    T_R32_64 E_OpReg
      ]]

i_bt :: X86Insn
i_bt = i "Bit test" "BT" [0x0F,0xA3]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xA3] Nothing
      []
      [ op    RO    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_bt_imm :: X86Insn
i_bt_imm = i "Bit test with immediate index" "BT" [0x0F,0xBA]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBA] (Just 4)
      []
      [ op    RO    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_btc :: X86Insn
i_btc = i "Bit test and complement" "BTC" [0x0F,0xBB]
   [Legacy, LongMode, Lockable]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBB] Nothing
      []
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_btc_imm :: X86Insn
i_btc_imm = i "Bit test with immediate index and complement" "BTC" [0x0F,0xBA]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBA] (Just 7)
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_btr :: X86Insn
i_btr = i "Bit test and reset" "BTR" [0x0F,0xB3]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xB3] Nothing
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_btr_imm :: X86Insn
i_btr_imm = i "Bit test with immediate index and reset" "BTR" [0x0F,0xBA]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBA] (Just 6)
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_bts :: X86Insn
i_bts = i "Bit test and set" "BTS" [0x0F,0xAB]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xAB] Nothing
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_R16_32_64    E_ModReg
      ]]

i_bts_imm :: X86Insn
i_bts_imm = i "Bit test with immediate index and set" "BTS" [0x0F,0xBA]
   [Legacy, LongMode]
   [Set [CF], Undef [OF,SF,AF,PF]]
   [legacyEncoding Nothing [0x0F,0xBA] (Just 5)
      [Lockable]
      [ op    RW    T_RM16_32_64   E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_bzhi :: X86Insn
i_bzhi = i "Zero high bits starting with specified bit position" "BZHI" [0x0F,0x38,0xF5]
   [Legacy, LongMode, Extension BMI2]
   [Set [ZF,CF,SF], Zero [OF], Undef [AF,PF]]
   [Vex L0        [ op    WO    T_R32_64     E_ModReg
                  , op    RO    T_RM32_64    E_ModRM
                  , op    RO    T_R32_64     E_VexV
                  ]]

i_rel_near_call :: X86Insn
i_rel_near_call = i "Relative near call" "CALL" [0xE8]
   [Legacy, LongMode]
   [Undef allFlags]
   [legacyEncoding Nothing [0xE8] Nothing
      []
      [ op    RO    T_REL_16_32    E_ModRM ]]

i_ind_near_call :: X86Insn
i_ind_near_call = i "Indirect near call" "CALL" [0xFF]
   []
   [Undef allFlags]
   [legacyEncoding Nothing [0xFF] (Just 2)
      [Legacy]
      [ op    RO    T_RM16_32      E_ModRM 
      ]
   ,legacyEncoding Nothing [0xFF] (Just 2)
      [LongMode]
      [ op    RO    T_RM64         E_ModRM
      ]
   ]

i_abs_far_call :: X86Insn
i_abs_far_call = i "Absolute far call" "CALL" [0x9A]
   [Legacy]
   [Undef allFlags]
   [legacyEncoding Nothing [0x9A] Nothing
      []
      [ op    RO    T_PTR_16_16    E_Imm ]
   ,legacyEncoding Nothing [0x9A] Nothing
      []
      [ op    RO    T_PTR_16_32    E_Imm ]
   ]

i_abs_ind_far_call :: X86Insn
i_abs_ind_far_call = i "Absolute indirect far call" "CALL" [0xFF]
   [Legacy, LongMode]
   [Undef allFlags]
   [legacyEncoding Nothing [0xFF] (Just 3)
      []
      [ op    RO    T_M16_XX       E_ModRM     ]]

i_extend_signed :: X86Insn
i_extend_signed = i "Extend signed word" "CBW/CWDE/CDQE" [0x98]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x98] Nothing
      []
      [ op    RW    T_Accu         E_Implicit  ]]

i_clac :: X86Insn
i_clac = i "Clear AC flag in EFLAGS register" "CLAC" [0x0F,0x01,0xCA]
   [Legacy, LongMode, Extension SMAP]
   [Zero [AC]]
   [legacyEncoding Nothing [0x0F,0x01,0xCA] Nothing [] []]

i_clc :: X86Insn
i_clc = i "Clear carry flag" "CLC" [0xF8]
   [Legacy, LongMode]
   [Zero [CF]]
   [legacyEncoding Nothing [0xF8] Nothing [] []]

i_cld :: X86Insn
i_cld = i "Clear direction flag" "CLD" [0xFC]
   [Legacy, LongMode]
   [Zero [DF]]
   [legacyEncoding Nothing [0xFC] Nothing [] []]

i_clflush :: X86Insn
i_clflush = i "Flush cache line" "CLFLUSH" [0x0F,0xAE]
   [Legacy, LongMode, Extension CLFLUSH]
   []
   [legacyEncoding Nothing [0x0F,0xAE] (Just 7)
      []
      [ op    RO    T_M      E_ModRM  ]]

i_cli :: X86Insn
i_cli = i "Clear interrupt flag" "CLI" [0xFA]
   [Legacy, LongMode]
   [Zero [IF]]
   [legacyEncoding Nothing [0xFA] Nothing [] []]

i_clts :: X86Insn
i_clts = i "Clear task-switched flag in CR0" "CLTS" [0x0F,0x06]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x06] Nothing [] []]

i_cmc :: X86Insn
i_cmc = i "Complement carry flag" "CMC" [0xF5]
   [Legacy, LongMode]
   [Set [CF]]
   [legacyEncoding Nothing [0xF5] Nothing [] []]

i_cmov :: X86Insn
i_cmov = i "Conditional move" "CMOVcc" [0x0F,0x40]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x40] Nothing
      []
      [ op    RO    T_CC          E_OpCC
      , op    RW    T_R16_32_64   E_ModReg
      , op    RO    T_RM16_32_64  E_ModRM
      ]]

i_cmp :: X86Insn
i_cmp = i "Compare" "CMP" [0x3C]
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing [0x3C] Nothing
      [Sizable 0]
      [ op    RW    T_Accu   E_Implicit
      , op    RO    T_Imm    E_Imm
      ]
   ,legacyEncoding Nothing [0x38] Nothing
      [Sizable 0, Reversable 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_R      E_ModReg
      ]
   ,legacyEncoding Nothing [0x80] (Just 7)
      [Sizable 0, SignExtendableImm8 1, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Imm    E_Imm
      ]
   ]

i_cmppd :: X86Insn
i_cmppd = i "Compare packed double-precision floating-point values" "CMPPD" [0x0F,0xC2]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0xC2] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmppd :: X86Insn
i_vcmppd = i "Compare packed double-precision floating-point values" "VCMPPD" [0x0F,0xC2]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_cmpps :: X86Insn
i_cmpps = i "Compare packed single-precision floating-point values" "CMPPS" [0x0F,0xC2]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing [0x0F,0xC2] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpps :: X86Insn
i_vcmpps = i "Compare packed single-precision floating-point values" "VCMPPS" [0x0F,0xC2]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_cmps :: X86Insn
i_cmps = i "Compare string operands" "CMPS" [0xA6]
   [Legacy, LongMode]
   [Set [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing [0xA6] Nothing
      [Sizable 0]
      [ op    RO    T_rSI    E_Implicit
      , op    RO    T_rDI    E_Implicit
      ]]

i_cmpsd :: X86Insn
i_cmpsd = i "Compare scalar double-precision floating-point values" "CMPSD" [0x0F,0xC2]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0xC2] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpsd :: X86Insn
i_vcmpsd = i "Compare scalar double-precision floating-point values" "VCMPSD" [0x0F,0xC2]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128      E_ModReg
                  , op     RO    T_V128      E_VexV
                  , op     RO    T_VM128     E_ModRM
                  , op     RO    T_Imm8      E_Imm
                  ]]

i_cmpss :: X86Insn
i_cmpss = i "Compare scalar single-precision floating-point values" "CMPSS" [0x0F,0xC2]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0xC2] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      , op    RO    T_Imm8   E_Imm
      ]]

i_vcmpss :: X86Insn
i_vcmpss = i "Compare scalar single-precision floating-point values" "VCMPSS" [0x0F,0xC2]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128      E_ModReg
                  , op     RO    T_V128      E_VexV
                  , op     RO    T_VM128     E_ModRM
                  , op     RO    T_Imm8      E_Imm
                  ]]

i_cmpxchg :: X86Insn
i_cmpxchg = i "Compare and exchange" "CMPXCHG" [0x0F,0xB0]
   [Legacy, LongMode, Arch Intel486]
   [Set [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing [0x0F,0xB0] Nothing
      [Sizable 0, Lockable]
      [ op    RW    T_RM     E_ModRM
      , op    RO    T_Accu   E_Implicit
      , op    RO    T_R      E_ModReg
      ]]

i_cmpxch8b :: X86Insn
i_cmpxch8b = i "Compare and exchange bytes" "CMPXCHG8B/CMPXCHG16B" [0x0F,0xC7]
   [Legacy, LongMode, Arch IntelPentium, Extension CX8]
   [Set [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing [0x0F,0xC7] Nothing
      [DoubleSizable, Lockable]
      [ op    RW    T_M64_128   E_ModRM ]]


i_comisd :: X86Insn
i_comisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "COMISD" [0x0F,0x2F]
   [Legacy, LongMode, Extension SSE2]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [legacyEncoding (Just 0x66) [0x0F,0x2F] Nothing
      []
      [ op    RO    T_V128_Low64      E_ModReg
      , op    RO    T_VM128_Low64     E_ModRM
      ]]

i_vcomisd :: X86Insn
i_vcomisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "VCOMISD" [0x0F,0x2F]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [Vex LWIG      [ op     RO    T_V128_Low64      E_ModReg
                  , op     RO    T_VM128_Low64     E_ModRM
                  ]]

i_comiss :: X86Insn
i_comiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "COMISS" [0x0F,0x2F]
   [Legacy, LongMode, Extension SSE]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [legacyEncoding Nothing [0x0F,0x2F] Nothing
      []
      [ op    RO    T_V128_Low32      E_ModReg
      , op    RO    T_VM128_Low32     E_ModRM
      ]]

i_vcomiss :: X86Insn
i_vcomiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "VCOMISS" [0x0F,0x2F]
   [Legacy, LongMode, Extension AVX]
   [Set [ZF,PF,CF], Zero [OF,SF,AF]]
   [Vex LWIG      [ op     RO    T_V128_Low32      E_ModReg
                  , op     RO    T_VM128_Low32     E_ModRM
                  ]]

i_cpuid :: X86Insn
i_cpuid = i "CPU identification" "CPUID" [0x0F,0xA2]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0xA2] Nothing
      []
      [ op    RW    T_xAX     E_Implicit
      , op    RW    T_xCX     E_Implicit
      , op    WO    T_xBX     E_Implicit
      , op    WO    T_xDX     E_Implicit
      ]]

i_crc32 :: X86Insn
i_crc32 = i "Accumulate CRC32 value" "CRC32" [0x0F,0x38,0xF0]
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x38,0xF0] Nothing
      [Sizable 0]
      [ op    RW    T_R      E_ModReg
      , op    RO    T_RM     E_ModRM
      ]]

i_cvtdq2pd :: X86Insn
i_cvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTDQ2PD" [0x0F,0xE6]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0xE6] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 
      ]]

i_vcvtdq2pd :: X86Insn
i_vcvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "VCVTDQ2PD" [0x0F,0xE6]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 or xmm/m128
                  ]]

i_cvtdq2ps :: X86Insn
i_cvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTDQ2PS" [0x0F,0x5B]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding Nothing [0x0F,0x5B] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtdq2ps :: X86Insn
i_vcvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "VCVTDQ2PS" [0x0F,0x5B]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvtpd2dq :: X86Insn
i_cvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DQ" [0x0F,0xE6]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0xE6] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtpd2dq :: X86Insn
i_vcvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "VCVTPD2DQ" [0x0F,0xE6]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvtpd2di :: X86Insn
i_cvtpd2di = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DI" [0x0F,0x2D]
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x2D] Nothing
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_cvtpd2ps :: X86Insn
i_cvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "CVTPD2PS" [0x0F,0x5A]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x5A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtpd2ps :: X86Insn
i_vcvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "VCVTPD2PS" [0x0F,0x5A]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvtpi2pd :: X86Insn
i_cvtpi2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTPI2PD" [0x0F,0x2A]
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x2A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM64         E_ModRM
      ]]

i_cvtpi2ps :: X86Insn
i_cvtpi2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTPI2PS" [0x0F,0x2A]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x2A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM64         E_ModRM
      ]]

i_cvtps2dq :: X86Insn
i_cvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2DQ" [0x0F,0x5B]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x5B] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtps2dq :: X86Insn
i_vcvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "VCVTPS2DQ" [0x0F,0x5B]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvtps2pd :: X86Insn
i_cvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "CVTPS2PD" [0x0F,0x5A]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding Nothing [0x0F,0x5A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvtps2pd :: X86Insn
i_vcvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "VCVTPS2PD" [0x0F,0x5A]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvtps2pi :: X86Insn
i_cvtps2pi = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2PI" [0x0F,0x2D]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x2D] Nothing
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_cvtsd2si :: X86Insn
i_cvtsd2si = i "Convert scalar double-precision floating-point value to integer" "CVTSD2SI" [0x0F,0x2D]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x2D] Nothing
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvtsd2si :: X86Insn
i_vcvtsd2si = i "Convert scalar double-precision floating-point value to integer" "VCVTSD2SI" [0x0F,0x2D]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_R32_64         E_ModReg
                  , op     RO    T_VM128_Low64    E_ModRM
                  ]]

i_cvtsd2ss :: X86Insn
i_cvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "CVTSD2SS" [0x0F,0x5A]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x5A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvtsd2ss :: X86Insn
i_vcvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "VCVTSD2SS" [0x0F,0x5A]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low64  E_ModRM
                  ]]

i_cvtsi2sd :: X86Insn
i_cvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "CVTSI2SD" [0x0F,0x2A]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x2A] Nothing
      []
      [ op    WO    T_V128      E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_vcvtsi2sd :: X86Insn
i_vcvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "VCVTSI2SD" [0x0F,0x2A]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_V128     E_ModReg
                  , op     RO    T_V128     E_VexV
                  , op     RO    T_RM32_64  E_ModRM
                  ]]


i_cvtsi2ss :: X86Insn
i_cvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "CVTSI2SS" [0x0F,0x2A]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x2A] Nothing
      []
      [ op    WO    T_V128      E_ModReg
      , op    RO    T_RM32_64   E_ModRM
      ]]

i_vcvtsi2ss :: X86Insn
i_vcvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "VCVTSI2SS" [0x0F,0x2A]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_V128     E_ModReg
                  , op     RO    T_V128     E_VexV
                  , op     RO    T_RM32_64  E_ModRM
                  ]]

i_cvtss2sd :: X86Insn
i_cvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "CVTSS2SD" [0x0F,0x5A]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x5A] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvtss2sd :: X86Insn
i_vcvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "VCVTSS2SD" [0x0F,0x5A]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low32  E_ModRM
                  ]]

i_cvtss2si :: X86Insn
i_cvtss2si = i "Convert scalar single-precision floating-point value to Int32" "CVTSS2SI" [0x0F,0x2D]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x2D] Nothing
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvtss2si :: X86Insn
i_vcvtss2si = i "Convert scalar single-precision floating-point value to Int32" "VCVTSS2SI" [0x0F,0x2D]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_R32_64       E_ModReg
                  , op     RO    T_VM128_Low32  E_ModRM
                  ]]

i_cvttpd2dq :: X86Insn
i_cvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2DQ" [0x0F,0xE6]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0xE6] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvttpd2dq :: X86Insn
i_vcvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "VCVTTPD2DQ" [0x0F,0xE6]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvttpd2pi :: X86Insn
i_cvttpd2pi = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2PI" [0x0F,0x2C]
   [Legacy, LongMode]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x2C] Nothing
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_cvttps2dq :: X86Insn
i_cvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2DQ" [0x0F,0x5B]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x5B] Nothing
      []
      [ op    WO    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      ]]

i_vcvttps2dq :: X86Insn
i_vcvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "VCVTTPS2DQ" [0x0F,0x5B]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_cvttps2pi :: X86Insn
i_cvttps2pi = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2PI" [0x0F,0x2C]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x2C] Nothing
      []
      [ op    WO    T_V64          E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_cvttsd2si :: X86Insn
i_cvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "CVTTSD2SI" [0x0F,0x2C]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x2C] Nothing
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vcvttsd2si :: X86Insn
i_vcvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "VCVTTSD2SI" [0x0F,0x2C]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_R32_64         E_ModReg
                  , op     RO    T_VM128_Low64    E_ModRM
                  ]]

i_cvttss2si :: X86Insn
i_cvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "CVTTSS2SI" [0x0F,0x2C]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x2C] Nothing
      []
      [ op    WO    T_R32_64       E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vcvttss2si :: X86Insn
i_vcvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "VCVTTSS2SI" [0x0F,0x2C]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LIG       [ op     WO    T_R32_64       E_ModReg
                  , op     RO    T_VM128_Low32  E_ModRM
                  ]]

i_cwd :: X86Insn
i_cwd = i "Convert between words (sign-extend)" "CWD/CDQ/CQO" [0x99]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x99] Nothing
      []
      [ op    WO    T_xDX_xAX      E_Implicit
      , op    RO    T_AX_EAX_RAX   E_Implicit
      ]]

i_daa :: X86Insn
i_daa = i "Decimal adjust AL after addition" "DAA" [0x27]
   [Legacy]
   [Set [AF,CF,SF,ZF,PF], Undef [OF]]
   [legacyEncoding Nothing [0x27] Nothing
      []
      [ op    RW    T_AL     E_Implicit ]]

i_das :: X86Insn
i_das = i "Decimal adjust AL after subtraction" "DAS" [0x2F]
   [Legacy]
   [Set [AF,CF,SF,ZF,PF], Undef [OF]]
   [legacyEncoding Nothing [0x2F] Nothing
      []
      [ op    RW    T_AL     E_Implicit ]]

i_dec :: X86Insn
i_dec = i "Decrement by 1" "DEC" [0xFE]
   [Legacy, LongMode]
   [Set [OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing [0xFE] (Just 1)
      [Sizable 0, Lockable]
      [ op    RW    T_RM     E_ModRM
      ]
   ,legacyEncoding Nothing [0x48] Nothing
      [Legacy,Lockable]
      [ op    RW    T_R16_32    E_OpReg
      ]
   ]

i_div :: X86Insn
i_div = i "Unsigned divide" "DIV" [0xF6]
   [Legacy, LongMode, FailOnZero 0]
   [Undef [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing [0xF6] (Just 6)
      [Sizable 0, Lockable]
      [ op    RO    T_RM        E_ModRM 
      , op    RW    T_xDX_xAX   E_Implicit
      ]]

i_divpd :: X86Insn
i_divpd = i "Divide packed double-precision floating-point values" "DIVPD" [0x0F,0x5E]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x5E] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vdivpd :: X86Insn
i_vdivpd = i "Divide packed double-precision floating-point values" "VDIVPD" [0x0F,0x5E]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_divps :: X86Insn
i_divps = i "Divide packed float-precision floating-point values" "DIVPS" [0x0F,0x5E]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding Nothing [0x0F,0x5E] Nothing
      []
      [ op    RW    T_V128   E_ModReg
      , op    RO    T_VM128  E_ModRM
      ]]

i_vdivps :: X86Insn
i_vdivps = i "Divide packed float-precision floating-point values" "VDIVPS" [0x0F,0x5E]
   [Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  ]]

i_divsd :: X86Insn
i_divsd = i "Divide scalar double-precision floating-point values" "DIVSD" [0x0F,0x5E]
   [Legacy, LongMode, Extension SSE2]
   []
   [legacyEncoding (Just 0xF2) [0x0F,0x5E] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low64  E_ModRM
      ]]

i_vdivsd :: X86Insn
i_vdivsd = i "Divide scalar double-precision floating-point values" "VDIVSD" [0x0F,0x5E]
   [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low64  E_ModRM
                  ]]

i_divss :: X86Insn
i_divss = i "Divide scalar single-precision floating-point values" "DIVSS" [0x0F,0x5E]
   [Legacy, LongMode, Extension SSE]
   []
   [legacyEncoding (Just 0xF3) [0x0F,0x5E] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128_Low32  E_ModRM
      ]]

i_vdivss :: X86Insn
i_vdivss = i "Divide scalar single-precision floating-point values" "VDIVSS" [0x0F,0x5E]
   [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
   []
   [Vex LWIG      [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128_Low32  E_ModRM
                        ]]
i_dppd :: X86Insn
i_dppd = i "Dot product of packed double precision floating-point values" "DPPD" [0x0F,0x3A,0x41]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0x41] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vdppd :: X86Insn
i_vdppd = i "Dot product of packed double precision floating-point values" "VDPPD" [0x0F,0x3A,0x41]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128         E_ModReg
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_VM128        E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_dpps :: X86Insn
i_dpps = i "Dot product of packed single precision floating-point values" "DPPS" [0x0F,0x3A,0x40]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0x40] Nothing
      []
      [ op    RW    T_V128         E_ModReg
      , op    RO    T_VM128        E_ModRM
      , op    RO    T_Imm8         E_Imm
      ]]

i_vdpps :: X86Insn
i_vdpps = i "Dot product of packed single precision floating-point values" "VDPPS" [0x0F,0x3A,0x40]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                  , op     RO    T_V128_256     E_VexV
                  , op     RO    T_VM128_256    E_ModRM
                  , op     RO    T_Imm8         E_Imm
                  ]]

i_emms :: X86Insn
i_emms = i "Empty MMX technology state" "EMMS" [0x0F,0x77]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0x0F,0x77] Nothing [] []]

i_enter :: X86Insn
i_enter = i "Make stack frame for procedure parameters" "ENTER" [0xC8]
   [Legacy, LongMode]
   []
   [legacyEncoding Nothing [0xC8] Nothing
      []
      [ op    RO    T_Imm16     E_Imm
      , op    RO    T_Imm8      E_Imm
      ]]

i_extractps :: X86Insn
i_extractps = i "Extract packed single precision floating-point value" "EXTRACTPS" [0x0F,0x3A,0x17]
   [Legacy, LongMode, Extension SSE4_1]
   []
   [legacyEncoding (Just 0x66) [0x0F,0x3A,0x17] Nothing
      []
      [ op    RW    T_RM32         E_ModRM
      , op    RO    T_V128         E_ModReg
      , op    RO    T_Imm8         E_Imm
      ]]

i_vextractps :: X86Insn
i_vextractps = i "Extract packed single precision floating-point value" "VEXTRACTPS" [0x0F,0x3A,0x17]
   [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
   []
   [Vex WIG       [ op     WO    T_RM32         E_ModRM
                  , op     RO    T_V128         E_VexV
                  , op     RO    T_Imm8         E_Imm
                  ]]



makeParser :: [X86Insn] -> Tree PTree
makeParser insns = makeTree insns
   where
      -- generate multiple opcodes from insn properties and encodings
      multiOpcode :: X86Insn -> [X86Insn]
      multiOpcode i = fmap (\o' -> i {iOpcode = ini ++ [o']}) ops
         where
            ops = o : recProp [] (iProperties i) ++
                      recEnc [] (getOperandEncodings i)

            o   = last (iOpcode i)
            ini = init (iOpcode i)

            recEnc os [] = os
            recEnc os (e:es) = recEnc os' es
               where
                  os' = case e of
                     E_OpReg -> fmap (o+) [1..7] ++ os
                     E_OpCC  -> fmap (o+) [1..15] ++ os
                     _       -> os

            recProp os []     = os
            recProp os (p:ps) = recProp (os' ++ os) ps
               where 
                  os' = case p of
                     Sizable n -> [setBit o n]

                     SignExtendableImm8 n -> [setBit o' n]
                        where
                           --find associated Sizable
                           isSizable (Sizable _) = True
                           isSizable _           = False
                           Sizable szb = head (filter isSizable (iProperties i))
                           o' = setBit o szb

                     Reversable n -> [setBit o n]

                     _  -> []

      -- drop the first opcode of all the given instructions
      dropOpcode []     = []
      dropOpcode (i:is) = 
         case iOpcode i of
            [] -> dropOpcode is
            o' -> (i { iOpcode = tail o'}) : dropOpcode is

      cmp x y = tst x == tst y
      tst = head . iOpcode

      rec :: [X86Insn] -> [Tree PTree]
      rec is = (leaves ++ gs)
         where
            (ls,ns) = List.partition (null . iOpcode) is

            leaves = fmap (\x -> Node (PInsn x) []) ls

            -- instruction grouped by first opcode
            gs = fmap f $ groupBy cmp ns

            f :: [X86Insn] -> Tree PTree
            f xs = Node (POpcode (head (iOpcode (head xs)))) (rec (dropOpcode xs))

      makeTree :: [X86Insn] -> Tree PTree
      makeTree is = Node PRoot (rec (concatMap multiOpcode is))


data PTree
   = PRoot           -- ^ Root of the parsing tree
   | POpcode Word8   -- ^ Parse opcode byte
   | PInsn X86Insn   -- ^ Candidate instruction
   deriving (Show)

data CmpOp
   = CmpEqual
   | CmpNotEqual
   | CmpGreater
   | CmpGreaterOrEqual
   | CmpSmaller
   | CmpSmallerOrEqual
   deriving (Show,Eq)

data ParsingTree a
   = PByte (Map Word8 (ParsingTree a))                      -- ^ Full byte
   | PPartial [(Word8, CmpOp, Map Word8 (ParsingTree a))]   -- ^ Masked byte
   | PCandidates [a]
   deriving (Show)

