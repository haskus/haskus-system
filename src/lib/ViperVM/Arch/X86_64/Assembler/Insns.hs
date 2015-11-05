module ViperVM.Arch.X86_64.Assembler.Insns
   ( X86Insn(..)
   , X86Extension(..)
   , Properties(..)
   , Flag(..)
   , FlagOp(..)
   , X87Flag(..)
   , X87Insn(..)
   , OperandType(..)
   , OperandEnc(..)
   , Operand(..)
   , AccessMode(..)
   , Encoding(..)
   , insnOpcodeMap
   , instructions
   , requireModRM
   -- x87
   , X87Instruction (..)
   , M16INT(..)
   , M32INT(..)
   , M64INT(..)
   , M32FP(..)
   , M64FP(..)
   , M80FP(..)
   , M80DEC(..)
   , M80BCD(..)
   , MSTATE(..)
   , MCW(..)
   , MSW(..)
   , MENV(..)
   , ST(..)
   )
where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List ((\\), foldl')

import ViperVM.Arch.X86_64.Assembler.ModRM

data X86Insn = X86Insn
   { iDesc        :: String
   , iMnemonic    :: String
   , iOpcode      :: [Word8]
   , iProperties  :: [Properties]
   , iFlags       :: [FlagOp Flag]
   , iEncoding    :: [Encoding]
   } deriving (Show)

data X87Insn = X87Insn
   { x87Desc        :: String
   , x87Mnemonic    :: String
   , x87Opcode      :: [Word8]
   , x87Properties  :: [Properties]
   , x87FPUFlags    :: [FlagOp X87Flag]
   , x87Flags       :: [FlagOp Flag]
   , x87Encoding    :: [Encoding]
   } deriving (Show)

data Properties
   = LongMode                 -- ^ Supported in 64 bit mode
   | Legacy                   -- ^ Supported in legacy/compatibility mode
   | MandatoryPrefix Word8    -- ^ Mandatory prefix
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand in used)
   | Reversable Int           -- ^ Args are reversed if the given bit is set in the opcode.
                              --   For x87 instructions, it is only valid if both operands are registers
   | SignExtendableImm8 Int   -- ^ Used in conjunction with a set Sizable bit.
                              --   Imm8 operand is used and sign-extended if the
                              --   given bit is set
   | Sizable Int              -- ^ Operand size is 8 if the given bit is unset in the
                              --   opcode. Otherwise, the size is defined by operand-size
                              --   prefix and REX.W bit
   | DoubleSizable            -- ^ Default size is 32+32 (a pair of registers is used). Can be extended to 64+64 with Rex.W
   | OpExt Word8              -- ^ Opcode extension in the ModRM.reg field
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

data X87Flag
   = C0 | C1 | C2 | C3
   deriving (Show,Eq)

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
   = LegacyEncoding [Operand]
   | Vex VexLW [Operand]
   deriving (Show)


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

      matchLegEnc (LegacyEncoding x) = any matchEnc x
      matchLegEnc _                  = False

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
   [ i "ASCII adjust AL after addition" "AAA" [0x37]
         [Legacy]
         [Set [AF,CF], Undef [OF,SF,ZF,PF]]
         [LegacyEncoding [ op    RW    T_AX     E_Implicit ]]

   , i "ASCII adjust AX before division" "AAD" [0xD5]
         [Legacy]
         [Set [SF,ZF,PF], Undef [OF,AF,CF]]
         [LegacyEncoding [ op    RW    T_AX     E_Implicit
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "ASCII adjust AX after multiply" "AAM" [0xD4]
         [Legacy, FailOnZero 0]
         [Set [SF,ZF,PF], Undef [OF,AF,CF]]
         [LegacyEncoding [ op    RW    T_AX     E_Implicit
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "ASCII adjust AL after subtraction" "AAS" [0x3F]
         [Legacy]
         [Set [AF,CF], Undef [OF,SF,ZF,PF]]
         [LegacyEncoding [ op    RW    T_AX     E_Implicit
                         ]]

   , i "Add with carry in accumulator" "ADC" [0x14]
         [Legacy, LongMode, Sizable 0]
         [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_Accu   E_Implicit
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Add with carry without immediate" "ADC" [0x10]
         [Legacy, LongMode, Lockable, Sizable 0, Reversable 1]
         [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_R      E_ModReg
                         ]]

   , i "Add with carry with immediate" "ADC" [0x80]
         [OpExt 2, Legacy, LongMode, Lockable, Sizable 0, SignExtendableImm8 2]
         [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Unsigned integer addition with carry flags" "ADCX" [0x0F,0x38,0xF6]
         [MandatoryPrefix 0x66, Extension ADX]
         [Read [CF], Set [CF]]
         [LegacyEncoding [ op    RW    T_R32_64    E_ModReg
                         , op    RO    T_RM32_64   E_ModRM
                         ]]

   , i "Add in accumulator" "ADD" [0x04]
         [Legacy, LongMode, Sizable 0]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_Accu   E_Implicit
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Add without immediate" "ADD" [0x00]
         [Legacy, LongMode, Lockable, Sizable 0, Reversable 1]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_R      E_ModReg
                         ]]

   , i "Add with immediate" "ADD" [0x80]
         [OpExt 0, Legacy, LongMode, Lockable, Sizable 0, SignExtendableImm8 2]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Add packed double-precision floating-point values" "ADDPD" [0x0F,0x58]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Add packed double-precision floating-point values" "VADDPD" [0x0F,0x58]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Add packed float-precision floating-point values" "ADDPS" [0x0F,0x58]
         [Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Add packed float-precision floating-point values" "VADDPS" [0x0F,0x58]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Add scalar double-precision floating-point values" "ADDSD" [0x0F,0x58]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Add scalar double-precision floating-point values" "VADDSD" [0x0F,0x58]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low64  E_ModRM
                        ]]

   , i "Add scalar single-precision floating-point values" "ADDSS" [0x0F,0x58]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128_Low32  E_ModRM
                         ]]

   , i "Add scalar single-precision floating-point values" "VADDSS" [0x0F,0x58]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low32  E_ModRM
                        ]]

   , i "Packed double-FP add/subtract" "ADDSUBPD" [0x0F,0xD0]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE3]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Packed double-FP add/subtract" "VADDSUBPD" [0x0F,0xD0]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]
   , i "Packed single-FP add/subtract" "ADDSUBPS" [0x0F,0xD0]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE3]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Packed single-FP add/subtract" "VADDSUBPS" [0x0F,0xD0]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Unsigned integer addition of two operands with overflow flag" "ADOX" [0x0F,0x38,0xF6]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension ADX]
         [Read [OF], Set [OF]]
         [LegacyEncoding [ op    RW    T_R32_64       E_ModReg
                         , op    RO    T_RM32_64      E_ModRM
                         ]]

   , i "Perform one round of an AES decryption flow" "AESDEC" [0x0F,0x38,0xDE]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Perform one round of an AES decryption flow" "VAESDEC" [0x0F,0x38,0xDE]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128        E_ModRM
                        ]]

   , i "Perform last round of an AES decryption flow" "AESDECLAST" [0x0F,0x38,0xDF]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Perform last round of an AES decryption flow" "VAESDECLAST" [0x0F,0x38,0xDF]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128        E_ModRM
                        ]]

   , i "Perform one round of an AES encryption flow" "AESENC" [0x0F,0x38,0xDC]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Perform one round of an AES encryption flow" "VAESENC" [0x0F,0x38,0xDC]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128        E_ModRM
                        ]]

   , i "Perform last round of an AES encryption flow" "AESENCLAST" [0x0F,0x38,0xDD]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Perform last round of an AES encryption flow" "VAESENCLAST" [0x0F,0x38,0xDD]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128        E_ModRM
                        ]]

   , i "Perform the AES InvMixColumn transformation" "AESIMC" [0x0F,0x38,0xDB]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Perform the AES InvMixColumn transformation" "VAESIMC" [0x0F,0x38,0xDB]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_VM128        E_ModRM
                        ]]

   , i "AES round key generation assist" "AESKEYGENASSIST" [0x0F,0x3A,0xDF]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "AES round key generation assist" "VAESKEYGENASSIST" [0x0F,0x3A,0xDF]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AES, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_VM128        E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Logical AND with accumulator" "AND" [0x24]
         [Legacy, LongMode, Sizable 0]
         [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_Accu   E_Implicit
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Logical AND without immediate" "AND" [0x20]
         [Legacy, LongMode, Lockable, Sizable 0, Reversable 1]
         [Read [CF], Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_R      E_ModReg
                         ]]

   , i "Logical AND with immediate" "AND" [0x80]
         [OpExt 4, Legacy, LongMode, Lockable, Sizable 0, SignExtendableImm8 2]
         [Zero [OF,CF], Set [SF,ZF,PF], Undef [AF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Logical AND NOT" "ANDN" [0x0F,0x38,0xF2]
         [Legacy, LongMode, Extension BMI1]
         [Set [SF,ZF], Zero [OF,CF], Undef [AF,PF]]
         [Vex L0        [ op    WO    T_R32_64     E_ModReg
                        , op    RO    T_R32_64     E_VexV
                        , op    RO    T_RM32_64    E_ModRM
                        ]]

   , i "Bitwise logical AND of packed double-precision floating-point values" "ANDPD" [0x0F,0x54]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Bitwise logical AND of packed double-precision floating-point values" "VANDPD" [0x0F,0x54]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Bitwise logical AND of packed float-precision floating-point values" "ANDPS" [0x0F,0x54]
         [Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Bitwise logical AND of packed float-precision floating-point values" "VANDPS" [0x0F,0x54]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Bitwise logical AND NOT of packed double-precision floating-point values" "ANDNPD" [0x0F,0x55]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Bitwise logical AND NOT of packed double-precision floating-point values" "VANDNPD" [0x0F,0x55]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Bitwise logical AND of packed float-precision floating-point values" "ANDNPS" [0x0F,0x55]
         [Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Bitwise logical AND of packed float-precision floating-point values" "VANDNPS" [0x0F,0x55]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Adjust RPL field of segment selector" "ARPL" [0x63]
         [Legacy]
         [Set [ZF]]
         [LegacyEncoding [ op    RW    T_RM16   E_ModRM
                         , op    RO    T_R16    E_ModReg
                         ]]

   , i "Blend packed double-precision floating-point values" "BLENDPD" [0x0F,0x3A,0x0D]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Blend packed double-precision floating-point values" "VBLENDPD" [0x0F,0x3A,0x0D]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_Imm8         E_Imm8_3_0
                        ]]

   , i "Bit field extract" "BEXTR" [0x0F,0x38,0xF7]
         [Legacy, LongMode, Extension BMI1]
         [Set [ZF], Undef [AF,SF,PF], Zero (allFlags \\ [ZF,AF,SF,PF])]
         [Vex L0        [ op    WO    T_R32_64     E_ModReg
                        , op    RO    T_RM32_64    E_ModRM
                        , op    RO    T_R32_64     E_VexV
                        ]]

   , i "Blend packed single-precision floating-point values" "BLENDPS" [0x0F,0x3A,0x0C]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Blend packed single-precision floating-point values" "VBLENDPS" [0x0F,0x3A,0x0C]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Variable blend packed double-precision floating-point values" "BLENDVPD" [0x0F,0x38,0x15]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_XMM0   E_Implicit
                         ]]

   , i "Variable blend packed double-precision floating-point values" "VBLENDVPD" [0x0F,0x3A,0x4B]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex W0        [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_V128_256     E_Imm8_7_4
                        ]]

   , i "Variable blend packed single-precision floating-point values" "BLENDVPS" [0x0F,0x38,0x14]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_XMM0   E_Implicit
                         ]]

   , i "Variable blend packed single-precision floating-point values" "VBLENDVPS" [0x0F,0x3A,0x4A]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex W0        [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_V128_256     E_Imm8_7_4
                        ]]

   , i "Extract lowest set isolated bit" "BLSI" [0x0F,0x38,0xF3]
         [OpExt 3, Legacy, LongMode, Extension BMI1]
         [Set [ZF,SF, CF], Zero [OF], Undef [AF,PF]]
         [Vex L0        [ op    WO    T_R32_64     E_VexV
                        , op    RO    T_RM32_64    E_ModRM
                        ]]

   , i "Get mask up to lowest set bit" "BLSMSK" [0x0F,0x38,0xF3]
         [OpExt 2, Legacy, LongMode, Extension BMI1]
         [Set [SF,CF], Zero [ZF,OF], Undef [AF,PF]]
         [Vex L0        [ op    WO    T_R32_64     E_VexV
                        , op    RO    T_RM32_64    E_ModRM
                        ]]

   , i "Reset lowest set bit" "BLSR" [0x0F,0x38,0xF3]
         [OpExt 1, Legacy, LongMode, Extension BMI1]
         [Set [ZF,SF,CF], Zero [OF], Undef [AF,PF]]
         [Vex L0        [ op    WO    T_R32_64     E_VexV
                        , op    RO    T_RM32_64    E_ModRM
                        ]]

   , i "Check array index against bounds" "BOUND" [0x62]
         [Legacy]
         []
         [LegacyEncoding [ op    RO    T_R16_32 E_ModReg
                         , op    RO    T_M_PAIR E_ModRM
                         ]]

   , i "Bit scan forward" "BSF" [0x0F,0xBC]
         [Legacy, LongMode]
         [Set [ZF], Undef [CF,OF,SF,AF,PF]]
         [LegacyEncoding [ op    WO    T_R      E_ModReg
                         , op    RO    T_RM     E_ModRM
                         ]]

   , i "Bit scan reverse" "BSR" [0x0F,0xBD]
         [Legacy, LongMode]
         [Set [ZF], Undef [CF,OF,SF,AF,PF]]
         [LegacyEncoding [ op    WO    T_R      E_ModReg
                         , op    RO    T_RM     E_ModRM
                         ]]

   , i "Byte swap" "BSWAP" [0x0F,0xC8]
         [Legacy, LongMode, Arch Intel486]
         []
         [LegacyEncoding [ op    RW    T_R32_64 E_OpReg
                         ]]

   , i "Bit test" "BT" [0x0F,0xA3]
         [Legacy, LongMode]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RO    T_RM16_32_64   E_ModRM
                         , op    RO    T_R16_32_64    E_ModReg
                         ]]

   , i "Bit test with immediate index" "BT" [0x0F,0xBA]
         [OpExt 4, Legacy, LongMode]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RO    T_RM16_32_64   E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Bit test and complement" "BTC" [0x0F,0xBB]
         [Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_R16_32_64    E_ModReg
                         ]]

   , i "Bit test with immediate index and complement" "BTC" [0x0F,0xBA]
         [OpExt 7, Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Bit test and reset" "BTR" [0x0F,0xB3]
         [Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_R16_32_64    E_ModReg
                         ]]

   , i "Bit test with immediate index and reset" "BTR" [0x0F,0xBA]
         [OpExt 6, Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Bit test and set" "BTS" [0x0F,0xAB]
         [Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_R16_32_64    E_ModReg
                         ]]

   , i "Bit test with immediate index and set" "BTS" [0x0F,0xBA]
         [OpExt 5, Legacy, LongMode, Lockable]
         [Set [CF], Undef [OF,SF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM16_32_64   E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Zero high bits starting with specified bit position" "BZHI" [0x0F,0x38,0xF5]
         [Legacy, LongMode, Extension BMI2]
         [Set [ZF,CF,SF], Zero [OF], Undef [AF,PF]]
         [Vex L0        [ op    WO    T_R32_64     E_ModReg
                        , op    RO    T_RM32_64    E_ModRM
                        , op    RO    T_R32_64     E_VexV
                        ]]

   , i "Relative near call" "CALL" [0xE8]
         [Legacy, LongMode]
         [Undef allFlags]
         [LegacyEncoding [ op    RO    T_REL_16_32    E_ModRM ]]

   , i "Indirect near call" "CALL" [0xFF]
         [OpExt 2, Legacy]
         [Undef allFlags]
         [LegacyEncoding [ op    RO    T_RM16_32      E_ModRM ]]

   , i "Indirect near call" "CALL" [0xFF]
         [OpExt 2, LongMode]
         [Undef allFlags]
         [LegacyEncoding [ op    RO    T_RM64         E_ModRM ]]

   , i "Absolute far call" "CALL" [0x9A]
         [Legacy]
         [Undef allFlags]
         [LegacyEncoding [ op    RO    T_PTR_16_16    E_Imm ]
         ,LegacyEncoding [ op    RO    T_PTR_16_32    E_Imm ]
         ]

   , i "Absolute indirect far call" "CALL" [0xFF]
         [OpExt 3, Legacy, LongMode]
         [Undef allFlags]
         [LegacyEncoding [ op    RO    T_M16_XX       E_ModRM     ]]

   , i "Extend signed word" "CBW/CWDE/CDQE" [0x98]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    RW    T_Accu         E_Implicit  ]]

   , i "Clear AC flag in EFLAGS register" "CLAC" [0x0F,0x01,0xCA]
         [Legacy, LongMode, Extension SMAP]
         [Zero [AC]]
         [LegacyEncoding []]

   , i "Clear carry flag" "CLC" [0xF8]
         [Legacy, LongMode]
         [Zero [CF]]
         [LegacyEncoding []]

   , i "Clear direction flag" "CLD" [0xFC]
         [Legacy, LongMode]
         [Zero [DF]]
         [LegacyEncoding []]

   , i "Flush cache line" "CLFLUSH" [0x0F,0xAE]
         [OpExt 7, Legacy, LongMode, Extension CLFLUSH]
         []
         [LegacyEncoding [ op    RO    T_M      E_ModRM  ]]

   , i "Clear interrupt flag" "CLI" [0xFA]
         [Legacy, LongMode]
         [Zero [IF]]
         [LegacyEncoding []]

   , i "Clear task-switched flag in CR0" "CLTS" [0x0F,0x06]
         [Legacy, LongMode]
         []
         [LegacyEncoding []]

   , i "Complement carry flag" "CMC" [0xF5]
         [Legacy, LongMode]
         [Set [CF]]
         [LegacyEncoding []]

   , i "Conditional move" "CMOVcc" [0x0F,0x40]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    RO    T_CC          E_OpCC
                         , op    RW    T_R16_32_64   E_ModReg
                         , op    RO    T_RM16_32_64  E_ModRM
                         ]]

   , i "Compare immediate with accumulator" "CMP" [0x3C]
         [Legacy, LongMode, Sizable 0]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_Accu   E_Implicit
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Compare two operands without immediate" "CMP" [0x38]
         [Legacy, LongMode, Lockable, Sizable 0, Reversable 1]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_R      E_ModReg
                         ]]

   , i "Compare operand with immediate" "CMP" [0x80]
         [OpExt 7, Legacy, LongMode, Lockable, Sizable 0, SignExtendableImm8 2]
         [Set [OF,SF,ZF,AF,CF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_Imm    E_Imm
                         ]]

   , i "Compare packed double-precision floating-point values" "CMPPD" [0x0F,0xC2]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Compare packed double-precision floating-point values" "VCMPPD" [0x0F,0xC2]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Compare packed single-precision floating-point values" "CMPPS" [0x0F,0xC2]
         [Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Compare packed single-precision floating-point values" "VCMPPS" [0x0F,0xC2]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Compare string operands" "CMPS" [0xA6]
         [Legacy, LongMode, Sizable 0]
         [Set [CF,OF,SF,ZF,AF,PF]]
         [LegacyEncoding []]

   , i "Compare scalar double-precision floating-point values" "CMPSD" [0x0F,0xC2]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Compare scalar double-precision floating-point values" "VCMPSD" [0x0F,0xC2]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128      E_ModReg
                        , op     RO    T_V128      E_VexV
                        , op     RO    T_VM128     E_ModRM
                        , op     RO    T_Imm8      E_Imm
                        ]]

   , i "Compare scalar single-precision floating-point values" "CMPSS" [0x0F,0xC2]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         , op    RO    T_Imm8   E_Imm
                         ]]

   , i "Compare scalar single-precision floating-point values" "VCMPSS" [0x0F,0xC2]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128      E_ModReg
                        , op     RO    T_V128      E_VexV
                        , op     RO    T_VM128     E_ModRM
                        , op     RO    T_Imm8      E_Imm
                        ]]

   , i "Compare and exchange" "CMPXCHG" [0x0F,0xB0]
         [Legacy, LongMode, Lockable, Sizable 0, Arch Intel486]
         [Set [ZF,CF,PF,AF,SF,OF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM
                         , op    RO    T_Accu   E_Implicit
                         , op    RO    T_R      E_ModReg
                         ]]

   , i "Compare and exchange bytes" "CMPXCHG8B/CMPXCHG16B" [0x0F,0xC7]
         [Legacy, LongMode, Lockable, Arch IntelPentium, DoubleSizable, Extension CX8]
         [Set [ZF,CF,PF,AF,SF,OF]]
         [LegacyEncoding [ op    RW    T_M64_128   E_ModRM ]]


   , i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "COMISD" [0x0F,0x2F]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         [Set [ZF,PF,CF], Zero [OF,SF,AF]]
         [LegacyEncoding [ op    RO    T_V128_Low64      E_ModReg
                         , op    RO    T_VM128_Low64     E_ModRM
                         ]]

   , i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "VCOMISD" [0x0F,0x2F]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         [Set [ZF,PF,CF], Zero [OF,SF,AF]]
         [Vex LWIG      [ op     RO    T_V128_Low64      E_ModReg
                        , op     RO    T_VM128_Low64     E_ModRM
                        ]]

   , i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "COMISS" [0x0F,0x2F]
         [Legacy, LongMode, Extension SSE]
         [Set [ZF,PF,CF], Zero [OF,SF,AF]]
         [LegacyEncoding [ op    RO    T_V128_Low32      E_ModReg
                         , op    RO    T_VM128_Low32     E_ModRM
                         ]]

   , i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "VCOMISS" [0x0F,0x2F]
         [Legacy, LongMode, Extension AVX]
         [Set [ZF,PF,CF], Zero [OF,SF,AF]]
         [Vex LWIG      [ op     RO    T_V128_Low32      E_ModReg
                        , op     RO    T_VM128_Low32     E_ModRM
                        ]]

   , i "CPU identification" "CPUID" [0x0F,0xA2]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    RW    T_xAX     E_Implicit
                         , op    RW    T_xCX     E_Implicit
                         , op    WO    T_xBX     E_Implicit
                         , op    WO    T_xDX     E_Implicit
                         ]]

   , i "Accumulate CRC32 value" "CRC32" [0x0F,0x38,0xF0]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Sizable 0]
         []
         [LegacyEncoding [ op    RW    T_R      E_ModReg
                         , op    RO    T_RM     E_ModRM
                         ]]

   , i "Convert packed Int32 to packed double-precision floating-point values" "CVTDQ2PD" [0x0F,0xE6]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 
                         ]]

   , i "Convert packed Int32 to packed double-precision floating-point values" "VCVTDQ2PD" [0x0F,0xE6]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128        E_ModRM     -- FIXME: it should be xmm_low64/m64 or xmm/m128
                        ]]

   , i "Convert packed Int32 to packed single-precision floating-point values" "CVTDQ2PS" [0x0F,0x5B]
         [Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed Int32 to packed single-precision floating-point values" "VCVTDQ2PS" [0x0F,0x5B]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DQ" [0x0F,0xE6]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed double-precision floating-point values to packed Int32" "VCVTPD2DQ" [0x0F,0xE6]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DI" [0x0F,0x2D]
         [MandatoryPrefix 0x66, Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V64          E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "CVTPD2PS" [0x0F,0x5A]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "VCVTPD2PS" [0x0F,0x5A]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert packed Int32 to packed double-precision floating-point values" "CVTPI2PD" [0x0F,0x2A]
         [MandatoryPrefix 0x66, Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM64         E_ModRM
                         ]]

   , i "Convert packed Int32 to packed single-precision floating-point values" "CVTPI2PS" [0x0F,0x2A]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM64         E_ModRM
                         ]]

   , i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2DQ" [0x0F,0x5B]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed single-precision floating-point values to packed Int32" "VCVTPS2DQ" [0x0F,0x5B]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "CVTPS2PD" [0x0F,0x5A]
         [Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "VCVTPS2PD" [0x0F,0x5A]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2PI" [0x0F,0x2D]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V64          E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Convert scalar double-precision floating-point value to integer" "CVTSD2SI" [0x0F,0x2D]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_R32_64       E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Convert scalar double-precision floating-point value to integer" "VCVTSD2SI" [0x0F,0x2D]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_R32_64         E_ModReg
                        , op     RO    T_VM128_Low64    E_ModRM
                        ]]

   , i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "CVTSD2SS" [0x0F,0x5A]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "VCVTSD2SS" [0x0F,0x5A]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low64  E_ModRM
                        ]]

   , i "Convert Int32 to scalar double-precision floating-point value" "CVTSI2SD" [0x0F,0x2A]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128      E_ModReg
                         , op    RO    T_RM32_64   E_ModRM
                         ]]

   , i "Convert Int32 to scalar double-precision floating-point value" "VCVTSI2SD" [0x0F,0x2A]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_V128     E_ModReg
                        , op     RO    T_V128     E_VexV
                        , op     RO    T_RM32_64  E_ModRM
                        ]]


   , i "Convert Int32 to scalar single-precision floating-point value" "CVTSI2SS" [0x0F,0x2A]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128      E_ModReg
                         , op    RO    T_RM32_64   E_ModRM
                         ]]

   , i "Convert Int32 to scalar single-precision floating-point value" "VCVTSI2SS" [0x0F,0x2A]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_V128     E_ModReg
                        , op     RO    T_V128     E_VexV
                        , op     RO    T_RM32_64  E_ModRM
                        ]]

   , i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "CVTSS2SD" [0x0F,0x5A]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128_Low32  E_ModRM
                         ]]

   , i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "VCVTSS2SD" [0x0F,0x5A]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low32  E_ModRM
                        ]]

   , i "Convert scalar single-precision floating-point value to Int32" "CVTSS2SI" [0x0F,0x2D]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    WO    T_R32_64       E_ModReg
                         , op    RO    T_VM128_Low32  E_ModRM
                         ]]

   , i "Convert scalar single-precision floating-point value to Int32" "VCVTSS2SI" [0x0F,0x2D]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_R32_64       E_ModReg
                        , op     RO    T_VM128_Low32  E_ModRM
                        ]]

   , i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2DQ" [0x0F,0xE6]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert with truncation packed double-precision floating-point values to packed Int32" "VCVTTPD2DQ" [0x0F,0xE6]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2PI" [0x0F,0x2C]
         [MandatoryPrefix 0x66, Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V64          E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2DQ" [0x0F,0x5B]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         ]]

   , i "Convert with truncation packed single-precision floating-point values to packed Int32" "VCVTTPS2DQ" [0x0F,0x5B]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2PI" [0x0F,0x2C]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_V64          E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Convert with truncation scalar double-precision floating-point value to integer" "CVTTSD2SI" [0x0F,0x2C]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    WO    T_R32_64       E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Convert with truncation scalar double-precision floating-point value to integer" "VCVTTSD2SI" [0x0F,0x2C]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_R32_64         E_ModReg
                        , op     RO    T_VM128_Low64    E_ModRM
                        ]]

   , i "Convert with truncation scalar single-precision floating-point value to Int32" "CVTTSS2SI" [0x0F,0x2C]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    WO    T_R32_64       E_ModReg
                         , op    RO    T_VM128_Low32  E_ModRM
                         ]]

   , i "Convert with truncation scalar single-precision floating-point value to Int32" "VCVTTSS2SI" [0x0F,0x2C]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LIG       [ op     WO    T_R32_64       E_ModReg
                        , op     RO    T_VM128_Low32  E_ModRM
                        ]]

   , i "Convert between words (sign-extend)" "CWD/CDQ/CQO" [0x99]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    WO    T_xDX_xAX      E_Implicit
                         , op    RO    T_AX_EAX_RAX   E_Implicit
                         ]]

   , i "Decimal adjust AL after addition" "DAA" [0x27]
         [Legacy]
         [Set [AF,CF,SF,ZF,PF], Undef [OF]]
         [LegacyEncoding [ op    RW    T_AL     E_Implicit ]]

   , i "Decimal adjust AL after subtraction" "DAS" [0x2F]
         [Legacy]
         [Set [AF,CF,SF,ZF,PF], Undef [OF]]
         [LegacyEncoding [ op    RW    T_AL     E_Implicit ]]

   , i "Decrement by 1" "DEC" [0xFE]
         [OpExt 1, Legacy, LongMode, Lockable, Sizable 0]
         [Set [OF,SF,ZF,AF,PF]]
         [LegacyEncoding [ op    RW    T_RM     E_ModRM ]]

   , i "Decrement by 1" "DEC" [0x48]
         [Legacy, Lockable]
         [Set [OF,SF,ZF,AF,PF]]
         [LegacyEncoding [ op    RW    T_R16_32    E_OpReg ]]

   , i "Unsigned divide" "DIV" [0xF6]
         [OpExt 6, Legacy, LongMode, Lockable, Sizable 0, FailOnZero 0]
         [Undef [CF,OF,SF,ZF,AF,PF]]
         [LegacyEncoding [ op    RO    T_RM        E_ModRM 
                         , op    RW    T_xDX_xAX   E_Implicit
                         ]]

   , i "Divide packed double-precision floating-point values" "DIVPD" [0x0F,0x5E]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Divide packed double-precision floating-point values" "VDIVPD" [0x0F,0x5E]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Divide packed float-precision floating-point values" "DIVPS" [0x0F,0x5E]
         [Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128   E_ModReg
                         , op    RO    T_VM128  E_ModRM
                         ]]

   , i "Divide packed float-precision floating-point values" "VDIVPS" [0x0F,0x5E]
         [Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        ]]

   , i "Divide scalar double-precision floating-point values" "DIVSD" [0x0F,0x5E]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension SSE2]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128_Low64  E_ModRM
                         ]]

   , i "Divide scalar double-precision floating-point values" "VDIVSD" [0x0F,0x5E]
         [MandatoryPrefix 0xF2, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low64  E_ModRM
                        ]]

   , i "Divide scalar single-precision floating-point values" "DIVSS" [0x0F,0x5E]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension SSE]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128_Low32  E_ModRM
                         ]]

   , i "Divide scalar single-precision floating-point values" "VDIVSS" [0x0F,0x5E]
         [MandatoryPrefix 0xF3, Legacy, LongMode, Extension AVX]
         []
         [Vex LWIG      [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128_Low32  E_ModRM
                        ]]
   , i "Dot product of packed double precision floating-point values" "DPPD" [0x0F,0x3A,0x41]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Dot product of packed double precision floating-point values" "DPPD" [0x0F,0x3A,0x41]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128         E_ModReg
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_VM128        E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Dot product of packed single precision floating-point values" "DPPS" [0x0F,0x3A,0x40]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_V128         E_ModReg
                         , op    RO    T_VM128        E_ModRM
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Dot product of packed single precision floating-point values" "DPPS" [0x0F,0x3A,0x40]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_V128_256     E_ModReg
                        , op     RO    T_V128_256     E_VexV
                        , op     RO    T_VM128_256    E_ModRM
                        , op     RO    T_Imm8         E_Imm
                        ]]

   , i "Empty MMX technology state" "EMMS" [0x0F,0x77]
         [Legacy, LongMode]
         []
         [LegacyEncoding []]

   , i "Make stack frame for procedure parameters" "ENTER" [0xC8]
         [Legacy, LongMode]
         []
         [LegacyEncoding [ op    RO    T_Imm16     E_Imm
                         , op    RO    T_Imm8      E_Imm
                         ]]

   , i "Extract packed single precision floating-point value" "EXTRACTPS" [0x0F,0x3A,0x17]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension SSE4_1]
         []
         [LegacyEncoding [ op    RW    T_RM32         E_ModRM
                         , op    RO    T_V128         E_ModReg
                         , op    RO    T_Imm8         E_Imm
                         ]]

   , i "Extract packed single precision floating-point value" "VEXTRACTPS" [0x0F,0x3A,0x17]
         [MandatoryPrefix 0x66, Legacy, LongMode, Extension AVX]
         []
         [Vex WIG       [ op     WO    T_RM32         E_ModRM
                        , op     RO    T_V128         E_VexV
                        , op     RO    T_Imm8         E_Imm
                        ]]


   ]

--------------------------------------
-- X87 instructions
--------------------------------------

x87 :: String -> String -> [Word8] -> [Properties] -> [FlagOp X87Flag] -> [FlagOp Flag] -> [Encoding] -> X87Insn
x87 = X87Insn

x87_f2xm1 :: X87Insn
x87_f2xm1 = x87
   "FPU compute 2^x - 1" "F2XM1" [0xD9,0xF0]
   [Legacy, LongMode, Extension FPU]
   [ Set [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]

x87_fabs :: X87Insn
x87_fabs = x87
   "FPU absolute value" "FABS" [0xD9,0xE1]
   [Legacy, LongMode, Extension FPU]
   [ Zero [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]

x87_fadd :: X87Insn
x87_fadd = x87
   "FPU add" "FADD" [0xD8]
   [Legacy, LongMode, Reversable 2, FPUPop 1, FPUMemFormat, OpExt 0x0, Extension FPU]
   [ Set [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RW    T_ST0    E_Implicit 
                   , op    RO    T_STMem  E_ModRM
                   ]]

x87_fbld :: X87Insn
x87_fbld = x87
   "FPU load binary coded decimal" "FBLD" [0xDF]
   [Legacy, LongMode, Extension FPU, OpExt 0x4]
   [ Set [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RO    T_M80dec E_ModRM ]]

x87_fbstp :: X87Insn
x87_fbstp = x87
   "Store BCD integer and pop" "FBSTP" [0xDF]
   [Legacy, LongMode, Extension FPU, OpExt 0x6]
   [ Set [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RO    T_ST0    E_Implicit
                   , op    WO    T_M80dec E_ModRM
                   ]]

x87_fchs :: X87Insn
x87_fchs = x87
   "FPU change sign" "FCHS" [0xD9,0xE9]
   [Legacy, LongMode, Extension FPU]
   [ Zero [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]

x87_fclex :: X87Insn
x87_fclex = x87
   "FPU clear exceptions" "FCLEX" [0xDB,0xE2]
   [Legacy, LongMode, Extension FPU]
   [ Undef [C0,C1,C2,C3]]
   []
   [LegacyEncoding []]

x87_fcmovb :: X87Insn
x87_fcmovb = x87
   "FPU conditional move if below" "FCMOVB" [0xDA,0xC0]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [Read [CF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmove :: X87Insn
x87_fcmove = x87
   "FPU conditional move if equal" "FCMOVE" [0xDA,0xC8]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [ZF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovbe :: X87Insn
x87_fcmovbe = x87
   "FPU conditional move if below or equal" "FCMOVBE" [0xDA,0xD0]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [CF,ZF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovu :: X87Insn
x87_fcmovu = x87
   "FPU conditional move if unordered" "FCMOVU" [0xDA,0xD8]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [PF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovnb :: X87Insn
x87_fcmovnb = x87
   "FPU conditional move if not below" "FCMOVNB" [0xDB,0xC0]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [CF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovne :: X87Insn
x87_fcmovne = x87
   "FPU conditional move if not equal" "FCMOVNE" [0xDB,0xC8]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [ZF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovnbe :: X87Insn
x87_fcmovnbe = x87
   "FPU conditional move if not below or equal" "FCMOVNBE" [0xDB,0xD0]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [CF,ZF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcmovnu :: X87Insn
x87_fcmovnu = x87
   "FPU conditional move if not unordered" "FCMOVNU" [0xDB,0xD8]
   [Legacy, LongMode, Extension FPU, Arch IntelP6]
   [ Set [C1], Undef [C0,C2,C3]]
   [ Read [PF]]
   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
                   , op    RO    T_ST     E_OpReg
                   ]]

x87_fcom :: X87Insn
x87_fcom = x87
   "FPU compare" "FCOM" [0xD8]
   [Legacy, LongMode, FPUPop 8, FPUMemFormat, OpExt 0x2, Extension FPU]
   [ Set [C1], Undef [C0,C2,C3]]
   []
   [LegacyEncoding [ op    RO    T_ST0    E_Implicit 
                   , op    RO    T_STMem  E_ModRM
                   ]]

newtype M32FP  = M32FP Addr deriving (Show,Eq)
newtype M64FP  = M64FP Addr deriving (Show,Eq)
newtype M16INT = M16INT Addr deriving (Show,Eq)
newtype M32INT = M32INT Addr deriving (Show,Eq)
newtype M64INT = M64INT Addr deriving (Show,Eq)
newtype M80DEC = M80DEC Addr deriving (Show,Eq)
newtype M80BCD = M80BCD Addr deriving (Show,Eq)
newtype M80FP  = M80FP Addr deriving (Show,Eq)
newtype ST     = ST Word8 deriving (Show,Eq)
newtype MCW    = MCW Addr deriving (Show,Eq)
newtype MSW    = MSW Addr deriving (Show,Eq)
newtype MENV   = MENV Addr deriving (Show,Eq)
newtype MSTATE = MSTATE Addr deriving (Show,Eq)

-- | X87 instructions
--
-- Some instruction are polymorphic (e.g. FADD m32fp/m64fp). Here we mangle the
-- data constructor to avoid this (e.g. FADD_m32 and FADD_m64).
data X87Instruction
   = F2XM1
   | FABS
   | FADD_m32 M32FP
   | FADD_m64 M64FP
   | FADD_st0_sti ST
   | FADD_sti_st0 ST
   | FADDP_sti_st0 ST
   | FIADD_m32 M32INT
   | FIADD_m16 M16INT
   | FBLD M80DEC
   | FBSTP M80BCD
   | FCHS
   | FNCLEX
   | FCMOVB ST
   | FCMOVE ST
   | FCMOVBE ST
   | FCMOVU ST
   | FCMOVNB ST
   | FCMOVNE ST
   | FCMOVNBE ST
   | FCMOVNU ST
   | FCOM_m32 M32FP
   | FCOM_m64 M64FP
   | FCOM_st  ST
   | FCOMP_m32 M32FP
   | FCOMP_m64 M64FP
   | FCOMP_st  ST
   | FCOMPP
   | FCOMI ST
   | FCOMIP ST
   | FUCOMI ST
   | FUCOMIP ST
   | FCOS
   | FDECSTP
   | FDIV_m32 M32FP
   | FDIV_m64 M64FP
   | FDIV_st0_st0_sti ST
   | FDIV_sti_st0_sti ST
   | FDIV_st0_sti_st0 ST
   | FDIV_sti_sti_st0 ST
   | FDIVP ST
   | FIDIV_m32 M32INT
   | FIDIV_m16 M16INT
   | FDIVR_m32 M32FP
   | FDIVR_m64 M64FP
   | FDIVRP ST
   | FIDIVR_m32 M32INT
   | FIDIVR_m16 M16INT
   | FFREE ST
   | FICOM_m16 M16INT
   | FICOM_m32 M32INT
   | FICOMP_m16 M16INT
   | FICOMP_m32 M32INT
   | FILD_m16 M16INT
   | FILD_m32 M32INT
   | FILD_m64 M64INT
   | FINCSTP
   | FNINIT
   | FIST_m16 M16INT
   | FIST_m32 M32INT
   | FISTP_m16 M16INT
   | FISTP_m32 M32INT
   | FISTP_m64 M64INT
   | FISTTP_m16 M16INT
   | FISTTP_m32 M32INT
   | FISTTP_m64 M64INT
   | FLD_m32 M32FP
   | FLD_m64 M64FP
   | FLD_m80 M80FP
   | FLD_st  ST
   | FLD1
   | FLDL2T
   | FLDL2E
   | FLDPI
   | FLDLG2
   | FLDLN2
   | FLDZ
   | FLDCW MCW
   | FLDENV MENV
   | FMUL_m32 M32FP
   | FMUL_m64 M64FP
   | FMUL_st0_sti ST
   | FMUL_sti_st0 ST
   | FMULP_sti_st0 ST
   | FIMUL_m32 M32INT
   | FIMUL_m16 M16INT
   | FNOP
   | FPATAN
   | FPREM
   | FPREM1
   | FPTAN
   | FRNDINT
   | FRSTOR MSTATE
   | FNSAVE MSTATE
   | FSCALE
   | FSIN
   | FSINCOS
   | FSQRT
   | FST_m32 M32FP
   | FST_m64 M64FP
   | FST_st  ST
   | FSTP_m32 M32FP
   | FSTP_m64 M64FP
   | FSTP_m80 M80FP
   | FSTP_st  ST
   | FNSTCW MCW
   | FNSTENV MENV
   | FNSTSW MSW
   | FNSTSW_ax
   | FSUB_st0_st0_m32 M32FP
   | FSUB_st0_st0_m64 M64FP
   | FSUB_st0_m32_st0 M32FP
   | FSUB_st0_m64_st0 M64FP
   | FSUB_st0_st0_sti ST
   | FSUB_st0_sti_st0 ST
   | FSUB_sti_st0_sti ST
   | FSUB_sti_sti_st0 ST
   | FSUBP_st0_st0_sti ST
   | FSUBP_sti_sti_st0 ST
   | FISUB_m32 M32INT
   | FISUB_m16 M16INT
   | FISUBR_m32 M32INT
   | FISUBR_m16 M16INT
   | FTST
   | FUCOM ST
   | FUCOMP ST
   | FUCOMPP
   | FXAM
   | FXCH ST
   | FXTRACT
   | FYL2X
   | FYL2XP1
   deriving (Show,Eq)

-- getX87Info :: X87Instruction -> X87Insn
-- getX87Info x = case x of
--    F2XM1                   -> x87_f2xm1
--    FABS                    -> x87_fabs
--    FADD_m32 _              -> x87_fadd
--    FADD_m64 _              -> x87_fadd
--    FADD_st0_sti _          -> x87_fadd
--    FADD_sti_st0 _          -> x87_fadd
--    FADDP_sti_sto _         -> x87_faddp
--    FADDP                   -> x87_faddp
--    FIADD_m32 _             -> x87_fiadd
--    FIADD_m16 _             -> x87_fiadd
--    FBLD _                  -> x87_fbld
--    FBSTP _                 -> x87_fbstp
--    FCHS                    -> x87_fchs
--    FNCLEX                  -> x87_fnclex
--    FCMOVB _                -> x87_fcmov
--    FCMOVE _                -> x87_fcmov
--    FCMOVBE _               -> x87_fcmov
--    FCMOVU _                -> x87_fcmov
--    FCMOVNB _               -> x87_fcmov
--    FCMOVNE _               -> x87_fcmov
--    FCMOVNBE _              -> x87_fcmov
--    FCMOVNU _               -> x87_fcmov
--    FCOM_m32 _              -> x87_fcom
--    FCOM_m64 _              -> x87_fcom
--    FCOM_st _               -> x87_fcom
--    FCOMP_m32 _             -> x87_fcomp
--    FCOMP_m64 _             -> x87_fcomp
--    FCOMP_st _              -> x87_fcomp
--    FCOMPP                  -> x87_fcompp
--    FCOMI _                 -> x87_fcomi
--    FCOMIP _                -> x87_fcomip
--    FUCOMI _                -> x87_fucomi
--    FUCOMIP _               -> x87_fucomip
--    FCOS                    -> x87_fcos
--    FDECSTP                 -> x87_fdecstp
--    FDIV_m32 _              -> x87_fdiv
--    FDIV_m64 _              -> x87_fdiv
--    FDIV_st0_sti _          -> x87_fdiv
--    FDIV_sti_st0 _          -> x87_fdiv
--    FDIVP _                 -> x87_fdivp
--    FIDIV_m32 _             -> x87_fidiv
--    FIDIV_m16 _             -> x87_fidiv
--    FDIVR_m32 _             -> x87_fdivr
--    FDIVR_m64 _             -> x87_fdivr
--    FDIVR_st0_sti _         -> x87_fdivr
--    FDIVR_sti_st0 _         -> x87_fdivr
--    FDIVRP _                -> x87_fdivrp
--    FIDIVR_m32 _            -> x87_fidivr
--    FIDIVR_m16 _            -> x87_fidivr
--    FFREE _                 -> x87_ffree
--    FICOM_m16 _             -> x87_ficom
--    FICOM_m32 _             -> x87_ficom
--    FICOMP_m16 _            -> x87_ficomp
--    FICOMP_m32 _            -> x87_ficomp
--    FILD_m16 _              -> x87_fild
--    FILD_m32 _              -> x87_fild
--    FILD_m64 _              -> x87_fild
--    FINCSTP                 -> x87_fincstp
--    FNINIT                  -> x87_fninit
--    FIST_m16 _              -> x87_fist
--    FIST_m32 _              -> x87_fist
--    FISTP_m16 _             -> x87_fistp
--    FISTP_m32 _             -> x87_fistp
--    FISTP_m64 _             -> x87_fistp
--    FISTTP_m16 _            -> x87_fistpp
--    FISTTP_m32 _            -> x87_fistpp
--    FISTTP_m64 _            -> x87_fistpp
--    FLD_m32 _               -> x87_fld
--    FLD_m64 _               -> x87_fld
--    FLD_m80 _               -> x87_fld
--    FLD_st _                -> x87_fld
--    FLD1                    -> x87_fld1
--    FLDL2T                  -> x87_fldl2t
--    FLDL2E                  -> x87_fldl2e
--    FLDPI                   -> x87_fldpi
--    FLDLG2                  -> x87_fldlg2
--    FLDLN2                  -> x87_fldln2
--    FLDZ                    -> x87_fldz
--    FLDCW _                 -> x87_fldcw
--    FLDENV _                -> x87_fldenv
--    FMUL_m32 _              -> x87_fmul
--    FMUL_m64 _              -> x87_fmul
--    FMUL_st0_sti _          -> x87_fmul
--    FMUL_sti_st0 _          -> x87_fmul
--    FMULP_sti_st0 _         -> x87_fmulp
--    FIMUL_m32 _             -> x87_fimul
--    FIMUL_m16 _             -> x87_fimul
--    FNOP                    -> x87_fnop
--    FPATAN                  -> x87_fpatan
--    FPREM                   -> x87_fprem
--    FPREM1                  -> x87_fprem1
--    FPTAN                   -> x87_fptan
--    FRNDINT                 -> x87_frndint
--    FRSTOR _                -> x87_frstor
--    FNSAVE _                -> x87_fnsave
--    FSCALE                  -> x87_fscale
--    FSIN                    -> x87_fsin
--    FSINCOS                 -> x87_fsincos
--    FSQRT                   -> x87_fsqrt
--    FST_m32 _               -> x87_fst
--    FST_m64 _               -> x87_fst
--    FST_st _                -> x87_fst
--    FSTP_m32 _              -> x87_fstp
--    FSTP_m64 _              -> x87_fstp
--    FSTP_m80 _              -> x87_fstp
--    FSTP_st _               -> x87_fstp
--    FNSTCW _                -> x87_fnstcw
--    FNSTENV _               -> x87_fnstenv
--    FNSTSW _                -> x87_fnstsw
--    FNSTSW_ax               -> x87_fnstsw
--    FSUB_m32 _              -> x87_fsub
--    FSUB_m64 _              -> x87_fsub
--    FSUB_st0_sti _          -> x87_fsub
--    FSUB_sti_st0 _          -> x87_fsub
--    FSUBP_st0_sti _         -> x87_fsubp
--    FISUB_m32 _             -> x87_fisub
--    FISUB_m64 _             -> x87_fisub
--    FSUBR_m32 _             -> x87_fsubr
--    FSUBR_m64 _             -> x87_fsubr
--    FSUBRP_sti_st0 _        -> x87_fsubrp
--    FISUBR_m32 _            -> x87_fisubr
--    FISUBR_m64 _            -> x87_fisubr
--    FTST                    -> x87_ftst
--    FUCOM _                 -> x87_fucom
--    FUCOMP _                -> x87_fucomp
--    FUCOMPP                 -> x87_fucompp
--    FXAM                    -> x87_fxam
--    FXCH _                  -> x87_fxch
--    FXTRACT                 -> x87_fxtract
--    FYL2X                   -> x87_fyl2x
--    FYL2XP1                 -> x87_fyl2xp1
