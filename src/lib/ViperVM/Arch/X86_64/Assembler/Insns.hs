{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Arch.X86_64.Assembler.Insns
   ( X86Insn(..)
   , X86Arch(..)
   , X86Extension(..)
   , Properties(..)
   , Flag(..)
   , FlagOp(..)
   , instructions
   , getLegacyOpcodes
   , FlaggedOpcode(..)
   , buildLegacyOpcodeMap
   , buildVexOpcodeMap
   , maybeOpTypeReg
   , amd3DNowEncoding
   -- * Opcode maps
   , opcodeMapPrimary
   , opcodeMap0F
   , opcodeMap0F38
   , opcodeMap0F3A
   , opcodeMap3DNow
   , opcodeMapVex1
   , opcodeMapVex2
   , opcodeMapVex3
   )
where

import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Data.List ((\\))
import qualified Data.Vector as V

import ViperVM.Arch.X86_64.MicroArch
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Encoding

data X86Insn = X86Insn
   { iDesc        :: String
   , iMnemonic    :: String
   , iProperties  :: [Properties]
   , iFlags       :: [FlagOp Flag]
   , iEncoding    :: [Encoding]
   } deriving (Show)

data FlagOp a
   = St        [a]  -- ^ Set flag to 1
   | Unset     [a]  -- ^ Set flag to 0
   | Modified  [a]  -- ^ Set flag depending on the result
   | Undefined [a]  -- ^ Flag is undefined after the operation
   | Read      [a]  -- ^ Flag read by the instruction
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

-- | Indicate if the operand type can be register when stored in ModRM.rm
-- (i.e. ModRM.mod may be 11b)
maybeOpTypeReg :: OperandType -> Bool
maybeOpTypeReg = \case
   T_Imm8       -> False
   T_Imm16      -> False
   T_Imm        -> False
   T_REL_16_32  -> False
   T_PTR_16_16  -> False
   T_PTR_16_32  -> False
   T_Mask       -> False

   T_R          -> True
   T_R16        -> True
   T_R32        -> True
   T_RM         -> True
   T_RM16       -> True
   T_RM32       -> True
   T_RM16_32    -> True
   T_RM32_64    -> True
   T_RM16_32_64 -> True
   T_RM64       -> True
   T_R16_32     -> True
   T_R32_64     -> True
   T_R16_32_64  -> True

   T_M_PAIR     -> False
   T_M16_XX     -> False
   T_M64_128    -> False
   T_M          -> False
   T_MFP        -> False
   T_M80dec     -> False

   T_Vec           -> True
   T_V64           -> True
   T_VM64          -> True
   T_V128          -> True
   T_VM128         -> True
   T_V128_Low32    -> True
   T_VM128_Low32   -> True
   T_V128_Low64    -> True
   T_VM128_Low64   -> True
   T_V128_256      -> True
   T_VM128_256     -> True

   T_Accu       -> False
   T_AX_EAX_RAX -> False
   T_xDX_xAX    -> False
   T_xCX_xBX    -> False
   T_xAX        -> False
   T_xBX        -> False
   T_xCX        -> False
   T_xDX        -> False
   T_AL         -> False
   T_AX         -> False
   T_XMM0       -> False
   T_rSI        -> False
   T_rDI        -> False

   T_ST0        -> False
   T_ST         -> True
   T_STMem      -> True

opf :: LegacyOpcodeFields
opf = LegacyOpcodeFields Nothing Nothing Nothing

legacyEncoding :: Maybe Word8 -> LegacyMap -> Word8 -> Maybe Word8 -> LegacyOpcodeFields -> [EncodingProperties] -> [OperandSpec] -> Encoding
legacyEncoding a b c d e f g = LegacyEncoding $ LegEnc a b c d e f g

vexoding :: Maybe Word8 -> OpcodeMap -> Word8 -> Maybe Word8 -> VexLW -> [EncodingProperties] -> [OperandSpec] -> Encoding
vexoding a b c d e f g = VexEncoding $ VexEnc a b c d e f g


i :: String -> String -> [Properties] -> [FlagOp Flag] -> [Encoding] -> X86Insn
i = X86Insn

op :: AccessMode -> OperandType -> OperandEnc -> OperandSpec
op = OperandSpec

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
   []
   [Modified [AF,CF], Undefined [OF,SF,ZF,PF]]
   [legacyEncoding Nothing MapPrimary 0x37 Nothing 
      opf
      [LegacyModeSupport]
      [ op    RW    T_AX     Implicit
      ]]

i_aad :: X86Insn
i_aad = i "ASCII adjust AX before division" "AAD"
   []
   [Modified [SF,ZF,PF], Undefined [OF,AF,CF]]
   [legacyEncoding Nothing MapPrimary 0xD5 Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_AX     Implicit
      , op    RO    T_Imm8   Imm
      ]]

i_aam :: X86Insn
i_aam = i "ASCII adjust AX after multiply" "AAM"
   [FailOnZero 0]
   [Modified [SF,ZF,PF], Undefined [OF,AF,CF]]
   [legacyEncoding Nothing MapPrimary 0xD4 Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_AX     Implicit
      , op    RO    T_Imm8   Imm
      ]]

i_aas :: X86Insn
i_aas = i "ASCII adjust AL after subtraction" "AAS"
   []
   [Modified [AF,CF], Undefined [OF,SF,ZF,PF]]
   [legacyEncoding Nothing MapPrimary 0x3F Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_AX     Implicit
      ]]

i_adc :: X86Insn
i_adc = i "Add with carry" "ADC"
   []
   [Read [CF], Modified [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x14 Nothing
      (opf { sizable = Just 0})
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_Accu   Implicit
      , op    RO    T_Imm    Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x10 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_R      Reg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 2)
      (opf {sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_Imm    Imm
      ]]

i_adcx :: X86Insn
i_adcx = i "Unsigned integer addition with carry flags" "ADCX"
   []
   [Read [CF], Modified [CF]]
   [legacyEncoding (Just 0x66) Map0F38 0xF6 Nothing
      opf
      [Extension ADX]
      [ op    RW    T_R32_64    Reg
      , op    RO    T_RM32_64   RM
      ]]

i_add :: X86Insn
i_add = i "Add" "ADD"
   []
   [Modified [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x04 Nothing
      (opf { sizable = Just 0})
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_Accu   Implicit
      , op    RO    T_Imm    Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x00 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_R      Reg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 0)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_Imm    Imm
      ]]

i_addpd :: X86Insn
i_addpd = i "Add packed double-precision floating-point values" "ADDPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x58 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vaddpd :: X86Insn
i_vaddpd = i "Add packed double-precision floating-point values" "VADDPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x58 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_addps :: X86Insn
i_addps = i "Add packed float-precision floating-point values" "ADDPS"
   []
   []
   [legacyEncoding Nothing Map0F 0x58 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vaddps :: X86Insn
i_vaddps = i "Add packed float-precision floating-point values" "VADDPS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x58 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_addsd :: X86Insn
i_addsd = i "Add scalar double-precision floating-point values" "ADDSD"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x58 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_vaddsd :: X86Insn
i_vaddsd = i "Add scalar double-precision floating-point values" "VADDSD"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x58 Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low64  RM
      ]]

i_addss :: X86Insn
i_addss = i "Add scalar single-precision floating-point values" "ADDSS"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x58 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128_Low32  RM
      ]]

i_vaddss :: X86Insn
i_vaddss = i "Add scalar single-precision floating-point values" "VADDSS"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x58 Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low32  RM
      ]]

i_addsubpd :: X86Insn
i_addsubpd = i "Packed double-FP add/subtract" "ADDSUBPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0xD0 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE3]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vaddsubpd :: X86Insn
i_vaddsubpd = i "Packed double-FP add/subtract" "VADDSUBPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0xD0 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_addsubps :: X86Insn
i_addsubps = i "Packed single-FP add/subtract" "ADDSUBPS"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0xD0 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE3]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vaddsubps :: X86Insn
i_vaddsubps = i "Packed single-FP add/subtract" "VADDSUBPS"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0xD0 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_adox :: X86Insn
i_adox = i "Unsigned integer addition of two operands with overflow flag" "ADOX"
   []
   [Read [OF], Modified [OF]]
   [legacyEncoding (Just 0xF3) Map0F38 0xF6 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension ADX]
      [ op    RW    T_R32_64       Reg
      , op    RO    T_RM32_64      RM
      ]]

i_aesdec :: X86Insn
i_aesdec = i "Perform one round of an AES decryption flow" "AESDEC"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDE Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vaesdec :: X86Insn
i_vaesdec = i "Perform one round of an AES decryption flow" "VAESDEC"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x02) 0xDE Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128        RM
      ]]

i_aesdeclast :: X86Insn
i_aesdeclast = i "Perform last round of an AES decryption flow" "AESDECLAST"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDF Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vaesdeclast :: X86Insn
i_vaesdeclast = i "Perform last round of an AES decryption flow" "VAESDECLAST"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x02) 0xDF Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128        RM
      ]]

i_aesenc :: X86Insn
i_aesenc = i "Perform one round of an AES encryption flow" "AESENC"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDC Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vaesenc :: X86Insn
i_vaesenc = i "Perform one round of an AES encryption flow" "VAESENC"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x02) 0xDC Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128        RM
      ]]

i_aesenclast :: X86Insn
i_aesenclast = i "Perform last round of an AES encryption flow" "AESENCLAST"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDD Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vaesenclast :: X86Insn
i_vaesenclast = i "Perform last round of an AES encryption flow" "VAESENCLAST"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x02) 0xDD Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128        RM
      ]]

i_aesimc :: X86Insn
i_aesimc = i "Perform the AES InvMixColumn transformation" "AESIMC"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0xDB Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vaesimc :: X86Insn
i_vaesimc = i "Perform the AES InvMixColumn transformation" "VAESIMC"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x02) 0xDB Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_VM128        RM
      ]]

i_aeskeygenassist :: X86Insn
i_aeskeygenassist = i "AES round key generation assist" "AESKEYGENASSIST"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0xDF Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension AES]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      , op    RO    T_Imm8         Imm
      ]]

i_vaeskeygenassist :: X86Insn
i_vaeskeygenassist = i "AES round key generation assist" "VAESKEYGENASSIST"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0xDF Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AES, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_VM128        RM
      , op     RO    T_Imm8         Imm
      ]]

i_and :: X86Insn
i_and = i "Logical AND" "AND"
   []
   [Unset [OF,CF], Modified [SF,ZF,PF], Undefined [AF]]
   [legacyEncoding Nothing MapPrimary 0x24 Nothing
      (opf { sizable = Just 0})
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_Accu   Implicit
      , op    RO    T_Imm    Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x20 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_R      Reg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 4)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_Imm    Imm
      ]
   ]

i_andn :: X86Insn
i_andn = i "Logical AND NOT" "ANDN"
   []
   [Modified [SF,ZF], Unset [OF,CF], Undefined [AF,PF]]
   [vexoding Nothing (MapVex 0x02) 0xF2 Nothing L0
      [LegacyModeSupport, LongModeSupport, Extension BMI1]
      [ op    WO    T_R32_64     Reg
      , op    RO    T_R32_64     Vvvv
      , op    RO    T_RM32_64    RM
      ]]

i_andpd :: X86Insn
i_andpd = i "Bitwise logical AND of packed double-precision floating-point values" "ANDPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x54 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vandpd :: X86Insn
i_vandpd = i "Bitwise logical AND of packed double-precision floating-point values" "VANDPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x54 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_andps :: X86Insn
i_andps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDPS"
   []
   []
   [legacyEncoding Nothing Map0F 0x54 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vandps :: X86Insn
i_vandps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDPS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x54 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_andnpd :: X86Insn
i_andnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "ANDNPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x55 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vandnpd :: X86Insn
i_vandnpd = i "Bitwise logical AND NOT of packed double-precision floating-point values" "VANDNPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x55 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_andnps :: X86Insn
i_andnps = i "Bitwise logical AND of packed float-precision floating-point values" "ANDNPS"
   []
   []
   [legacyEncoding Nothing Map0F 0x55 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vandnps :: X86Insn
i_vandnps = i "Bitwise logical AND of packed float-precision floating-point values" "VANDNPS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x55 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_arpl :: X86Insn
i_arpl = i "Adjust RPL field of segment selector" "ARPL"
   []
   [Modified [ZF]]
   [legacyEncoding Nothing MapPrimary 0x63 Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_RM16   RM
      , op    RO    T_R16    Reg
      ]]

i_blendpd :: X86Insn
i_blendpd = i "Blend packed double-precision floating-point values" "BLENDPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x0D Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vblendpd :: X86Insn
i_vblendpd = i "Blend packed double-precision floating-point values" "VBLENDPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x0D Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_Mask         Imm8l
      ]]

i_bextr :: X86Insn
i_bextr = i "Bit field extract" "BEXTR"
   []
   [Modified [ZF], Undefined [AF,SF,PF], Unset (allFlags \\ [ZF,AF,SF,PF])]
   [vexoding Nothing (MapVex 0x02) 0xF7 Nothing L0
      [LegacyModeSupport, LongModeSupport, Extension BMI1]
      [ op    WO    T_R32_64     Reg
      , op    RO    T_RM32_64    RM
      , op    RO    T_R32_64     Vvvv
      ]]

i_blendps :: X86Insn
i_blendps = i "Blend packed single-precision floating-point values" "BLENDPS"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x0C Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vblendps :: X86Insn
i_vblendps = i "Blend packed single-precision floating-point values" "VBLENDPS"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x0C Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_Imm8         Imm
      ]]

i_blendvpd :: X86Insn
i_blendvpd = i "Variable blend packed double-precision floating-point values" "BLENDVPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0x15 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_XMM0   Implicit
      ]]

i_vblendvpd :: X86Insn
i_vblendvpd = i "Variable blend packed double-precision floating-point values" "VBLENDVPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x4B Nothing W0
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_V128_256     Imm8h
      ]]

i_blendvps :: X86Insn
i_blendvps = i "Variable blend packed single-precision floating-point values" "BLENDVPS"
   []
   []
   [legacyEncoding (Just 0x66) Map0F38 0x14 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_XMM0   Implicit
      ]]

i_vblendvps :: X86Insn
i_vblendvps = i "Variable blend packed single-precision floating-point values" "VBLENDVPS"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x4A Nothing W0
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_V128_256     Imm8h
      ]]

i_blsi :: X86Insn
i_blsi = i "Extract lowest set isolated bit" "BLSI"
   []
   [Modified [ZF,SF, CF], Unset [OF], Undefined [AF,PF]]
   [vexoding Nothing (MapVex 0x02) 0xF3 (Just 3) L0
      [LegacyModeSupport, LongModeSupport, Extension BMI1]
      [ op    WO    T_R32_64     Vvvv
      , op    RO    T_RM32_64    RM
      ]]

i_blsmsk :: X86Insn
i_blsmsk = i "Get mask up to lowest set bit" "BLSMSK"
   []
   [Modified [SF,CF], Unset [ZF,OF], Undefined [AF,PF]]
   [vexoding Nothing (MapVex 0x02) 0xF3 (Just 2) L0
      [LegacyModeSupport, LongModeSupport, Extension BMI1]
      [ op    WO    T_R32_64     Vvvv
      , op    RO    T_RM32_64    RM
      ]]

i_blsr :: X86Insn
i_blsr = i "Reset lowest set bit" "BLSR"
   []
   [Modified [ZF,SF,CF], Unset [OF], Undefined [AF,PF]]
   [vexoding Nothing (MapVex 0x02) 0xF3 (Just 1) L0
      [LegacyModeSupport, LongModeSupport, Extension BMI1]
      [ op    WO    T_R32_64     Vvvv
      , op    RO    T_RM32_64    RM
      ]]

i_bound :: X86Insn
i_bound = i "Check array index against bounds" "BOUND"
   []
   []
   [legacyEncoding Nothing MapPrimary 0x62 Nothing
      opf
      [LegacyModeSupport]
      [ op    RO    T_R16_32 Reg
      , op    RO    T_M_PAIR RM
      ]]

i_bsf :: X86Insn
i_bsf = i "Bit scan forward" "BSF"
   []
   [Modified [ZF], Undefined [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBC Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_R      Reg
      , op    RO    T_RM     RM
      ]]

i_bsr :: X86Insn
i_bsr = i "Bit scan reverse" "BSR"
   []
   [Modified [ZF], Undefined [CF,OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBD Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_R      Reg
      , op    RO    T_RM     RM
      ]]

i_bswap :: X86Insn
i_bswap = i "Byte swap" "BSWAP"
   []
   []
   [legacyEncoding Nothing Map0F 0xC8 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Arch Intel486]
      [ op    RW    T_R32_64 OpcodeLow3
      ]]

i_bt :: X86Insn
i_bt = i "Bit test" "BT"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xA3 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RO    T_RM16_32_64   RM
      , op    RO    T_R16_32_64    Reg
      ]]

i_bt_imm :: X86Insn
i_bt_imm = i "Bit test with immediate index" "BT"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 4)
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RO    T_RM16_32_64   RM
      , op    RO    T_Imm8         Imm
      ]]

i_btc :: X86Insn
i_btc = i "Bit test and complement" "BTC"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBB Nothing
      opf
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_R16_32_64    Reg
      ]]

i_btc_imm :: X86Insn
i_btc_imm = i "Bit test with immediate index and complement" "BTC"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 7)
      opf
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_Imm8         Imm
      ]]

i_btr :: X86Insn
i_btr = i "Bit test and reset" "BTR"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xB3 Nothing
      opf
      [Lockable,  LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_R16_32_64    Reg
      ]]

i_btr_imm :: X86Insn
i_btr_imm = i "Bit test with immediate index and reset" "BTR"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 6)
      opf
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_Imm8         Imm
      ]]

i_bts :: X86Insn
i_bts = i "Bit test and set" "BTS"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xAB Nothing
      opf
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_R16_32_64    Reg
      ]]

i_bts_imm :: X86Insn
i_bts_imm = i "Bit test with immediate index and set" "BTS"
   []
   [Modified [CF], Undefined [OF,SF,AF,PF]]
   [legacyEncoding Nothing Map0F 0xBA (Just 5)
      opf
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM16_32_64   RM
      , op    RO    T_Imm8         Imm
      ]]

i_bzhi :: X86Insn
i_bzhi = i "Zero high bits starting with specified bit position" "BZHI"
   []
   [Modified [ZF,CF,SF], Unset [OF], Undefined [AF,PF]]
   [vexoding Nothing (MapVex 0x02) 0xF5 Nothing L0
      [LegacyModeSupport, LongModeSupport, Extension BMI2]
      [ op    WO    T_R32_64     Reg
      , op    RO    T_RM32_64    RM
      , op    RO    T_R32_64     Vvvv
      ]]

i_rel_near_call :: X86Insn
i_rel_near_call = i "Relative near call" "CALL"
   []
   [Undefined allFlags]
   [legacyEncoding Nothing MapPrimary 0xE8 Nothing
      opf
      [DefaultOperandSize64, LegacyModeSupport, LongModeSupport]
      [ op    RO    T_REL_16_32    Imm ]]

i_ind_near_call :: X86Insn
i_ind_near_call = i "Indirect near call" "CALL"
   []
   [Undefined allFlags]
   [legacyEncoding Nothing MapPrimary 0xFF (Just 2)
      opf
      [LegacyModeSupport]
      [ op    RO    T_RM16_32      RM 
      ]
   ,legacyEncoding Nothing MapPrimary 0xFF (Just 2)
      opf
      [LongModeSupport, DefaultOperandSize64]
      [ op    RO    T_RM64         RM
      ]
   ]

i_abs_far_call :: X86Insn
i_abs_far_call = i "Absolute far call" "CALL"
   []
   [Undefined allFlags]
   [legacyEncoding Nothing MapPrimary 0x9A Nothing
      opf
      [LegacyModeSupport]
      [ op    RO    T_PTR_16_16    Imm ]
   ,legacyEncoding Nothing MapPrimary 0x9A Nothing
      opf
      [LegacyModeSupport]
      [ op    RO    T_PTR_16_32    Imm ]
   ]

i_abs_ind_far_call :: X86Insn
i_abs_ind_far_call = i "Absolute indirect far call" "CALL"
   []
   [Undefined allFlags]
   [legacyEncoding Nothing MapPrimary 0xFF (Just 3)
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RO    T_M16_XX       RM     ]]

i_extend_signed :: X86Insn
i_extend_signed = i "Extend signed word" "CBW/CWDE/CDQE"
   []
   []
   [legacyEncoding Nothing MapPrimary 0x98 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_Accu         Implicit  ]]

-- FIXME: 0F01 isn't a map, but an extension in the whole ModRM byte
-- i_clac :: X86Insn
-- i_clac = i "Clear AC flag in EFLAGS register" "CLAC"
--    []
--    [Unset [AC]]
--    [legacyEncoding Nothing Map0F01 0xCA Nothing opf
--       [LegacyModeSupport, LongModeSupport, Extension SMAP]
--       []]

i_clc :: X86Insn
i_clc = i "Clear carry flag" "CLC"
   []
   [Unset [CF]]
   [legacyEncoding Nothing MapPrimary 0xF8 Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_cld :: X86Insn
i_cld = i "Clear direction flag" "CLD"
   []
   [Unset [DF]]
   [legacyEncoding Nothing MapPrimary 0xFC Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_clflush :: X86Insn
i_clflush = i "Flush cache line" "CLFLUSH"
   []
   []
   [legacyEncoding Nothing Map0F 0xAE (Just 7)
      opf
      [LegacyModeSupport, LongModeSupport, Extension CLFLUSH]
      [ op    RO    T_M      RM  ]]

i_cli :: X86Insn
i_cli = i "Clear interrupt flag" "CLI"
   []
   [Unset [IF]]
   [legacyEncoding Nothing MapPrimary 0xFA Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_clts :: X86Insn
i_clts = i "Clear task-switched flag in CR0" "CLTS"
   []
   []
   [legacyEncoding Nothing Map0F 0x06 Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_cmc :: X86Insn
i_cmc = i "Complement carry flag" "CMC"
   []
   [Modified [CF]]
   [legacyEncoding Nothing MapPrimary 0xF5 Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_cmovo :: X86Insn
i_cmovo = i "Move if overflow (OF=1)" "CMOVO"
   []
   [Read [OF]]
   [legacyEncoding Nothing Map0F 0x40 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovno :: X86Insn
i_cmovno = i "Move if not overflow (OF=0)" "CMOVNO"
   []
   [Read [OF]]
   [legacyEncoding Nothing Map0F 0x41 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovc :: X86Insn
i_cmovc = i "Move if carry (CF=1)" "CMOVC"
   []
   [Read [CF]]
   [legacyEncoding Nothing Map0F 0x42 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovnc :: X86Insn
i_cmovnc = i "Move if not carry (CF=0)" "CMOVNC"
   []
   [Read [CF]]
   [legacyEncoding Nothing Map0F 0x43 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovz :: X86Insn
i_cmovz = i "Move if zero (ZF=1)" "CMOVZ"
   []
   [Read [ZF]]
   [legacyEncoding Nothing Map0F 0x44 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovnz :: X86Insn
i_cmovnz = i "Move if not zero (ZF=0)" "CMOVNZ"
   []
   [Read [ZF]]
   [legacyEncoding Nothing Map0F 0x45 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovbe :: X86Insn
i_cmovbe = i "Move if below or equal (CF=1, ZF=1)" "CMOVBE"
   []
   [Read [ZF,CF]]
   [legacyEncoding Nothing Map0F 0x46 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmova :: X86Insn
i_cmova = i "Move if above (CF=0, ZF=0)" "CMOVA"
   []
   [Read [ZF,CF]]
   [legacyEncoding Nothing Map0F 0x47 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovs :: X86Insn
i_cmovs = i "Move if sign (SF=1)" "CMOVS"
   []
   [Read [SF]]
   [legacyEncoding Nothing Map0F 0x48 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovns :: X86Insn
i_cmovns = i "Move if not sign (SF=0)" "CMOVNS"
   []
   [Read [SF]]
   [legacyEncoding Nothing Map0F 0x49 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovp :: X86Insn
i_cmovp = i "Move if parity even (PF=1)" "CMOVP"
   []
   [Read [PF]]
   [legacyEncoding Nothing Map0F 0x4a Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovnp :: X86Insn
i_cmovnp = i "Move if parity odd (PF=0)" "CMOVNP"
   []
   [Read [PF]]
   [legacyEncoding Nothing Map0F 0x4b Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovl :: X86Insn
i_cmovl = i "Move if less (SF /= OF)" "CMOVL"
   []
   [Read [SF,OF]]
   [legacyEncoding Nothing Map0F 0x4c Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovge :: X86Insn
i_cmovge = i "Move if greater or equal (SF = OF)" "CMOVGE"
   []
   [Read [SF,OF]]
   [legacyEncoding Nothing Map0F 0x4d Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovle :: X86Insn
i_cmovle = i "Move if less or equal (ZF = 1 or SF <> OF)" "CMOVLE"
   []
   [Read [ZF,SF,OF]]
   [legacyEncoding Nothing Map0F 0x4e Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]

i_cmovg :: X86Insn
i_cmovg = i "Move if greater (ZF = 0 or SF = OF)" "CMOVG"
   []
   [Read [ZF,SF,OF]]
   [legacyEncoding Nothing Map0F 0x4f Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R16_32_64   Reg
      , op    RO    T_RM16_32_64  RM
      ]]


i_cmp :: X86Insn
i_cmp = i "Compare" "CMP"
   []
   [Modified [OF,SF,ZF,AF,CF,PF]]
   [legacyEncoding Nothing MapPrimary 0x3C Nothing
      (opf { sizable = Just 0})
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_Accu   Implicit
      , op    RO    T_Imm    Imm
      ]
   ,legacyEncoding Nothing MapPrimary 0x38 Nothing
      (opf { sizable = Just 0, reversable = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_R      Reg
      ]
   ,legacyEncoding Nothing MapPrimary 0x80 (Just 7)
      (opf { sizable = Just 0, signExtendableImm8 = Just 1})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      , op    RO    T_Imm    Imm
      ]
   ]

i_cmppd :: X86Insn
i_cmppd = i "Compare packed double-precision floating-point values" "CMPPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0xC2 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vcmppd :: X86Insn
i_vcmppd = i "Compare packed double-precision floating-point values" "VCMPPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0xC2 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_Imm8         Imm
      ]]

i_cmpps :: X86Insn
i_cmpps = i "Compare packed single-precision floating-point values" "CMPPS"
   []
   []
   [legacyEncoding Nothing Map0F 0xC2 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vcmpps :: X86Insn
i_vcmpps = i "Compare packed single-precision floating-point values" "VCMPPS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0xC2 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_Imm8         Imm
      ]]

i_cmps :: X86Insn
i_cmps = i "Compare string operands" "CMPS"
   []
   [Modified [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xA6 Nothing
      (opf { sizable = Just 0})
      [LegacyModeSupport, LongModeSupport]
      [ op    RO    T_rSI    Implicit
      , op    RO    T_rDI    Implicit
      ]]

i_cmpsd :: X86Insn
i_cmpsd = i "Compare scalar double-precision floating-point values" "CMPSD"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0xC2 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vcmpsd :: X86Insn
i_vcmpsd = i "Compare scalar double-precision floating-point values" "VCMPSD"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0xC2 Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128      Reg
      , op     RO    T_V128      Vvvv
      , op     RO    T_VM128     RM
      , op     RO    T_Imm8      Imm
      ]]

i_cmpss :: X86Insn
i_cmpss = i "Compare scalar single-precision floating-point values" "CMPSS"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0xC2 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      , op    RO    T_Imm8   Imm
      ]]

i_vcmpss :: X86Insn
i_vcmpss = i "Compare scalar single-precision floating-point values" "VCMPSS"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0xC2 Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128      Reg
      , op     RO    T_V128      Vvvv
      , op     RO    T_VM128     RM
      , op     RO    T_Imm8      Imm
      ]]

i_cmpxchg :: X86Insn
i_cmpxchg = i "Compare and exchange" "CMPXCHG"
   []
   [Modified [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing Map0F 0xB0 Nothing
      (opf {sizable = Just 0})
      [Lockable, LegacyModeSupport, LongModeSupport, Arch Intel486]
      [ op    RW    T_RM     RM
      , op    RO    T_Accu   Implicit
      , op    RO    T_R      Reg
      ]]

i_cmpxch8b :: X86Insn
i_cmpxch8b = i "Compare and exchange bytes" "CMPXCHG8B/CMPXCHG16B"
   []
   [Modified [ZF,CF,PF,AF,SF,OF]]
   [legacyEncoding Nothing Map0F 0xC7 Nothing
      opf
      [DoubleSizable, Lockable, LegacyModeSupport, LongModeSupport, Arch IntelPentium, Extension CX8]
      [ op    RW    T_M64_128   RM ]]


i_comisd :: X86Insn
i_comisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "COMISD"
   []
   [Modified [ZF,PF,CF], Unset [OF,SF,AF]]
   [legacyEncoding (Just 0x66) Map0F 0x2F Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RO    T_V128_Low64      Reg
      , op    RO    T_VM128_Low64     RM
      ]]

i_vcomisd :: X86Insn
i_vcomisd = i "Compare scalar ordered double-precision floating-point values and set EFLAGS" "VCOMISD"
   []
   [Modified [ZF,PF,CF], Unset [OF,SF,AF]]
   [vexoding (Just 0x66) (MapVex 0x01) 0x2F Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     RO    T_V128_Low64      Reg
      , op     RO    T_VM128_Low64     RM
      ]]

i_comiss :: X86Insn
i_comiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "COMISS"
   []
   [Modified [ZF,PF,CF], Unset [OF,SF,AF]]
   [legacyEncoding Nothing Map0F 0x2F Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RO    T_V128_Low32      Reg
      , op    RO    T_VM128_Low32     RM
      ]]

i_vcomiss :: X86Insn
i_vcomiss = i "Compare scalar ordered single-precision floating-point values and set EFLAGS" "VCOMISS"
   []
   [Modified [ZF,PF,CF], Unset [OF,SF,AF]]
   [vexoding Nothing (MapVex 0x01) 0x2F Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     RO    T_V128_Low32      Reg
      , op     RO    T_VM128_Low32     RM
      ]]

i_cpuid :: X86Insn
i_cpuid = i "CPU identification" "CPUID"
   []
   []
   [legacyEncoding Nothing Map0F 0xA2 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_xAX     Implicit
      , op    RW    T_xCX     Implicit
      , op    WO    T_xBX     Implicit
      , op    WO    T_xDX     Implicit
      ]]

i_crc32 :: X86Insn
i_crc32 = i "Accumulate CRC32 value" "CRC32"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F38 0xF0 Nothing
      (opf { sizable = Just 0 })
      [LegacyModeSupport, LongModeSupport]
      [ op    RW    T_R      Reg
      , op    RO    T_RM     RM
      ]]

i_cvtdq2pd :: X86Insn
i_cvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTDQ2PD"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0xE6 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM     -- FIXME: it should be xmm_low64/m64 
      ]]

i_vcvtdq2pd :: X86Insn
i_vcvtdq2pd = i "Convert packed Int32 to packed double-precision floating-point values" "VCVTDQ2PD"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0xE6 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128        RM     -- FIXME: it should be xmm_low64/m64 or xmm/m128
      ]]

i_cvtdq2ps :: X86Insn
i_cvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTDQ2PS"
   []
   []
   [legacyEncoding Nothing Map0F 0x5B Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvtdq2ps :: X86Insn
i_vcvtdq2ps = i "Convert packed Int32 to packed single-precision floating-point values" "VCVTDQ2PS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x5B Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvtpd2dq :: X86Insn
i_cvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DQ"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0xE6 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvtpd2dq :: X86Insn
i_vcvtpd2dq = i "Convert packed double-precision floating-point values to packed Int32" "VCVTPD2DQ"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0xE6 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvtpd2di :: X86Insn
i_cvtpd2di = i "Convert packed double-precision floating-point values to packed Int32" "CVTPD2DI"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x2D Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V64          Reg
      , op    RO    T_VM128        RM
      ]]

i_cvtpd2ps :: X86Insn
i_cvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "CVTPD2PS"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x5A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvtpd2ps :: X86Insn
i_vcvtpd2ps = i "Convert packed double-precision floating-point values to packed single-precision floating-point values" "VCVTPD2PS"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x5A Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvtpi2pd :: X86Insn
i_cvtpi2pd = i "Convert packed Int32 to packed double-precision floating-point values" "CVTPI2PD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x2A Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM64         RM
      ]]

i_cvtpi2ps :: X86Insn
i_cvtpi2ps = i "Convert packed Int32 to packed single-precision floating-point values" "CVTPI2PS"
   []
   []
   [legacyEncoding Nothing Map0F 0x2A Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM64         RM
      ]]

i_cvtps2dq :: X86Insn
i_cvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2DQ"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x5B Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvtps2dq :: X86Insn
i_vcvtps2dq = i "Convert packed single-precision floating-point values to packed Int32" "VCVTPS2DQ"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x5B Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvtps2pd :: X86Insn
i_cvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "CVTPS2PD"
   []
   []
   [legacyEncoding Nothing Map0F 0x5A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvtps2pd :: X86Insn
i_vcvtps2pd = i "Convert packed single-precision floating-point values to packed double-precision floating-point values" "VCVTPS2PD"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x5A Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvtps2pi :: X86Insn
i_cvtps2pi = i "Convert packed single-precision floating-point values to packed Int32" "CVTPS2PI"
   []
   []
   [legacyEncoding Nothing Map0F 0x2D Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V64          Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_cvtsd2si :: X86Insn
i_cvtsd2si = i "Convert scalar double-precision floating-point value to integer" "CVTSD2SI"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2D Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_R32_64       Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_vcvtsd2si :: X86Insn
i_vcvtsd2si = i "Convert scalar double-precision floating-point value to integer" "VCVTSD2SI"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x2D Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_R32_64         Reg
      , op     RO    T_VM128_Low64    RM
      ]]

i_cvtsd2ss :: X86Insn
i_cvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "CVTSD2SS"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x5A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_vcvtsd2ss :: X86Insn
i_vcvtsd2ss = i "Convert scalar double-precision floating-point value to scalar single-precision floating-point value" "VCVTSD2SS"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x5A Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low64  RM
      ]]

i_cvtsi2sd :: X86Insn
i_cvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "CVTSI2SD"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128      Reg
      , op    RO    T_RM32_64   RM
      ]]

i_vcvtsi2sd :: X86Insn
i_vcvtsi2sd = i "Convert Int32 to scalar double-precision floating-point value" "VCVTSI2SD"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x2A Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128     Reg
      , op     RO    T_V128     Vvvv
      , op     RO    T_RM32_64  RM
      ]]


i_cvtsi2ss :: X86Insn
i_cvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "CVTSI2SS"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128      Reg
      , op    RO    T_RM32_64   RM
      ]]

i_vcvtsi2ss :: X86Insn
i_vcvtsi2ss = i "Convert Int32 to scalar single-precision floating-point value" "VCVTSI2SS"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x2A Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128     Reg
      , op     RO    T_V128     Vvvv
      , op     RO    T_RM32_64  RM
      ]]

i_cvtss2sd :: X86Insn
i_cvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "CVTSS2SD"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5A Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128_Low32  RM
      ]]

i_vcvtss2sd :: X86Insn
i_vcvtss2sd = i "Convert scalar single-precision floating-point value to scalar double-precision floating-point value" "VCVTSS2SD"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x5A Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low32  RM
      ]]

i_cvtss2si :: X86Insn
i_cvtss2si = i "Convert scalar single-precision floating-point value to Int32" "CVTSS2SI"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2D Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    WO    T_R32_64       Reg
      , op    RO    T_VM128_Low32  RM
      ]]

i_vcvtss2si :: X86Insn
i_vcvtss2si = i "Convert scalar single-precision floating-point value to Int32" "VCVTSS2SI"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x2D Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_R32_64       Reg
      , op     RO    T_VM128_Low32  RM
      ]]

i_cvttpd2dq :: X86Insn
i_cvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2DQ"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0xE6 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvttpd2dq :: X86Insn
i_vcvttpd2dq = i "Convert with truncation packed double-precision floating-point values to packed Int32" "VCVTTPD2DQ"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0xE6  Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvttpd2pi :: X86Insn
i_cvttpd2pi = i "Convert with truncation packed double-precision floating-point values to packed Int32" "CVTTPD2PI"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x2C Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V64          Reg
      , op    RO    T_VM128        RM
      ]]

i_cvttps2dq :: X86Insn
i_cvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2DQ"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5B Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_V128         Reg
      , op    RO    T_VM128        RM
      ]]

i_vcvttps2dq :: X86Insn
i_vcvttps2dq = i "Convert with truncation packed single-precision floating-point values to packed Int32" "VCVTTPS2DQ"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x5B Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_VM128_256    RM
      ]]

i_cvttps2pi :: X86Insn
i_cvttps2pi = i "Convert with truncation packed single-precision floating-point values to packed Int32" "CVTTPS2PI"
   []
   []
   [legacyEncoding Nothing Map0F 0x2C Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_V64          Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_cvttsd2si :: X86Insn
i_cvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "CVTTSD2SI"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x2C Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    WO    T_R32_64       Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_vcvttsd2si :: X86Insn
i_vcvttsd2si = i "Convert with truncation scalar double-precision floating-point value to integer" "VCVTTSD2SI"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x2C Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_R32_64         Reg
      , op     RO    T_VM128_Low64    RM
      ]]

i_cvttss2si :: X86Insn
i_cvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "CVTTSS2SI"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x2C Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    WO    T_R32_64       Reg
      , op    RO    T_VM128_Low32  RM
      ]]

i_vcvttss2si :: X86Insn
i_vcvttss2si = i "Convert with truncation scalar single-precision floating-point value to Int32" "VCVTTSS2SI"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x2C Nothing LIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_R32_64       Reg
      , op     RO    T_VM128_Low32  RM
      ]]

i_cwd :: X86Insn
i_cwd = i "Convert between words (sign-extend)" "CWD/CDQ/CQO"
   []
   []
   [legacyEncoding Nothing MapPrimary 0x99 Nothing
      opf
      [LegacyModeSupport, LongModeSupport]
      [ op    WO    T_xDX_xAX      Implicit
      , op    RO    T_AX_EAX_RAX   Implicit
      ]]

i_daa :: X86Insn
i_daa = i "Decimal adjust AL after addition" "DAA"
   []
   [Modified [AF,CF,SF,ZF,PF], Undefined [OF]]
   [legacyEncoding Nothing MapPrimary 0x27 Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_AL     Implicit ]]

i_das :: X86Insn
i_das = i "Decimal adjust AL after subtraction" "DAS"
   []
   [Modified [AF,CF,SF,ZF,PF], Undefined [OF]]
   [legacyEncoding Nothing MapPrimary 0x2F Nothing
      opf
      [LegacyModeSupport]
      [ op    RW    T_AL     Implicit ]]

i_dec :: X86Insn
i_dec = i "Decrement by 1" "DEC"
   []
   [Modified [OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xFE (Just 1)
      (opf { sizable = Just 0})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RW    T_RM     RM
      ]
   ,legacyEncoding Nothing MapPrimary 0x48 Nothing
      opf
      [LegacyModeSupport,Lockable]
      [ op    RW    T_R16_32    OpcodeLow3
      ]
   ]

i_div :: X86Insn
i_div = i "Unsigned divide" "DIV"
   [FailOnZero 0]
   [Undefined [CF,OF,SF,ZF,AF,PF]]
   [legacyEncoding Nothing MapPrimary 0xF6 (Just 6)
      (opf {sizable = Just 0})
      [Lockable, LegacyModeSupport, LongModeSupport]
      [ op    RO    T_RM        RM 
      , op    RW    T_xDX_xAX   Implicit
      ]]

i_divpd :: X86Insn
i_divpd = i "Divide packed double-precision floating-point values" "DIVPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F 0x5E Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vdivpd :: X86Insn
i_vdivpd = i "Divide packed double-precision floating-point values" "VDIVPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x01) 0x5E Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_divps :: X86Insn
i_divps = i "Divide packed float-precision floating-point values" "DIVPS"
   []
   []
   [legacyEncoding Nothing Map0F 0x5E Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128   Reg
      , op    RO    T_VM128  RM
      ]]

i_vdivps :: X86Insn
i_vdivps = i "Divide packed float-precision floating-point values" "VDIVPS"
   []
   []
   [vexoding Nothing (MapVex 0x01) 0x5E Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      ]]

i_divsd :: X86Insn
i_divsd = i "Divide scalar double-precision floating-point values" "DIVSD"
   []
   []
   [legacyEncoding (Just 0xF2) Map0F 0x5E Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE2]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128_Low64  RM
      ]]

i_vdivsd :: X86Insn
i_vdivsd = i "Divide scalar double-precision floating-point values" "VDIVSD"
   []
   []
   [vexoding (Just 0xF2) (MapVex 0x01) 0x5E Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low64  RM
      ]]

i_divss :: X86Insn
i_divss = i "Divide scalar single-precision floating-point values" "DIVSS"
   []
   []
   [legacyEncoding (Just 0xF3) Map0F 0x5E Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128_Low32  RM
      ]]

i_vdivss :: X86Insn
i_vdivss = i "Divide scalar single-precision floating-point values" "VDIVSS"
   []
   []
   [vexoding (Just 0xF3) (MapVex 0x01) 0x5E Nothing LWIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128_Low32  RM
      ]]

i_dppd :: X86Insn
i_dppd = i "Dot product of packed double precision floating-point values" "DPPD"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x41 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      , op    RO    T_Imm8         Imm
      ]]

i_vdppd :: X86Insn
i_vdppd = i "Dot product of packed double precision floating-point values" "VDPPD"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x41 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128         Reg
      , op     RO    T_V128         Vvvv
      , op     RO    T_VM128        RM
      , op     RO    T_Imm8         Imm
      ]]

i_dpps :: X86Insn
i_dpps = i "Dot product of packed single precision floating-point values" "DPPS"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x40 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_V128         Reg
      , op    RO    T_VM128        RM
      , op    RO    T_Imm8         Imm
      ]]

i_vdpps :: X86Insn
i_vdpps = i "Dot product of packed single precision floating-point values" "VDPPS"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x40 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_V128_256     Reg
      , op     RO    T_V128_256     Vvvv
      , op     RO    T_VM128_256    RM
      , op     RO    T_Imm8         Imm
      ]]

i_emms :: X86Insn
i_emms = i "Empty MMX technology state" "EMMS"
   []
   []
   [legacyEncoding Nothing Map0F 0x77 Nothing opf
      [LegacyModeSupport, LongModeSupport]
      []]

i_enter :: X86Insn
i_enter = i "Make stack frame for procedure parameters" "ENTER"
   []
   []
   [legacyEncoding Nothing MapPrimary 0xC8 Nothing
      opf
      [DefaultOperandSize64, LegacyModeSupport, LongModeSupport]
      [ op    RO    T_Imm16     Imm
      , op    RO    T_Imm8      Imm
      ]]

i_extractps :: X86Insn
i_extractps = i "Extract packed single precision floating-point value" "EXTRACTPS"
   []
   []
   [legacyEncoding (Just 0x66) Map0F3A 0x17 Nothing
      opf
      [LegacyModeSupport, LongModeSupport, Extension SSE4_1]
      [ op    RW    T_RM32         RM
      , op    RO    T_V128         Reg
      , op    RO    T_Imm8         Imm
      ]]

i_vextractps :: X86Insn
i_vextractps = i "Extract packed single precision floating-point value" "VEXTRACTPS"
   []
   []
   [vexoding (Just 0x66) (MapVex 0x03) 0x17 Nothing WIG
      [LegacyModeSupport, LongModeSupport, Extension AVX]
      [ op     WO    T_RM32         RM
      , op     RO    T_V128         Vvvv
      , op     RO    T_Imm8         Imm
      ]]



data FlaggedOpcode = FlaggedOpcode
   { fgOpcode        :: Word8
   , fgReversed      :: Bool
   , fgSized         :: Bool
   , fgSignExtended  :: Bool
   } deriving (Show)

-- | Return the different opcode for an encoding
getLegacyOpcodes :: LegEnc -> [FlaggedOpcode]
getLegacyOpcodes e = os
   where
      sz = sizable            (legacyOpcodeFields e)
      rv = reversable         (legacyOpcodeFields e)
      se = signExtendableImm8 (legacyOpcodeFields e)
      oc = legacyOpcode e
   
      os' = orig : szb ++ opb
      -- with reversable bit set
      os = case rv of
         Nothing -> os'
         Just x  -> os' ++ fmap rev os'
            where
               rev o = o { fgOpcode   = setBit (fgOpcode o) x
                         , fgReversed = True
                         }

      -- original opcode
      orig = FlaggedOpcode oc False False False
      -- with sizable and sign-extendable bits
      szb = case (sz,se) of
         (Nothing,Nothing) -> []
         (Nothing,Just _)  -> error "Invalid opcode fields"
         (Just x,Nothing)  -> [FlaggedOpcode (setBit oc x) False True False]
         (Just x,Just y)   -> 
            [ FlaggedOpcode (setBit oc x)            False True False
            , FlaggedOpcode (setBit (setBit oc x) y) False True True
            ]
      -- with operand in the last 3 bits of the opcode
      opb = case any ((==) OpcodeLow3) $ fmap opEnc (legacyParams e) of
         False -> []
         True  ->  fmap (\x -> FlaggedOpcode (oc+x) False False False) [1..7]

getEncodings :: [X86Insn] -> [(Encoding,X86Insn)]
getEncodings is = concatMap f is
   where
      f x = fmap (,x) (iEncoding x)

getVexOpcodes :: VexEnc -> [FlaggedOpcode]
getVexOpcodes e = [FlaggedOpcode (vexOpcode e) False False False]


-- | Build a legacy opcode map
buildLegacyOpcodeMap :: LegacyMap -> [X86Insn] -> V.Vector [(Encoding,X86Insn)]
buildLegacyOpcodeMap omap insns = buildOpcodeMap encs
   where
      encs = filter (ff . fst) (getEncodings insns)
      ff = \case
         LegacyEncoding x -> legacyOpcodeMap x == omap 
         _                -> False

-- | Build a VEX opcode map
buildVexOpcodeMap :: OpcodeMap -> [X86Insn] -> V.Vector [(Encoding,X86Insn)]
buildVexOpcodeMap omap insns = buildOpcodeMap encs
   where
      encs = filter (ff . fst) (getEncodings insns)
      ff = \case
         VexEncoding x -> vexOpcodeMap x == omap 
         _             -> False
            
            
-- | Build the opcode maps
buildOpcodeMap :: [(Encoding,X86Insn)] -> V.Vector [(Encoding,X86Insn)]
buildOpcodeMap encs = go encs Map.empty
   where
      go [] rs     = V.generate 256 $ \x -> case Map.lookup x rs of
         Nothing -> []
         Just xs -> xs
      go ((e,x):xs) rs = let
            os = fmap (fromIntegral . fgOpcode) (getOpcodes e)
         in go xs (insertAll os (e,x) rs)
      
      getOpcodes = \case
         LegacyEncoding x -> getLegacyOpcodes x
         VexEncoding x    -> getVexOpcodes x

      insertAll [] _ rs     = rs
      insertAll (o:os) x rs = insertAll os x (Map.insertWith (++) o [x] rs)


opcodeMapPrimary :: V.Vector [(Encoding,X86Insn)]
opcodeMapPrimary = buildLegacyOpcodeMap MapPrimary instructions

opcodeMap0F :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F = buildLegacyOpcodeMap Map0F instructions

opcodeMap0F38 :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F38 = buildLegacyOpcodeMap Map0F38 instructions

opcodeMap0F3A :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F3A = buildLegacyOpcodeMap Map0F3A instructions

opcodeMap3DNow :: V.Vector [(Encoding,X86Insn)]
opcodeMap3DNow = buildLegacyOpcodeMap Map3DNow instructions

opcodeMapVex1 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex1 = buildVexOpcodeMap (MapVex 1) instructions

opcodeMapVex2 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex2 = buildVexOpcodeMap (MapVex 2) instructions

opcodeMapVex3 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex3 = buildVexOpcodeMap (MapVex 3) instructions

-- We use a dummy encoding for 3DNow: because all the instructions use the same
amd3DNowEncoding :: Encoding
amd3DNowEncoding =
   legacyEncoding Nothing Map3DNow 0x0 Nothing
     opf
     []
     [ op    RW    T_V64          Reg
     , op    RO    T_VM64         RM
     ]
