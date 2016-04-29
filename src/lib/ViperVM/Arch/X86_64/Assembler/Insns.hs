{-# LANGUAGE LambdaCase #-}

-- | X86 (and X87) instructions
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
   , encOpcodeMap
   , encOperands
   , encMandatoryPrefix
   , encProperties
   , encParams
   , encSizableBit
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
      , legacySizable         :: Maybe Int            -- ^ Operand size is 8 if the given bit is
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
   | L0     -- ^ Vex.L set to 0
   | L1     -- ^ Vex.L set to 1
   | LIG    -- ^ Vex.L ignored
   | LWIG   -- ^ Ignore Vex.W and Vex.L
   deriving (Show)

-- | Instruction properties
data Properties
   = FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   | MemAlign Int             -- ^ Memory alignment constraint in bytes
   deriving (Show,Eq)

-- | Encoding properties
data EncodingProperties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | Repeatable               -- ^ Allow repeat prefix
   | DoubleSizable            -- ^ Default size is 32+32 (a pair of registers is used)
                              --   Can be extended to 64+64 with Rex.W
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | RequireRexW              -- ^ Require REX.W
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
   , legacySizable         = Nothing
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
   , vexOpcodeExt       = Just 0
   , vexLW              = LWIG
   , vexProperties      = []
   , vexParams          = []
   }

-- | Operand
op :: AccessMode -> OperandType -> OperandEnc -> OperandSpec
op = OperandSpec

-- We use a dummy encoding for 3DNow: because all the instructions use the same
amd3DNowEncoding :: Encoding
amd3DNowEncoding = leg
   { legacyOpcodeMap = Map3DNow
   , legacyParams    = [ op    RW    T_V64          Reg
                       , op    RO    T_VM64         RM
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
encProperties VexEncoding      {} = []

encParams :: Encoding -> [OperandSpec]
encParams e@LegacyEncoding {} = legacyParams e
encParams e@VexEncoding    {} = vexParams e

encSizableBit :: Encoding -> Maybe Int
encSizableBit e@LegacyEncoding {} = legacySizable e
encSizableBit _                   = Nothing

encSignExtendImmBit :: Encoding -> Maybe Int
encSignExtendImmBit e@LegacyEncoding {} = legacySignExtendable e
encSignExtendImmBit _                   = Nothing

encReversableBit :: Encoding -> Maybe Int
encReversableBit e@LegacyEncoding {} = legacyReversable e
encReversableBit _                   = Nothing

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
                           , legacyParams          = [ op    RW    T_AX     Implicit ]
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
                           , legacyParams          = [ op    RW    T_AX     Implicit
                                                     , op    RO    T_Imm8   Imm
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
                           , legacyParams          = [ op    RW    T_AX     Implicit
                                                     , op    RO    T_Imm8   Imm
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
                           , legacyParams          = [ op    RW    T_AX     Implicit ]
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_Accu   Implicit
                                                     , op    RO    T_Imm    Imm
                                                     ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x10
                           , legacySizable         = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_R      Reg
                                                     ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 2
                           , legacySizable         = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_Imm    Imm
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
                           , legacyParams          = [ op    RW    T_R32_64    Reg
                                                     , op    RO    T_RM32_64   RM
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_Accu   Implicit
                                                     , op    RO    T_Imm    Imm
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x00
                           , legacySizable         = Just 0
                           , legacyReversable      = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_R      Reg
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 0
                           , legacySizable         = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_Imm    Imm
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams             = [ op     WO    T_V128_256     Reg
                                                     , op     RO    T_V128_256     Vvvv
                                                     , op     RO    T_VM128_256    RM
                                                     ]
                           }
                       ]
   }

i_addps :: X86Insn
i_addps = insn
   { insnDesc        = "Add packed float-precision floating-point values"
   , insnMnemonic    = "ADDPS"
   , insnEncodings   = [ leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x58
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , Extension SSE
                                                     ]
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     ]
                           }
                       ]
   }

i_vaddps :: X86Insn
i_vaddps = insn
   { insnDesc        = "Add packed float-precision floating-point values"
   , insnMnemonic    = "VADDPS"
   , insnEncodings   = [ vex
                           { vexOpcodeMap       = MapVex 0x01
                           , vexOpcode          = 0x58
                           , vexLW              = WIG
                           , vexProperties      = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  , Extension AVX
                                                  ]
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128_Low64  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low64  RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128_Low32  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low32  RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams             = [ op     WO    T_V128_256     Reg
                                                     , op     RO    T_V128_256     Vvvv
                                                     , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_R32_64       Reg
                                                     , op    RO    T_RM32_64      RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128        RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128        RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128        RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128        RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_VM128        RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
                                                     , op    RO    T_Imm8         Imm
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_VM128        RM
                                                  , op     RO    T_Imm8         Imm
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_Accu   Implicit
                                                  , op    RO    T_Imm    Imm
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x20
                           , legacySizable      = Just 0
                           , legacyReversable   = Just 1
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_RM     RM
                                                  , op    RO    T_R      Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 4
                           , legacySizable         = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_Imm    Imm
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
                           , vexParams       = [ op    WO    T_R32_64     Reg
                                               , op    RO    T_R32_64     Vvvv
                                               , op    RO    T_RM32_64    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    RW    T_V128   Reg
                                                  , op    RO    T_VM128  RM
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
                           , vexParams       = [ op     WO    T_V128_256     Reg
                                               , op     RO    T_V128_256     Vvvv
                                               , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    RW    T_V128   Reg
                                                  , op    RO    T_VM128  RM
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
                           , vexParams       = [ op     WO    T_V128_256     Reg
                                               , op     RO    T_V128_256     Vvvv
                                               , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    RW    T_RM16   RM
                                                  , op    RO    T_R16    Reg
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     , op    RO    T_Imm8   Imm
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
                                                  , op     RO    T_Mask         Imm8l
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
                           , vexParams       = [ op    WO    T_R32_64     Reg
                                               , op    RO    T_RM32_64    RM
                                               , op    RO    T_R32_64     Vvvv
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     , op    RO    T_Imm8   Imm
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
                                                  , op     RO    T_Imm8         Imm
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     , op    RO    T_XMM0   Implicit
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
                                                  , op     RO    T_V128_256     Imm8h
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     , op    RO    T_XMM0   Implicit
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
                                                  , op     RO    T_V128_256     Imm8h
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
                           , vexParams       = [ op    WO    T_R32_64     Vvvv
                                               , op    RO    T_RM32_64    RM
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
                           , vexParams       = [ op    WO    T_R32_64     Vvvv
                                               , op    RO    T_RM32_64    RM
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
                           , vexParams       = [ op    WO    T_R32_64     Vvvv
                                               , op    RO    T_RM32_64    RM
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
                           , legacyParams       = [ op    RO    T_R16_32 Reg
                                                  , op    RO    T_M_PAIR RM
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
                           , legacyParams       = [ op    WO    T_R      Reg
                                                  , op    RO    T_RM     RM
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
                           , legacyParams       = [ op    WO    T_R      Reg
                                                  , op    RO    T_RM     RM
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
                           , legacyParams       = [ op    RW    T_R32_64 OpcodeLow3 ]
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
                           , legacyParams       = [ op    RO    T_RM16_32_64   RM
                                                  , op    RO    T_R16_32_64    Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = Map0F
                           , legacyOpcode       = 0xBA
                           , legacyOpcodeExt    = Just 4
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RO    T_RM16_32_64   RM
                                                  , op    RO    T_Imm8         Imm
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_R16_32_64    Reg
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_Imm8         Imm
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_R16_32_64    Reg
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_Imm8         Imm
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_R16_32_64    Reg
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
                           , legacyParams       = [ op    RW    T_RM16_32_64   RM
                                                  , op    RO    T_Imm8         Imm
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
                              , vexParams       = [ op    WO    T_R32_64     Reg
                                                  , op    RO    T_RM32_64    RM
                                                  , op    RO    T_R32_64     Vvvv
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
                           , legacyParams       = [ op    RO    T_Rel16_32    Imm ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 2
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ op    RO    T_RM16_32      RM ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 2
                           , legacyProperties   = [ LongModeSupport
                                                  , DefaultOperandSize64
                                                  ]
                           , legacyParams       = [ op    RO    T_RM64         RM ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x9A
                           , legacyProperties   = [LegacyModeSupport]
                           , legacyParams       = [ op    RO    T_PTR16_16_32    Imm ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0xFF
                           , legacyOpcodeExt    = Just 3
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RO    T_M16_XX       RM]
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
                           , legacyParams       = [ op    RW    T_Accu         Implicit]
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
                           , legacyParams       = [ op    RO    T_M      RM  ]
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacyParams       = [ op    RW    T_R16_32_64   Reg
                                                  , op    RO    T_RM16_32_64  RM
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_Accu   Implicit
                                                  , op    RO    T_Imm    Imm
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x38
                           , legacySizable      = Just 0
                           , legacyReversable   = Just 1
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_RM     RM
                                                  , op    RO    T_R      Reg
                                                  ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x80
                           , legacyOpcodeExt       = Just 7
                           , legacySizable         = Just 0
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM     RM
                                                     , op    RO    T_Imm    Imm
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
                           , legacyParams             = [ op    RW    T_V128   Reg
                                                        , op    RO    T_VM128  RM
                                                        , op    RO    T_Imm8   Imm
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
                           , vexParams             = [ op     WO    T_V128_256     Reg
                                                     , op     RO    T_V128_256     Vvvv
                                                     , op     RO    T_VM128_256    RM
                                                     , op     RO    T_Imm8         Imm
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
                           , legacyParams       = [ op    RW    T_V128   Reg
                                                  , op    RO    T_VM128  RM
                                                  , op    RO    T_Imm8   Imm
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
                           , vexParams       = [ op     WO    T_V128_256     Reg
                                               , op     RO    T_V128_256     Vvvv
                                               , op     RO    T_VM128_256    RM
                                               , op     RO    T_Imm8         Imm
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RO    T_rSI    Implicit
                                                  , op    RO    T_rDI    Implicit
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
                           , legacyParams             = [ op    RW    T_V128   Reg
                                                        , op    RO    T_VM128  RM
                                                        , op    RO    T_Imm8   Imm
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
                           , vexParams             = [ op     WO    T_V128      Reg
                                                     , op     RO    T_V128      Vvvv
                                                     , op     RO    T_VM128     RM
                                                     , op     RO    T_Imm8      Imm
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
                           , legacyParams             = [ op    RW    T_V128   Reg
                                                        , op    RO    T_VM128  RM
                                                        , op    RO    T_Imm8   Imm
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
                           , vexParams             = [ op     WO    T_V128      Reg
                                                     , op     RO    T_V128      Vvvv
                                                     , op     RO    T_VM128     RM
                                                     , op     RO    T_Imm8      Imm
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  , Arch Intel486
                                                  ]
                           , legacyParams       = [ op    RW    T_RM     RM
                                                  , op    RO    T_Accu   Implicit
                                                  , op    RO    T_R      Reg
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
                           , legacyProperties   = [ DoubleSizable
                                                  , Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  , Arch IntelPentium
                                                  , Extension CX8
                                                  ]
                           , legacyParams       = [ op    RW    T_M64_128   RM ]
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
                           , legacyParams             = [ op    RO    T_V128_Low64      Reg
                                                        , op    RO    T_VM128_Low64     RM
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
                           , vexParams          = [ op     RO    T_V128_Low64      Reg
                                                  , op     RO    T_VM128_Low64     RM
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
                           , legacyParams       = [ op    RO    T_V128_Low32      Reg
                                                  , op    RO    T_VM128_Low32     RM
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
                           , vexParams       = [ op     RO    T_V128_Low32      Reg
                                               , op     RO    T_VM128_Low32     RM
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
                           , legacyParams       = [ op    RW    T_xAX     Implicit
                                                  , op    RW    T_xCX     Implicit
                                                  , op    WO    T_xBX     Implicit
                                                  , op    WO    T_xDX     Implicit
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_R      Reg
                                                     , op    RO    T_RM     RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM     -- FIXME: it should be xmm_low64/m64 
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_VM128        RM     -- FIXME: it should be xmm_low64/m64 or xmm/m128
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
                           , legacyParams       = [ op    WO    T_V128         Reg
                                                  , op    RO    T_VM128        RM
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
                           , vexParams       = [ op     WO    T_V128_256     Reg
                                               , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    WO    T_V64          Reg
                                                     , op    RO    T_VM128        RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM64         RM
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
                           , legacyParams       = [ op    WO    T_V128         Reg
                                                  , op    RO    T_VM64         RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    WO    T_V128         Reg
                                                  , op    RO    T_VM128        RM
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
                           , vexParams       = [ op     WO    T_V128         Reg
                                               , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    WO    T_V64          Reg
                                                  , op    RO    T_VM128_Low64  RM
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
                           , legacyParams          = [ op    WO    T_R32_64       Reg
                                                     , op    RO    T_VM128_Low64  RM
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
                           , vexParams          = [ op     WO    T_R32_64         Reg
                                                  , op     RO    T_VM128_Low64    RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128_Low64  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low64  RM
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
                           , legacyParams          = [ op    WO    T_V128      Reg
                                                     , op    RO    T_RM32_64   RM
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
                           , vexParams          = [ op     WO    T_V128     Reg
                                                  , op     RO    T_V128     Vvvv
                                                  , op     RO    T_RM32_64  RM
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
                           , legacyParams          = [ op    WO    T_V128      Reg
                                                     , op    RO    T_RM32_64   RM
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
                           , vexParams          = [ op     WO    T_V128     Reg
                                                  , op     RO    T_V128     Vvvv
                                                  , op     RO    T_RM32_64  RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128_Low32  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low32  RM
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
                           , legacyParams          = [ op    WO    T_R32_64       Reg
                                                     , op    RO    T_VM128_Low32  RM
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
                           , vexParams          = [ op     WO    T_R32_64       Reg
                                                  , op     RO    T_VM128_Low32  RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    WO    T_V64          Reg
                                                     , op    RO    T_VM128        RM
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
                           , legacyParams          = [ op    WO    T_V128         Reg
                                                     , op    RO    T_VM128        RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    WO    T_V64          Reg
                                                  , op    RO    T_VM128_Low64  RM
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
                           , legacyParams          = [ op    WO    T_R32_64       Reg
                                                     , op    RO    T_VM128_Low64  RM
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
                           , vexParams          = [ op     WO    T_R32_64         Reg
                                                  , op     RO    T_VM128_Low64    RM
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
                           , legacyParams          = [ op    WO    T_R32_64       Reg
                                                     , op    RO    T_VM128_Low32  RM
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
                           , vexParams          = [ op     WO    T_R32_64       Reg
                                                  , op     RO    T_VM128_Low32  RM
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
                           , legacyParams       = [ op    WO    T_xDX_xAX      Implicit
                                                  , op    RO    T_AX_EAX_RAX   Implicit
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
                           , legacyParams       = [ op    RW    T_AL     Implicit ]
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
                           , legacyParams       = [ op    RW    T_AL     Implicit ]
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ Lockable
                                                  , LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_RM     RM ]
                           }
                       , leg
                           { legacyOpcodeMap    = MapPrimary
                           , legacyOpcode       = 0x48
                           , legacyProperties   = [ LegacyModeSupport
                                                  , Lockable
                                                  ]
                           , legacyParams       = [ op    RW    T_R16_32    OpcodeLow3]
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
                           , legacySizable      = Just 0
                           , legacyProperties   = [ LegacyModeSupport
                                                  , LongModeSupport
                                                  ]
                           , legacyParams       = [ op    RW    T_xDX_xAX   Implicit
                                                  , op    RO    T_RM        RM 
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams       = [ op    RW    T_V128   Reg
                                                  , op    RO    T_VM128  RM
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
                           , vexParams       = [ op     WO    T_V128_256     Reg
                                               , op     RO    T_V128_256     Vvvv
                                               , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128_Low64  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low64  RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128_Low32  RM
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128_Low32  RM
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
                                                     , op    RO    T_Imm8         Imm
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
                           , vexParams          = [ op     WO    T_V128         Reg
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_VM128        RM
                                                  , op     RO    T_Imm8         Imm
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
                           , legacyParams          = [ op    RW    T_V128         Reg
                                                     , op    RO    T_VM128        RM
                                                     , op    RO    T_Imm8         Imm
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
                                                  , op     RO    T_Imm8         Imm
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
                           , legacyParams       = [ op    RO    T_Imm16     Imm
                                                  , op    RO    T_Imm8      Imm
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
                           , legacyParams          = [ op    RW    T_RM32         RM
                                                     , op    RO    T_V128         Reg
                                                     , op    RO    T_Imm8         Imm
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
                           , vexParams          = [ op     WO    T_RM32         RM
                                                  , op     RO    T_V128         Vvvv
                                                  , op     RO    T_Imm8         Imm
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RW    T_ST0     Implicit
                                                   , op  RO    T_M80dec  RM 
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
                           , legacyParams        = [ op  RO    T_ST0     Implicit
                                                   , op  RW    T_M80bcd  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  WO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST1       Implicit
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST        RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST        RM 
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RO    T_ST   RM   -- not really RO, only change the tag
                                                   ]                       -- associated to the register
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
                           , legacyParams        = [ op  RO    T_ST0   Implicit
                                                   , op  RO    T_MInt  RM 
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
                           , legacyParams        = [ op  RO    T_ST0   Implicit
                                                   , op  RO    T_MInt  RM 
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
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_MInt32  RM ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_MInt16  RM ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_MInt64  RM ]
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
                           , legacyParams        = [ op  RO    T_ST0   Implicit
                                                   , op  WO    T_MInt  RM 
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
                           , legacyParams        = [ op  RO    T_ST0   Implicit
                                                   , op  WO    T_MInt  RM 
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_ST0     Implicit
                                                   , op  WO    T_MInt64  RM 
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
                           , legacyParams        = [ op  RO    T_ST0   Implicit
                                                   , op  WO    T_MInt  RM 
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 1
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_ST0     Implicit
                                                   , op  WO    T_MInt64  RM 
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 5
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  WO    T_ST0       Implicit
                                                   , op  RO    T_M80real   RM
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  WO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RO    T_M16  RM ]
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
                           , legacyParams        = [ op  RO    T_M14_28  RM ]
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RW    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit ]
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
                           , legacyParams        = [ op  RO    T_M94_108  RM ]
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
                           , legacyParams        = [ op  WO    T_M94_108  RM ]
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  WO    T_ST1       Implicit
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  WO    T_ST_MReal  RM
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  WO    T_ST_MReal  RM
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDB
                           , legacyOpcodeExt     = Just 7
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_ST0      Implicit
                                                   , op  WO    T_M80real  RM
                                                   ]
                           }
                       , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDD
                           , legacyOpcodeExt     = Just 3
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  RO    T_ST0      Implicit
                                                   , op  WO    T_ST       RM
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
                           , legacyParams        = [ op  WO    T_M16  RM ]
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
                           , legacyParams        = [ op  WO    T_M14_28  RM ]
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
                           , legacyParams        = [ op  WO    T_M16  RM ]
                           }
                        , leg
                           { legacyOpcodeMap     = MapPrimary
                           , legacyOpcode        = 0xDF
                           , legacyOpcodeFullExt = Just 0xE0
                           , legacyProperties    = [ Extension FPU ]
                           , legacyParams        = [ op  WO    T_AX  Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RW    T_ST0       Implicit
                                                   , op  RO    T_ST_MReal  RM 
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RO    T_MInt   RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  RO    T_ST0    Implicit
                                                   , op  RO    T_ST     RM 
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit
                                                   , op  RO    T_ST1       Implicit
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
                           , legacyParams        = [ op  RO    T_ST0       Implicit ]
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
                           , legacyParams        = [ op  RW    T_ST0    Implicit
                                                   , op  RW    T_ST     RM 
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
                           , legacyParams        = [ op  RO    T_M512     RM ]
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
                           , legacyParams        = [ op  RO    T_M512     RM ]
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
                           , legacyParams        = [ op  WO    T_M512     RM ]
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
                           , legacyParams        = [ op  RO    T_M512     RM ]
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  WO    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit 
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
                           , legacyParams        = [ op  RW    T_ST0  Implicit
                                                   , op  RO    T_ST1  Implicit 
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_V128_256     Vvvv
                                                  , op     RO    T_VM128_256    RM
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_xDX_xAX   Implicit
                                                     , op    RO    T_RM        RM
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_xDX_xAX   Implicit
                                                     , op    RO    T_RM        RM
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0xAF
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_R16_32_64     Reg
                                                     , op    RO    T_RM16_32_64    RM
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x69
                           , legacySignExtendable  = Just 1
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_R16_32_64    Reg
                                                     , op    RO    T_RM16_32_64   RM
                                                     , op    RO    T_Imm          Imm
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    WO    T_AL_AX_EAX   Implicit
                                                     , op    RO    T_Imm8        Imm
                                                     ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xEC
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    WO    T_AL_AX_EAX   Implicit
                                                     , op    RO    T_DX          Implicit
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ Lockable
                                                     , LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RW    T_RM   RM ]
                           }
                       , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0x40
                           , legacyProperties      = [ LegacyModeSupport
                                                     , Lockable
                                                     ]
                           , legacyParams          = [ op    RW    T_R16_32    OpcodeLow3 ]
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
                           , legacySizable         = Just 0
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     , DefaultOperandSize64
                                                     , Repeatable
                                                     ]
                           , legacyParams          = [ op    RW    T_rDI   Implicit
                                                     , op    RO    T_DX    Implicit
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
                           , legacyParams          = [ op    RW    T_V128   Reg
                                                     , op    RO    T_VM128  RM
                                                     , op    RO    T_Imm8   Imm
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
                           , vexParams          = [ op     WO    T_V128     Reg
                                                  , op     RO    T_V128     Vvvv
                                                  , op     RO    T_VM128    RM
                                                  , op     RO    T_Imm8     Imm
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
                           , legacyParams          = [ op    RO    T_3   Implicit ]
                           }
                        , leg
                           { legacyOpcodeMap       = MapPrimary
                           , legacyOpcode          = 0xCD
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op    RO    T_Imm8 Imm ]
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
                           , legacyParams          = [ op    RO    T_3   Implicit ]
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
                           , legacyParams          = [ op    RO    T_M   RM ]
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
                           , legacyParams          = [ op   RO    T_R32_64    Reg
                                                     , op   RO    T_M128      RM
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x87
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x83
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x82
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x86
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_CX_ECX_RCX    Implicit
                                                     , op   RO    T_Rel8          Imm
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x84
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8F
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8D
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8C
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8E
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x85
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x81
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8B
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x89
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x80
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x8A
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcodeMap       = Map0F
                           , legacyOpcode          = 0x88
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
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
                           , legacyParams          = [ op   RO    T_Rel8    Imm ]
                           }
                        , leg
                           { legacyOpcode          = 0xE9
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_Rel16_32    Imm ]
                           }
                        , leg
                           { legacyOpcode          = 0xFF
                           , legacyOpcodeExt       = Just 4
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_RM16_32_64    RM ]
                           }
                        , leg
                           { legacyOpcode          = 0xEA
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_PTR16_16_32    Imm ]
                           }
                        , leg
                           { legacyOpcode          = 0xFF
                           , legacyOpcodeExt       = Just 5
                           , legacyProperties      = [ LegacyModeSupport
                                                     , LongModeSupport
                                                     ]
                           , legacyParams          = [ op   RO    T_M16_XX    Imm ]
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
                           , legacyParams          = [ op   WO    T_AH    Implicit ]
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
                           , legacyParams          = [ op   WO    T_R16_32  Reg
                                                     , op   RO    T_RM16_32 RM
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
                           , legacyParams          = [ op    WO    T_V128   Reg
                                                     , op    RO    T_M128   RM
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
                           , vexParams          = [ op     WO    T_V128_256     Reg
                                                  , op     RO    T_M128_256     RM
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
                           , legacyParams          = [ op    RO    T_M32   RM ]
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
                           , vexParams          = [ op     RO    T_M32     RM ]
                           }
                       ]
   }

