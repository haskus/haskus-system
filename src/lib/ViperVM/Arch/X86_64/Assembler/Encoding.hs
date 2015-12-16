{-# LANGUAGE LambdaCase #-}

-- | Instruction encodings
module ViperVM.Arch.X86_64.Assembler.Encoding
   ( Properties(..)
   , X86Extension(..)
   , OperandSpec(..)
   , OperandEnc(..)
   , hasImmediate
   , isImmediate
   , LegacyOpcodeFields(..)
   , OpcodeMap(..)
   , AccessMode(..)
   -- * Generic API
   , Encoding(..)
   , isLegacyEncoding
   , isVexEncoding
   , encOpcodeExt
   , encOperands
   , encProperties
   , encSizableBit
   , encSignExtendImmBit
   , requireModRM
   -- * Legacy encoding
   , LegEnc(..)
   , legEncRequireModRM
   -- * VEX encoding
   , VexEnc (..)
   , VexLW (..)
   , vexEncRequireModRM
   )
where

import Data.Word
import Data.Maybe (isJust)
import ViperVM.Arch.X86_64.MicroArch
import ViperVM.Arch.X86_64.Assembler.Operand

data Properties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch

   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | DoubleSizable            -- ^ Default size is 32+32 (a pair of registers is used)
                              --   Can be extended to 64+64 with Rex.W
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
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

data OperandEnc
   = E_ModRM      -- ^ Operand stored in ModRM.rm
   | E_ModReg     -- ^ Operand stored in ModRM.reg
   | E_Imm        -- ^ Operand stored in immediate bytes
   | E_Imm8_7_4   -- ^ Operand stored in bits [7:4] of the immediate byte
   | E_Imm8_3_0   -- ^ Operand stored in bits [3:0] of the immediate byte
   | E_Implicit   -- ^ Implicit
   | E_VexV       -- ^ Operand stored in Vex.vvvv field
   | E_OpReg      -- ^ Operand stored in opcode 3 last bits
   deriving (Show,Eq)

isImmediate :: OperandEnc -> Bool
isImmediate = \case
   E_Imm       -> True
   E_Imm8_7_4  -> True
   E_Imm8_3_0  -> True
   _           -> False

hasImmediate :: Encoding -> Bool
hasImmediate e = any (isImmediate . opEnc) (encOperands e)

data Encoding
   = LegacyEncoding LegEnc
   | VexEncoding    VexEnc
   deriving (Show)


isLegacyEncoding :: Encoding -> Bool
isLegacyEncoding (LegacyEncoding _) = True
isLegacyEncoding _                  = False

isVexEncoding :: Encoding -> Bool
isVexEncoding (VexEncoding _) = True
isVexEncoding _               = False

encOpcodeExt :: Encoding -> Maybe Word8
encOpcodeExt (LegacyEncoding e) = legEncOpcodeExt e
encOpcodeExt (VexEncoding    e) = vexEncOpcodeExt e

encOperands :: Encoding -> [OperandSpec]
encOperands (LegacyEncoding e)  = legEncParams e
encOperands (VexEncoding    e)  = vexEncParams e

encProperties :: Encoding -> [Properties]
encProperties (LegacyEncoding e) = legEncProperties e
encProperties (VexEncoding    _) = []

encSizableBit :: Encoding -> Maybe Int
encSizableBit (LegacyEncoding e) = sizable (legEncOpcodeFields e)
encSizableBit _                  = Nothing

encSignExtendImmBit :: Encoding -> Maybe Int
encSignExtendImmBit (LegacyEncoding e) = signExtendableImm8 (legEncOpcodeFields e)
encSignExtendImmBit _                  = Nothing

data LegEnc = LegEnc
   { legEncMandatoryPrefix :: Maybe Word8        -- ^ Mandatory prefix
   , legEncOpcodeMap       :: OpcodeMap          -- ^ Map
   , legEncOpcode          :: Word8              -- ^ Opcode
   , legEncOpcodeExt       :: Maybe Word8        -- ^ Opcode extension in ModRM.reg
   , legEncOpcodeFields    :: LegacyOpcodeFields -- ^ Fields in the opcode
   , legEncProperties      :: [Properties]       -- ^ Encoding properties
   , legEncParams          :: [OperandSpec]      -- ^ Operand encoding
   }
   deriving (Show)

data VexEnc = VexEnc
   { vexEncMandatoryPrefix :: Maybe Word8       -- ^ Mandatory prefix
   , vexEncOpcodeMap       :: OpcodeMap         -- ^ Map
   , vexEncOpcode          :: Word8             -- ^ Opcode
   , vexEncOpcodeExt       :: Maybe Word8       -- ^ Opcode extension in ModRM.reg
   , vexEncLW              :: VexLW
   , vexEncParams          :: [OperandSpec]     -- ^ Operand encoding
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

legEncRequireModRM :: LegEnc -> Bool
legEncRequireModRM e = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg 
      hasOpExt = isJust (legEncOpcodeExt e)

      -- has operands in ModRM
      hasOps   = any matchEnc (legEncParams e)
      matchEnc x = case opEnc x of
         E_ModRM     -> True
         E_ModReg    -> True
         E_Imm       -> False
         E_Imm8_7_4  -> False
         E_Imm8_3_0  -> False
         E_Implicit  -> False
         E_VexV      -> False
         E_OpReg     -> False

vexEncRequireModRM :: VexEnc -> Bool
vexEncRequireModRM e = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg 
      hasOpExt = isJust (vexEncOpcodeExt e)

      -- has operands in ModRM
      hasOps   = any matchEnc (vexEncParams e)
      matchEnc x = case opEnc x of
         E_ModRM     -> True
         E_ModReg    -> True
         E_Imm       -> False
         E_Imm8_7_4  -> False
         E_Imm8_3_0  -> False
         E_Implicit  -> False
         E_VexV      -> False
         E_OpReg     -> False

requireModRM :: Encoding -> Bool
requireModRM enc = case enc of
   LegacyEncoding e -> legEncRequireModRM e
   VexEncoding    e -> vexEncRequireModRM e

data OpcodeMap
   = MapPrimary
   | Map0F
   | Map0F01
   | Map0F38
   | Map0F3A
   | Map3DNow
   | MapX87
   | MapVex Int
   | MapXop Int
   deriving (Show,Eq)

data OperandSpec = OperandSpec
   { opMode :: AccessMode
   , opType :: OperandType
   , opEnc  :: OperandEnc
   } deriving (Show)

data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
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

