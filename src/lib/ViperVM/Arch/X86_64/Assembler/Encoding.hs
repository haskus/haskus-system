{-# LANGUAGE LambdaCase #-}

-- | Instruction encodings
module ViperVM.Arch.X86_64.Assembler.Encoding
   ( Properties(..)
   , EncodingProperties(..)
   , X86Extension(..)
   , OperandSpec(..)
   , OperandEnc(..)
   , hasImmediate
   , isImmediate
   , LegacyOpcodeFields(..)
   , OpcodeMap(..)
   , LegacyMap(..)
   , AccessMode(..)
   , Variant(..)
   -- * Generic API
   , Encoding(..)
   , isLegacyEncoding
   , isVexEncoding
   , encOpcode
   , encOpcodeExt
   , encOpcodeMap
   , encOperands
   , encMandatoryPrefix
   , encProperties
   , encSizableBit
   , encSignExtendImmBit
   , encReversableBit
   , encLockable
   , encRequireModRM
   -- * Legacy encoding
   , LegEnc(..)
   -- * VEX encoding
   , VexEnc (..)
   , VexLW (..)
   )
where

import Data.Word
import Data.Maybe (isJust)
import ViperVM.Arch.X86_64.MicroArch
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Opcode

-- | Instruction properties
data Properties
   = FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   deriving (Show,Eq)

-- | Encoding properties
data EncodingProperties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | DoubleSizable            -- ^ Default size is 32+32 (a pair of registers is used)
                              --   Can be extended to 64+64 with Rex.W
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
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

isImmediate :: OperandEnc -> Bool
isImmediate = \case
   Imm    -> True
   Imm8h  -> True
   Imm8l  -> True
   _      -> False

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

encOpcode :: Encoding -> Word8
encOpcode (LegacyEncoding e) = legacyOpcode e
encOpcode (VexEncoding    e) = vexOpcode e

encOpcodeExt :: Encoding -> Maybe Word8
encOpcodeExt (LegacyEncoding e) = legacyOpcodeExt e
encOpcodeExt (VexEncoding    e) = vexOpcodeExt e

encOpcodeMap :: Encoding -> OpcodeMap
encOpcodeMap (LegacyEncoding e) = MapLegacy (legacyOpcodeMap e)
encOpcodeMap (VexEncoding    e) = vexOpcodeMap e

encOperands :: Encoding -> [OperandSpec]
encOperands (LegacyEncoding e)  = legacyParams e
encOperands (VexEncoding    e)  = vexParams e

encMandatoryPrefix :: Encoding -> Maybe Word8
encMandatoryPrefix (LegacyEncoding e) = legacyMandatoryPrefix e
encMandatoryPrefix (VexEncoding    e) = vexMandatoryPrefix e

encProperties :: Encoding -> [EncodingProperties]
encProperties (LegacyEncoding e) = legacyProperties e
encProperties (VexEncoding    _) = []

encSizableBit :: Encoding -> Maybe Int
encSizableBit (LegacyEncoding e) = sizable (legacyOpcodeFields e)
encSizableBit _                  = Nothing

encSignExtendImmBit :: Encoding -> Maybe Int
encSignExtendImmBit (LegacyEncoding e) = signExtendableImm8 (legacyOpcodeFields e)
encSignExtendImmBit _                  = Nothing

encReversableBit :: Encoding -> Maybe Int
encReversableBit (LegacyEncoding e) = reversable (legacyOpcodeFields e)
encReversableBit _                  = Nothing

-- | Indicate if LOCK prefix is allowed
encLockable :: Encoding -> Bool
encLockable e = Lockable `elem` encProperties e

data LegEnc = LegEnc
   { legacyMandatoryPrefix :: Maybe Word8          -- ^ Mandatory prefix
   , legacyOpcodeMap       :: LegacyMap            -- ^ Map
   , legacyOpcode          :: Word8                -- ^ Opcode
   , legacyOpcodeExt       :: Maybe Word8          -- ^ Opcode extension in ModRM.reg
   , legacyOpcodeFields    :: LegacyOpcodeFields   -- ^ Fields in the opcode
   , legacyProperties      :: [EncodingProperties] -- ^ Encoding properties
   , legacyParams          :: [OperandSpec]        -- ^ Operand encoding
   }
   deriving (Show)

data VexEnc = VexEnc
   { vexMandatoryPrefix :: Maybe Word8          -- ^ Mandatory prefix
   , vexOpcodeMap       :: OpcodeMap            -- ^ Map
   , vexOpcode          :: Word8                -- ^ Opcode
   , vexOpcodeExt       :: Maybe Word8          -- ^ Opcode extension in ModRM.reg
   , vexLW              :: VexLW
   , vexProperties      :: [EncodingProperties] -- ^ Encoding properties
   , vexParams          :: [OperandSpec]        -- ^ Operand encoding
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

encRequireModRM :: Encoding -> Bool
encRequireModRM e = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg 
      hasOpExt = isJust (encOpcodeExt e)

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

{-
 Note [Opcode maps]
 ~~~~~~~~~~~~~~~~~~~~~

 Legacy opcodes are up to 3 bytes long. They have the following forms:
    - 0x0F 0x38 <op>
    - 0x0F 0x3A <op> 
    - 0x0F 0x0F (3DNow! 1-byte opcode after (ModRM, [SIB], [Displacement]))
    - 0x0F <op>
    - <op>

 The bytes before <op> indicate the opcode map to use.

 To fully identify the instruction, some additional bits of the ModRM byte may
 be required: opcode extension in ModRM.reg or invalid parameters (i.e. invalid
 ModRM.mod).
-}

data VexLW
   = W0     -- ^ Vex.W set to 0
   | W1     -- ^ Vex.W set to 1
   | WIG    -- ^ Vex.W ignored
   | L0     -- ^ Vex.L set to 0
   | L1     -- ^ Vex.L set to 1
   | LIG    -- ^ Vex.L ignored
   | LWIG   -- ^ Ignore Vex.W and Vex.L
   deriving (Show)

data Variant
   = Locked        -- ^ Locked memory access
   | Reversed      -- ^ Parameters are reversed (useful when some instructions have two valid encodings, e.g. CMP reg8, reg8)
   | ExplicitParam -- ^ A variant exists with an implicit parameter, but the explicit variant is used
   deriving (Show,Eq)
