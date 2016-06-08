{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Instruction encoding
module ViperVM.Arch.X86_64.ISA.Encoding
   ( Encoding (..)
   , Properties(..)
   , FlagOp(..)
   , VexLW (..)
   , EncodingProperties(..)
   , OperandSpec(..)
   , OperandEnc(..)
   , OpcodeMap(..)
   , LegacyMap(..)
   , AccessMode(..)
   , EncodingVariant(..)
   , HLEAction (..)
   , ValidMode (..)
   , encValidModRMMode
   , encMayHaveMemoryOperand
   , encHasHLE
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
   , encRepeatable
   , encBranchHintable
   , encRequireModRM
   , genEncodingOpcodeVariants
   )
where

import Data.Word
import Data.Maybe
import Data.Bits

import ViperVM.Format.Binary.BitSet (CBitSet(..))
import ViperVM.Arch.X86_64.ISA.MicroArch
import ViperVM.Arch.X86_64.ISA.Operand
import ViperVM.Arch.X86_64.ISA.Opcode
import ViperVM.Arch.X86_64.ISA.Mode
import ViperVM.Arch.X86_64.ISA.Registers

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

-- | Flag state modification
data FlagOp a
   = St        [a]  -- ^ Set flag to 1
   | Unset     [a]  -- ^ Set flag to 0
   | Modified  [a]  -- ^ Set flag depending on the result
   | Undefined [a]  -- ^ Flag is undefined after the operation
   | Read      [a]  -- ^ Flag read by the instruction
   deriving (Show,Eq)

-- | VEX.(L/W) spec
data VexLW
   = W0     -- ^ Vex.W = 0
   | W1     -- ^ Vex.W = 1
   | WIG    -- ^ Vex.W ignored
   | L0_WIG -- ^ Vex.L = 0, ignore Vex.W
   | L1_WIG -- ^ Vex.L = 1, ignore Vex.W
   | L0_W0  -- ^ Vex.L = 0, Vex.W = 0 
   | L0_W1  -- ^ Vex.L = 0, Vex.W = 1
   | L1_W0  -- ^ Vex.L = 1, Vex.W = 0 
   | L1_W1  -- ^ Vex.L = 1, Vex.W = 1
   | LIG_W0 -- ^ Ignore Vex.L, Vex.W = 0 
   | LIG_W1 -- ^ Ignore Vex.L, Vex.W = 1
   | L0     -- ^ Vex.L = 0
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
   | BranchHintable           -- ^ Support branch-hint prefixes
   | ImplicitLock             -- ^ Implicitly locked (lock prefix still supported)
   | Repeatable               -- ^ Allow repeat prefix
   | Commutable               -- ^ Operands can be commuted
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | NoOperandSize64          -- ^ 64-bit operand size not supported

   | DefaultAddressSize64     -- ^ Default address size is 64-bits for this
                              --   instruction in LongMode
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | RequireRexW              -- ^ Require REX.W = 1
   | DefaultSegment Register  -- ^ Default register
   | HLE HLEAction            -- ^ Hardware-lock elision (HLE)
   deriving (Show,Eq)

data HLEAction
   = XAcquire
   | XRelease
   | XBoth
   deriving (Show,Eq)

-- | Instruction variant encoding
data EncodingVariant
   = Locked                     -- ^ Locked memory access
   | Reversed                   -- ^ Parameters are reversed (useful when some instructions have two valid encodings, e.g. CMP reg8, reg8)
   | ExplicitParam              -- ^ A variant exists with an implicit parameter, but the explicit variant is used
   | RepeatZero                 -- ^ REP(Z) prefix
   | RepeatNonZero              -- ^ REPNZ prefix
   | LockEllisionAcquire        -- ^ XACQUIRE prefix
   | LockEllisionRelease        -- ^ XRELEASE prefix
   | BranchHintTaken            -- ^ Branch hint (branch taken)
   | BranchHintNotTaken         -- ^ Branch hint (not taken)
   | SuperfluousSegmentOverride -- ^ Segment override equal to default segment
   deriving (Show,Eq,Enum,CBitSet)

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
encLockable e = Lockable     `elem` encProperties e
             || ImplicitLock `elem` encProperties e

-- | Indicate if branch hint prefixes are allowed
encBranchHintable :: Encoding -> Bool
encBranchHintable e = BranchHintable `elem` encProperties e

-- | Indicate if REPEAT prefix is allowed
encRepeatable :: Encoding -> Bool
encRepeatable e = Repeatable `elem` encProperties e

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

data ValidMode
   = ModeOnlyReg
   | ModeOnlyMem
   | ModeBoth
   | ModeNone
   deriving (Show,Eq)

-- | ModRM.mod only supports the given value
encValidModRMMode :: Encoding -> ValidMode
encValidModRMMode e = case ots of
      []  -> ModeNone
      [x] -> toM x
      _   -> error ("encValidModRMMode: more than one ModRM.rm param: " ++ show ots)
   where
      toM = \case
         T_Mem _     -> ModeOnlyMem
         T_SubReg {} -> ModeOnlyReg
         T_Reg _     -> ModeOnlyReg
         TME _ _     -> ModeBoth
         TLE x y     -> if toM x == toM y
                           then toM x
                           else ModeBoth
         x           -> error ("encValidModRMMode: invalid param type: " ++ show x)
      ots = opType <$> filter ((== RM) . opEnc) (encOperands e)

-- | Indicate if a memory operand may be encoded
encMayHaveMemoryOperand :: Encoding -> Bool
encMayHaveMemoryOperand e = case encValidModRMMode e of
   ModeNone    -> False
   ModeOnlyReg -> False
   ModeOnlyMem -> True
   ModeBoth    -> True

-- | Test if an encoding support the given Hardware-Lock Ellision prefix
encHasHLE :: HLEAction -> Encoding -> Bool
encHasHLE a e = case filter isHLE (encProperties e) of
      []       -> False
      [HLE a'] -> a' == XBoth || a == a'
      xs       -> error ("Invalid HLE actions: "++show xs)
   where
      isHLE (HLE _) = True
      isHLE _       = False



-- | Some instructions store flags and values into the opcode byte. This method
-- returns the list of potential opcodes for an encoding
genEncodingOpcodeVariants :: Encoding -> [Word8]
genEncodingOpcodeVariants e = ocs
   where
      -- the original opcode
      oc = encOpcode e

      -- reversed (check: can we have reversed + operand in opcode (or something
      -- else)?)
      (roc,rsoc) = case (encReversableBit e, encNoForce8Bit e) of
               (Just i, Nothing) -> (Just (setBit oc i), Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               _                 -> (Nothing,Nothing)
      -- sizable, sign-extended
      (szoc,seoc) = case (encNoForce8Bit e, encSignExtendImmBit e) of
               (Nothing,Nothing) -> (Nothing,Nothing)
               (Just i, Nothing) -> (Just (setBit oc i),Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               (Nothing, Just i) ->  (Nothing,Just (setBit oc i))
      -- FPU dest
      fdoc = setBit oc <$> encFPUDestBit e
      -- FPU pop
      fpoc = setBit oc <$> encFPUPopBit e
      -- FPU sizable
      fsoc = setBit oc <$> encFPUSizableBit e

      -- opcodes with differetnt flags
      ocs' = oc : catMaybes [roc,rsoc,szoc,seoc,fdoc,fpoc,fsoc]

      -- operand stored in the opcode
      ocs = if OpcodeLow3 `elem` fmap opEnc (encParams e)
               then [o + i | o <- ocs', i <- [0..7]]
               else ocs'


