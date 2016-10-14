{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BinaryLiterals #-}

-- | Instruction encoding
module ViperVM.Arch.X86_64.ISA.Encoding
   ( -- * Encoding
     Encoding (..)
   , OpcodeEncoding (..)
   , EncodingProperties(..)
   , HLEAction (..)
   , ValidMode (..)
   , hasImmediate
   , encSupportHLE
   , encValidModRMMode
   , encHasVariableSizedOperand
   , encMayHaveMemoryOperand
   , isLegacyEncoding
   , isVexEncoding
   , encLockable
   , encRepeatable
   , encBranchHintable
   , encRequireModRM
   , encGenerateOpcodes
   -- * Generic opcode
   , Opcode (..)
   , OpcodeMap (..)
   , LegacyMap (..)
   , opcodeByte
   , opcodeMap
   , opcodeB
   , opcodeR
   , opcodeX
   , opcodeW
   , opcodeL
   -- * Legacy prefixes
   , LegacyPrefix (..)
   , toLegacyPrefix
   -- * REX prefix
   , Rex (..)
   , rexW
   , rexR
   , rexX
   , rexB
   , isRexPrefix
   -- * VEX/XOP prefix
   , Vex (..)
   , vexW
   , vexR
   , vexX
   , vexB
   , vexL
   , vexVVVV
   , vexMMMMM
   , vexMapSelect
   , vexPP
   , vexPrefix
   -- * ModRM/SIB
   , ModRM(..)
   , SIB(..)
   , Scale(..)
   , RMMode(..)
   , Mode(..)
   , newModRM
   , rmField
   , regField
   , modField
   , modeField
   , modRMFields
   , rmMode
   , useDisplacement
   , useSIB
   , scaleField
   , indexField
   , baseField
   , rmRegMode
   -- * Operands
   , OperandType(..)
   , OperandEnc(..)
   , OperandSpec (..)
   , AccessMode (..)
   , Operand(..)
   , Addr(..)
   , ImmType (..)
   , RegType (..)
   , SubRegType (..)
   , MemType (..)
   , RelType (..)
   , RegFamilies (..)
   , VSIBType (..)
   , VSIBIndexReg (..)
   , maybeOpTypeReg
   , isImmediate
   )
where

import ViperVM.Utils.Types
import ViperVM.Utils.Maybe
import ViperVM.Utils.List (nub)
import ViperVM.Format.Binary.Bits
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.BitField
import ViperVM.Arch.X86_64.ISA.MicroArch
import ViperVM.Arch.X86_64.ISA.Mode
import ViperVM.Arch.X86_64.ISA.Size
import ViperVM.Arch.X86_64.ISA.Registers

-- | Instruction encoding
data Encoding = Encoding
   { encOpcodeEncoding  :: OpcodeEncoding       -- ^ Opcode encoding
   , encMandatoryPrefix :: Maybe LegacyPrefix   -- ^ Mandatory prefix
   , encOpcodeMap       :: OpcodeMap            -- ^ Map
   , encOpcode          :: Word8                -- ^ Opcode
   , encOpcodeExt       :: Maybe Word8          -- ^ Opcode extension in ModRM.reg
   , encOpcodeFullExt   :: Maybe Word8          -- ^ Opcode extension in full ModRM byte
   , encOpcodeWExt      :: Maybe Bool           -- ^ Opcode extension in REX.W, VEX.W, etc.
   , encOpcodeLExt      :: Maybe Bool           -- ^ Opcode extension in VEX.L, etc.
   , encReversableBit   :: Maybe Int            -- ^ Args are reversed if the given bit is
                                                --   set in the opcode.
   , encNoForce8Bit     :: Maybe Int            -- ^ Operand size is 8 if the given bit is
                                                --   unset in the opcode. Otherwise, the
                                                --   size is defined by operand-size
                                                --   prefix and REX.W bit
   , encSignExtendImmBit:: Maybe Int            -- ^ Used in conjunction with a set
                                                --   Sizable bit.  Imm8 operand is used
                                                --   and sign-extended if the given bit is
                                                --   set
   , encFPUDestBit      :: Maybe Int            -- ^ Opcode bit: register destination (0 if ST0, 1 if ST(i))
                                                --   only if both operands are registers!
   , encFPUPopBit       :: Maybe Int            -- ^ Opcode bit: pop the FPU register,
                                                --   only if destination is (ST(i))
   , encFPUSizableBit   :: Maybe Int            -- ^ Opcode bit: change the FPU size (only if memory operand)
   , encProperties      :: [EncodingProperties] -- ^ Encoding properties
   , encOperands        :: [OperandSpec]        -- ^ Operand encoding
   }
   deriving (Show)

-- | Opcode encoding
data OpcodeEncoding
   = EncLegacy -- ^ Legacy encoding
   | EncVEX    -- ^ VEX encoding
   deriving (Show,Eq,Ord)

-- | Encoding properties
data EncodingProperties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | ImplicitLock             -- ^ Implicitly locked (lock prefix still supported)
   | BranchHintable           -- ^ Support branch-hint prefixes
   | Repeatable               -- ^ Allow repeat prefix
   | Commutable               -- ^ Operands can be commuted
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | NoOperandSize64          -- ^ 64-bit operand size not supported

   | DefaultAddressSize64     -- ^ Default address size is 64-bits for this
                              --   instruction in LongMode
   | Extension X86Extension   -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | DefaultSegment Register  -- ^ Default register
   | HLE HLEAction            -- ^ Hardware-lock elision (HLE) prefix support
   deriving (Show,Eq)

-- | Hardware-lock ellision prefixes
data HLEAction
   = XAcquire
   | XRelease
   | XBoth
   deriving (Show,Eq)

hasImmediate :: Encoding -> Bool
hasImmediate e = any (isImmediate . opEnc) (encOperands e)

isLegacyEncoding :: Encoding -> Bool
isLegacyEncoding = (== EncLegacy) . encOpcodeEncoding

isVexEncoding :: Encoding -> Bool
isVexEncoding = (== EncVEX) . encOpcodeEncoding

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

-- | Indicate if a variable-sized operand is encoded (hence the operand-size
-- prefix can be used)
encHasVariableSizedOperand :: Encoding -> Bool
encHasVariableSizedOperand e = any (vsizeOp . opType) (encOperands e)
   where
      vsizeOp = \case
         TME o1 o2     -> vsizeOp o1 || vsizeOp o2
         TLE o1 o2     -> vsizeOp o1 || vsizeOp o2
         TWE o1 o2     -> vsizeOp o1 || vsizeOp o2
         T_Pair o1 o2  -> vsizeOp o1 || vsizeOp o2
         T_SubReg _ rt -> vsizeOp (T_Reg rt)
         T_Mem mt      -> case mt of
                           MemPair16o32 -> True
                           MemOpSize    -> True
                           MemDSrSI     -> True
                           MemESrDI     -> True
                           MemDSrDI     -> True
                           _            -> False
         T_Reg rt      -> case rt of
                           RegOpSize   -> True
                           RegAccu     -> True
                           RegStackPtr -> True
                           RegBasePtr  -> True
                           RegFam _    -> True
                           _           -> False
                           
         T_Imm it      -> case it of
                           ImmSizeOp -> True
                           ImmSizeSE -> True
                           _         -> False
         T_Rel _       -> False
         T_MemOffset   -> True
         T_MemDSrAX    -> False

-- | Test if an encoding support the given Hardware-Lock Ellision prefix
encSupportHLE :: HLEAction -> Encoding -> Bool
encSupportHLE a e = case filter isHLE (encProperties e) of
      []       -> False
      [HLE a'] -> a' == XBoth || a == a'
      xs       -> error ("Invalid HLE actions: "++show xs)
   where
      isHLE (HLE _) = True
      isHLE _       = False



-- | Some instructions store flags and values into the opcode byte. This method
-- returns the list of potential opcodes for an encoding
encGenerateOpcodes :: Encoding -> [Word8]
encGenerateOpcodes e = nub ocs
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

      -- FPU flags
      fps = [encFPUDestBit e, encFPUSizableBit e, encFPUPopBit e, Nothing]
      mf (Just x , Just y ) = setBit (setBit oc y) x
      mf (Nothing, Just y ) = setBit oc y
      mf (Just x , Nothing) = setBit oc x
      mf (Nothing, Nothing) = oc
      fs = [ mf (x,y) | x <- fps, y <- fps]

      -- opcodes with differetnt flags
      ocs' = oc : (fs ++ catMaybes [roc,rsoc,szoc,seoc])

      -- operand stored in the opcode
      ocs = if OpcodeLow3 `elem` fmap opEnc (encOperands e)
               then [o + i | o <- ocs', i <- [0..7]]
               else ocs'

-------------------------------------------------------------------
-- Generic opcode
-------------------------------------------------------------------

-- | Generic opcode
data Opcode
   = OpLegacy [LegacyPrefix] (Maybe Rex) LegacyMap !Word8 --TODO: remove legacy prefixes?
   | OpVex    Vex  !Word8
   | OpXop    Vex  !Word8
   deriving (Show,Eq)

-- | Opcode map
data OpcodeMap
   = MapLegacy LegacyMap
   | MapVex    !Word8
   | MapXop    !Word8
   deriving (Show,Eq,Ord)

-- | Legacy opcode map
data LegacyMap
   = MapPrimary
   | Map0F
   | Map0F38
   | Map0F3A
   | Map3DNow
   | MapX87
   deriving (Show,Eq,Ord)


-- | Opcode byte
opcodeByte :: Opcode -> Word8
opcodeByte (OpLegacy _ _ _ x) = x
opcodeByte (OpVex _ x)        = x
opcodeByte (OpXop _ x)        = x

-- | Get the opcode map
opcodeMap :: Opcode -> OpcodeMap
opcodeMap = \case
   OpLegacy _ _ t _ -> MapLegacy t
   OpVex  v    _    -> vexMapSelect v
   OpXop  v    _    -> vexMapSelect v

-- | Base extension
opcodeB :: Opcode -> Word8
opcodeB = \case
   OpVex v _                 -> if vexB v then 1 else 0
   OpXop v _                 -> if vexB v then 1 else 0
   OpLegacy _ (Just rex) _ _ -> rexB rex
   OpLegacy _ Nothing    _ _ -> 0

-- | Reg extension
opcodeR :: Opcode -> Word8
opcodeR = \case
   OpVex v _                 -> if vexR v then 1 else 0
   OpXop v _                 -> if vexR v then 1 else 0
   OpLegacy _ (Just rex) _ _ -> rexR rex
   OpLegacy _ Nothing    _ _ -> 0

-- | Index extension
opcodeX :: Opcode -> Word8
opcodeX = \case
   OpVex v _                 -> if vexX v then 1 else 0
   OpXop v _                 -> if vexX v then 1 else 0
   OpLegacy _ (Just rex) _ _ -> rexX rex
   OpLegacy _ Nothing    _ _ -> 0

-- | W (64-bit operand size)
opcodeW :: Opcode -> Bool
opcodeW = \case
   OpVex v _                 -> vexW v
   OpXop v _                 -> vexW v
   OpLegacy _ (Just rex) _ _ -> rexW rex
   OpLegacy _ Nothing    _ _ -> False

-- | Get vector length (stored in VEX.L, XOP.L, etc.)
opcodeL :: Opcode -> Maybe Bool
opcodeL = \case
   OpVex v _ -> Just $ vexL v
   OpXop v _ -> Just $ vexL v
   _         -> Nothing


-------------------------------------------------------------------
-- Legacy prefixes
-------------------------------------------------------------------


-- | Legacy prefixes
data LegacyPrefix
   = LegacyPrefix66
   | LegacyPrefix67
   | LegacyPrefix2E
   | LegacyPrefix3E
   | LegacyPrefix26
   | LegacyPrefix64
   | LegacyPrefix65
   | LegacyPrefix36
   | LegacyPrefixF0
   | LegacyPrefixF3
   | LegacyPrefixF2
   deriving (Show,Eq)

toLegacyPrefix :: Word8 -> Maybe LegacyPrefix
toLegacyPrefix = \case
   0x66 -> Just LegacyPrefix66
   0x67 -> Just LegacyPrefix67
   0x2E -> Just LegacyPrefix2E
   0x3E -> Just LegacyPrefix3E
   0x26 -> Just LegacyPrefix26
   0x64 -> Just LegacyPrefix64
   0x65 -> Just LegacyPrefix65
   0x36 -> Just LegacyPrefix36
   0xF0 -> Just LegacyPrefixF0
   0xF3 -> Just LegacyPrefixF3
   0xF2 -> Just LegacyPrefixF2
   _    -> Nothing
-------------------------------------------------------------------
-- REX prefix
-------------------------------------------------------------------

-- | Rex prefix
newtype Rex = Rex Word8 deriving (Show,Eq)

-- | Test W bit of REX prefix
rexW :: Rex -> Bool
rexW (Rex v) = testBit v 3

-- | Test R bit of REX prefix
rexR :: Rex -> Word8
rexR (Rex v) = if testBit v 2 then 1 else 0

-- | Test X bit of REX prefix
rexX :: Rex -> Word8
rexX (Rex v) = if testBit v 1 then 1 else 0

-- | Test B bit of REX prefix
rexB :: Rex -> Word8
rexB (Rex v) = if testBit v 0 then 1 else 0

-- | Test for a REX prefix
isRexPrefix :: Word8 -> Bool
isRexPrefix w = w .&. 0xF0 == 0x40


-------------------------------------------------------------------
-- VEX prefix
-------------------------------------------------------------------

-- | A VEX prefix
data Vex
   = Vex2 !Word8           -- ^ Two-byte VEX prefix
   | Vex3 !Word8 !Word8    -- ^ Three-byte VEX prefix
   deriving (Show,Eq)

vexW :: Vex -> Bool
vexW (Vex2 _)   = False
vexW (Vex3 _ x) = testBit x 7

vexR :: Vex -> Bool
vexR (Vex2 x)   = not $ testBit x 7
vexR (Vex3 x _) = not $ testBit x 7

vexX :: Vex -> Bool
vexX (Vex2 _)   = False
vexX (Vex3 x _) = not $ testBit x 6

vexB :: Vex -> Bool
vexB (Vex2 _)   = False
vexB (Vex3 x _) = not $ testBit x 5

vexL :: Vex -> Bool
vexL (Vex2 x)   = testBit x 2
vexL (Vex3 _ x) = testBit x 2

vexVVVV :: Vex -> Word8
vexVVVV (Vex2 x)   = complement (x `shiftR` 3) .&. 0x0F
vexVVVV (Vex3 _ x) = complement (x `shiftR` 3) .&. 0x0F

vexPP :: Vex -> Word8
vexPP (Vex2 x)   = x .&. 0x03
vexPP (Vex3 _ x) = x .&. 0x03

vexPrefix :: Vex -> Maybe LegacyPrefix
vexPrefix v = case vexPP v of
   0x00 -> Nothing
   0x01 -> Just LegacyPrefix66
   0x02 -> Just LegacyPrefixF3
   0x03 -> Just LegacyPrefixF2
   _    -> error "Invalid VEX.pp"

vexMMMMM :: Vex -> Word8
vexMMMMM (Vex2 _)   = 0x01
vexMMMMM (Vex3 x _) = x .&. 0x1F

vexMapSelect :: Vex -> OpcodeMap
vexMapSelect = MapVex . vexMMMMM

-------------------------------------------------------------------
-- ModRM/SIB
-------------------------------------------------------------------

-- | ModRM byte
newtype ModRM = ModRM (BitFields Word8
  '[ BitField 2 "mode" Word8
   , BitField 3 "reg"  Word8
   , BitField 3 "rm"   Word8
   ])
   deriving (Show,Eq)

-- | SIB byte
newtype SIB = SIB Word8 deriving (Show,Eq)

-- | SIB scale factor
data Scale
   = Scale1 
   | Scale2 
   | Scale4 
   | Scale8 
   deriving (Show,Eq)

-- | Mode for the R/M field
data RMMode
   = RMRegister   -- ^ Direct register addressing
   | RMBaseIndex  -- ^ Memory addressing with only base/index register
   | RMSIB        -- ^ Memory addressing with SIB byte
   deriving (Show, Eq)

-- | Create a ModRM byte (check inputs)
newModRM :: Word8 -> Word8 -> Word8 -> ModRM
newModRM md rm reg
   | md  > 3 = error "Invalid value for mod field (> 3)"
   | rm  > 7 = error "Invalid value for rm field (> 7)"
   | reg > 7 = error "Invalid value for reg field (> 7)"
   | otherwise = ModRM
         $ updateField (Proxy :: Proxy "mode") md
         $ updateField (Proxy :: Proxy "reg")  reg
         $ updateField (Proxy :: Proxy "rm")   rm
         $ BitFields 0


-- | Get r/m field in ModRM
rmField :: ModRM -> Word8
rmField (ModRM x) = extractField (Proxy :: Proxy "rm") x

-- | Get reg field in ModRM
regField :: ModRM -> Word8
regField (ModRM x) = extractField (Proxy :: Proxy "reg") x

-- | Get mod field in ModRM
modField :: ModRM -> Word8
modField (ModRM x) = extractField (Proxy :: Proxy "mode") x


-- | Mode for pattern matching
data Mode
   = Mode00
   | Mode01
   | Mode10
   | Mode11
   deriving (Show,Eq,Enum)

-- | Get mod field in ModRM
modeField :: ModRM -> Mode
modeField = toEnum . fromIntegral . modField

-- | Get the tree fields (mod,reg,rm)
modRMFields :: ModRM -> (Word8,Word8,Word8)
modRMFields (ModRM x) = matchFields x

-- | Indicate R/M field mode
rmMode :: AddressSize -> ModRM -> RMMode
rmMode sz rm = case (sz, modField rm, rmField rm) of
   (_,3,_)          -> RMRegister
   (AddrSize16,_,_) -> RMBaseIndex
   (_,_,4)          -> RMSIB
   _                -> RMBaseIndex

-- | Indicate if the r/m field contains a register
rmRegMode :: ModRM -> Bool
rmRegMode rm = modField rm == 3

-- | Indicate if displacement bytes follow
useDisplacement :: AddressSize -> Maybe SIB -> ModRM -> Maybe Size
useDisplacement sz sib modrm = case (sz,modField modrm,rmField modrm) of
   -- 16-bit addressing
   (AddrSize16, 0, 0b110) -> Just Size16
   (AddrSize16, 1,     _) -> Just Size8
   (AddrSize16, 2,     _) -> Just Size16
   (AddrSize16, _,     _) -> Nothing

   -- 64 bit uses 32 bit addressing
   (_, 0, 0b101)          -> Just Size32
   (_, 1,     _)          -> Just Size8
   (_, 2,     _)          -> Just Size32
   (_, 0, 0b100)          -> case sib of
      Nothing -> error "SIB required"
      Just s  -> if baseField s == 0b101
         then Just Size32
         else Nothing

   _                      -> Nothing

-- | Indicate if a SIB byte follows
useSIB :: AddressSize -> ModRM -> Bool
useSIB sz modrm = case (sz,modField modrm,rmField modrm) of
   (AddrSize16, _, _) -> False -- no SIB in 16 bit addressing
   (_, 3, _)          -> False -- direct register addressing
   (_, _, 4)          -> True
   _                  -> False


-- | Get SIB scale field
scaleField :: SIB -> Scale
scaleField (SIB x) = case x `shiftR` 6 of
   0 -> Scale1
   1 -> Scale2
   2 -> Scale4
   3 -> Scale8
   _ -> error "Invalid scaling factor"

-- | Get SIB index field
indexField :: SIB -> Word8
indexField (SIB x) = (x `shiftR` 3) .&. 0x07

-- | Get SIB base field
baseField :: SIB -> Word8
baseField (SIB x) = x .&. 0x07

-------------------------------------------------------------------
-- Operands
-------------------------------------------------------------------

-- | An operand
data Operand
   = OpImmediate SizedValue            -- ^ Immediate value
   | OpReg Register                    -- ^ Register
   | OpRegPair Register Register       -- ^ REG:REG
   | OpMem MemType Addr                -- ^ Memory address
   | OpCodeAddr Addr                   -- ^ Code address
   | OpPtr16_16 !Word16 !Word16        -- ^ Immediate 16:16 ptr
   | OpPtr16_32 !Word16 !Word32        -- ^ Immediate 16:32 ptr
   | OpStackFrame !Word16 !Word8       -- ^ Stack frame (cf ENTER)
   deriving (Show,Eq)

-- The X86 architecture supports different kinds of memory addressing. The
-- available addressing modes depend on the execution mode.
-- The most complicated addressing has:
--    - a base register
--    - an index register with a scaling factor (1, 2, 4 or 8)
--    - an offset (displacement)
--
-- Base and index registers can be extended in 64-bit mode to access new registers.
-- Offset size depends on the address size and on the execution mode.

-- | A memory address
data Addr = Addr
   { addrSeg   :: Register             -- ^ Segment register
   , addrBase  :: Maybe Register       -- ^ Base register
   , addrIndex :: Maybe Register       -- ^ Index register
   , addrScale :: Maybe Scale          -- ^ Scale
   , addrDisp  :: Maybe SizedValue     -- ^ Displacement
   }
   deriving (Show,Eq)

-- Note [Operand size]
-- ~~~~~~~~~~~~~~~~~~~
--
-- Default operand size(s)
-- -----------------------
--   * In virtual 8086-mode, real-mode and system management mode: 16-bit
--   * In protected mode or compatibility mode: 16-bit or 32-bit (a flag is set
--   for each segment)
--   * In 64-bit mode: 32-bit. Some instructions have 64-bit default.
-- 
-- 0x66 prefix
-- -----------
-- In protected mode and compatibility mode, the 0x66 prefix can be used to
-- switch to the second default mode.
--
-- Instruction specific operand size
-- ---------------------------------
-- Some instructions have a bit in the operand indicating whether they use the
-- default operand size or a fixed 8-bit operand size.
--
-- W bit
-- -----
-- REX/VEX/XOP prefixes have a W flag that indicates whether the operand size is
-- the default one or 64-bit. The flag is ignored by some instructions.
--
-- Some instructions only use 32- or 64-bit selected with the W bit (e.g. ADOX).
--
-- L bit
-- -----
-- VEX/XOP prefixes have a L flag that indicates the size of the vector register
-- (XMM or YMM). It can be ignored of fixed at a specified value.
--
-- Immediate operands
-- ------------------
-- Immediate operands can be of the operand size (e.g. MOV)
--
-- More commonly, they are of the operand size *except in 64-bit*:
--    Operand size   | 8 | 16 | 32 | 64
--    Immediate size | 8 | 16 | 32 | 32 (sign-extended)
--
-- Or the immediate size can be fixed to 8-bit and it is sign-extended.
--
-- Or the immediate size can be arbitrarily fixed.
--
-- Per-operand size
-- ----------------
--
-- Some instructions (e.g. CRC32) have one operand that follows REX.W (i.e.
-- 32-bit or 64-bit) while the other one follows the default size (or sizable
-- bit in the opcode).

-- Note [Operands]
-- ~~~~~~~~~~~~~~~
--
-- The ModRM.RM field allows the encoding of either a memory address or a
-- register.
--
-- Only a subset of a register may be used (e.g. the low-order 64-bits of a XMM
-- register).

-- | Immediate type
data ImmType
   = ImmSize8    -- ^ 8-bit immediate
   | ImmSize16   -- ^ 16-bit immediate
   | ImmSizeOp   -- ^ operand-size immediate
   | ImmSizeSE   -- ^ sign-extendable immediate:
                 --     * if sign-extendable bit is set: sign-extended 8-bit immediate
                 --     * if 64-bit operand size: sign-extended 32-bit immediate
                 --     * otherwise: operand-size immediate
   | ImmConst Int -- ^ Constant immediate (used in implicit)
   deriving (Show,Eq)

-- | Memory address type
data MemType
   = MemPair16o32 -- ^ Pair of words in memory (words are operand-size large)
   | Mem8         -- ^ 8-bit memory
   | Mem16        -- ^ 16-bit memory
   | Mem32        -- ^ 32-bit memory
   | Mem64        -- ^ 64-bit memory
   | Mem128       -- ^ 128-bit memory
   | Mem256       -- ^ 256-bit memory
   | Mem512       -- ^ 512-bit memory
   | MemOpSize    -- ^ operand-size-bit memory
   | MemVoid      -- ^ The pointer is used to identify a page, etc. (e.g., CLFLUSH)
   | MemPtr       -- ^ m16:16, m16:32 or m16:64 (16-bit selector + offset)
   | MemDescTable -- ^ Descriptor table: m16&32 (legacy)  or m16&64 (64-bit mode)
   | MemFP        -- ^ m32fp or m64fp (x87)
   | MemFP80      -- ^ m80fp (x87)
   | MemInt       -- ^ m32int or m16int (x87)
   | MemInt64     -- ^ m64int (x87)
   | MemDec80     -- ^ Binary coded decimal (m80dec (x87))
   | MemEnv       -- ^ 14/28 bit FPU environment (x87)
   | MemFPUState  -- ^ 94/108 bit FPU state (x87)
   | MemDSrSI     -- ^ operand-size memory at DS:rSI (rSI depends on address-size, DS if fixed)
   | MemESrDI     -- ^ operand-size memory at ES:rDI (rDI depends on address-size, ES is fixed)
   | MemDSrDI     -- ^ operand-size memory at DS:rDI (rDI depends on address-size, DS is overridable with prefixes)
   | MemVSIB32 VSIBType -- ^ VSIB: 32-bit memory referred to by the VSIB
   | MemVSIB64 VSIBType -- ^ VSIB: 64-bit memory referred to by the VSIB
   | MemState     -- ^ Processor extended states (cf XSAVE/XRSTOR)
   deriving (Show,Eq)

-- | How to use the index register
-- e.g., VSIBType 32 128 --> 32-bit indices in a 128-bits register (XMM)
data VSIBType = VSIBType Size VSIBIndexReg
   deriving (Show,Eq)

-- | Register size for VSIB
data VSIBIndexReg
   = VSIB128
   | VSIB256
   deriving (Show,Eq)

-- | Register type
data RegType
   = RegVec64           -- ^  64-bit vector register (mmx)
   | RegVec128          -- ^ 128-bit vector register (xmm)
   | RegVec256          -- ^ 256-bit vector register (ymm)
   | RegFixed Register  -- ^ Fixed register
   | RegSegment         -- ^ Segment register
   | RegControl         -- ^ Control register
   | RegDebug           -- ^ Debug register
   | Reg8               -- ^ General purpose 8-bit register
   | Reg16              -- ^ General purpose 16-bit register
   | Reg32              -- ^ General purpose 32-bit register
   | Reg64              -- ^ General purpose 64-bit register
   | Reg32o64           -- ^ General purpose 32-bit register in legacy mode,
                        -- general purpose 64-bit register in 64-bit mode
   | RegOpSize          -- ^ General purpose register: 8, 16, 32 or 64-bit
   | RegST              -- ^ x87 register
   | RegCounter         -- ^ CX, ECX or RCX depending on the address-size
   | RegAccu            -- ^ AL, AX, EAX, RAX depending on the operand-size
   | RegStackPtr        -- ^ SP, ESP, RSP (default in 64-bit mode)
   | RegBasePtr         -- ^ BP, EBP, RBP (default in 64-bit mode)
   | RegFam RegFamilies -- ^ Register family
   deriving (Show,Eq)

-- | Register family
data RegFamilies
   = RegFamAX          -- ^ AX, EAX, RAX (depending on operand-size)
   | RegFamBX          -- ^ BX, EBX, RBX (depending on operand-size)
   | RegFamCX          -- ^ CX, ECX, RCX (depending on operand-size)
   | RegFamDX          -- ^ DX, EDX, RDX (depending on operand-size)
   | RegFamSI          -- ^ SI, ESI, RSI (depending on operand-size)
   | RegFamDI          -- ^ DI, EDI, RDI (depending on operand-size)
   | RegFamDXAX        -- ^ AX, DX:AX, EDX:EAX, RDX:RAX
   deriving (Show,Eq)

-- | Sub register type
data SubRegType
   = SubLow8      -- ^ Low  8-bit of a register
   | SubLow16     -- ^ Low 16-bit of a register
   | SubLow32     -- ^ Low 32-bit of a register
   | SubLow64     -- ^ Low 64-bit of a register
   | SubHigh64    -- ^ High 64-bit of a register
   | SubEven64    -- ^ [63:0] and [191:128], etc.
   deriving (Show,Eq)

-- | Relative type
data RelType
   = Rel8         -- ^ Relative 8-bit displacement
   | Rel16o32     -- ^ Relative 16- or 32-bit displacement (16-bit invalid in 64-bit mode)
   deriving (Show,Eq)

-- | Operand types
data OperandType
   = TME OperandType OperandType    -- ^ One of the two types (for ModRM.rm)
   | TLE OperandType OperandType    -- ^ One of the two types depending on Vex.L
   | TWE OperandType OperandType    -- ^ One of the two types depending on Rex.W
   | T_Mem MemType                  -- ^ Memory address
   | T_Reg RegType                  -- ^ Register
   | T_SubReg SubRegType RegType    -- ^ Sub-part of a register
   | T_Pair OperandType OperandType -- ^ Pair (AAA:BBB)
   | T_Imm ImmType                  -- ^ Immediate value
   | T_Rel RelType                  -- ^ Memory offset relative to current IP
   | T_MemOffset                    -- ^ Memory offset relative to the segment base: the offset is address-sized, the value is operand-sized
   | T_MemDSrAX                     -- ^ Memory whose address is DS:EAX or DS:RAX (64-bit mode)
   deriving (Show,Eq)

-- | Operand encoding
data OperandEnc
   = RM         -- ^ Operand stored in ModRM.rm
   | Reg        -- ^ Operand stored in ModRM.reg
   | Imm        -- ^ Operand stored in immediate bytes
   | Imm8h      -- ^ Operand stored in bits [7:4] of the immediate byte
   | Imm8l      -- ^ Operand stored in bits [3:0] of the immediate byte
   | Implicit   -- ^ Implicit
   | Vvvv       -- ^ Operand stored in Vex.vvvv field
   | OpcodeLow3 -- ^ Operand stored in opcode 3 last bits
   deriving (Show,Eq)

-- | Operand specification
data OperandSpec = OperandSpec
   { opMode :: AccessMode
   , opType :: OperandType
   , opEnc  :: OperandEnc
   } deriving (Show)

-- | Operand access mode
data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
   | NA         -- ^ Meta use of the operand
   deriving (Show,Eq)

-- | Indicate if the operand type can be register when stored in ModRM.rm
-- (i.e. ModRM.mod may be 11b)
maybeOpTypeReg :: OperandType -> Bool
maybeOpTypeReg = \case
   TME x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   TLE x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   TWE x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   T_Pair x y      -> maybeOpTypeReg x || maybeOpTypeReg y
   T_Rel _         -> False
   T_Mem _         -> False
   T_Reg _         -> True
   T_SubReg _ _    -> True
   T_Imm _         -> False
   T_MemOffset     -> False
   T_MemDSrAX      -> False

isImmediate :: OperandEnc -> Bool
isImmediate = \case
   Imm    -> True
   Imm8h  -> True
   Imm8l  -> True
   _      -> False

