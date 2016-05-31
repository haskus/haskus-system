{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.X86_64.Assembler.Opcode
   ( LegacyPrefix (..)
   , toLegacyPrefix
   , Opcode (..)
   , opcodeByte
   , opcodeB
   , opcodeR
   , opcodeX
   , opcodeW
   , OpcodeMap (..)
   , LegacyMap (..)
   -- * REX prefix
   , Rex (..)
   , rexW
   , rexR
   , rexX
   , rexB
   , isRexPrefix
   -- * VEX prefix
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
   )
where

import Data.Word
import Data.Bits

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

data Opcode
   = OpLegacy [LegacyPrefix] (Maybe Rex) LegacyMap !Word8 --TODO: remove legacy prefixes?
   | OpVex    Vex  !Word8
   | OpXop    Vex  !Word8
   deriving (Show,Eq)

-- | Opcode byte
opcodeByte :: Opcode -> Word8
opcodeByte (OpLegacy _ _ _ x) = x
opcodeByte (OpVex _ x) = x
opcodeByte (OpXop _ x) = x

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

data OpcodeMap
   = MapLegacy LegacyMap
   | MapVex    !Word8
   | MapXop    !Word8
   deriving (Show,Eq,Ord)

data LegacyMap
   = MapPrimary
   | Map0F
   | Map0F38
   | Map0F3A
   | Map3DNow
   | MapX87
   deriving (Show,Eq,Ord)

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
vexVVVV (Vex2 x)   = (x `shiftR` 3) .&. 0x0F
vexVVVV (Vex3 _ x) = (x `shiftR` 3) .&. 0x0F

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
