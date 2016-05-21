module ViperVM.Arch.X86_64.Assembler.Opcode
   ( LegacyPrefixes (..)
   , LegacyPrefix (..)
   , Opcode (..)
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

data Opcode
   = OpLegacy LegacyPrefixes (Maybe Rex) LegacyMap !Word8
   | OpVex    Vex  !Word8
   | OpXop    Vex  !Word8
   deriving (Show,Eq)

newtype LegacyPrefixes = LegacyPrefixes [LegacyPrefix] deriving (Show,Eq)

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

vexW :: Vex -> Maybe Bool
vexW (Vex2 _) = Nothing
vexW (Vex3 _ x) = Just (testBit x 7)

vexR :: Vex -> Bool
vexR (Vex2 x) = not $ testBit x 7
vexR (Vex3 x _) = not $ testBit x 7

vexX :: Vex -> Maybe Bool
vexX (Vex2 _) = Nothing
vexX (Vex3 x _) = Just (not $ testBit x 6)

vexB :: Vex -> Maybe Bool
vexB (Vex2 _) = Nothing
vexB (Vex3 x _) = Just (not $ testBit x 5)

vexL :: Vex -> Bool
vexL (Vex2 x) = testBit x 2
vexL (Vex3 _ x) = testBit x 2

vexVVVV :: Vex -> Word8
vexVVVV (Vex2 x) = (x `shiftR` 3) .&. 0x0F
vexVVVV (Vex3 _ x) = (x `shiftR` 3) .&. 0x0F

vexPP :: Vex -> Word8
vexPP (Vex2 x) = x .&. 0x03
vexPP (Vex3 _ x) = x .&. 0x03

vexMMMMM :: Vex -> Maybe Word8
vexMMMMM (Vex2 _) = Nothing
vexMMMMM (Vex3 x _) = Just $ x .&. 0x1F

vexMapSelect :: Vex -> OpcodeMap
vexMapSelect v = case (v, vexMMMMM v) of
   (Vex2 _, _)                   -> MapVex 1
   (_, Just n) | n > 0 && n <= 3 -> MapVex n
   _           -> error "Reserved map select in VEX/XOP prefix"
