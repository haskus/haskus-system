module ViperVM.Arch.X86_64.Assembler.Opcode
   ( LegacyPrefixes (..)
   , Opcode (..)
   , OpcodeMap (..)
   , LegacyMap (..)
   , Rex (..)
   , Vex (..)
   )
where

import Data.Word

data Opcode
   = OpLegacy LegacyPrefixes (Maybe Rex) LegacyMap !Word8
   | OpVex    Vex  !Word8
   | OpXop    Vex  !Word8
   deriving (Show,Eq)

newtype LegacyPrefixes = LegacyPrefixes [Word8] deriving (Show,Eq)

data OpcodeMap
   = MapLegacy LegacyMap
   | MapVex    !Word8
   | MapXop    !Word8
   deriving (Show,Eq)

data LegacyMap
   = MapPrimary
   | Map0F
   | Map0F38
   | Map0F3A
   | Map3DNow
   | MapX87
   deriving (Show,Eq)

newtype Rex = Rex Word8 deriving (Show,Eq)


-- | A VEX prefix
data Vex
   = Vex2 !Word8           -- ^ Two-byte VEX prefix
   | Vex3 !Word8 !Word8    -- ^ Three-byte VEX prefix
   deriving (Show,Eq)

