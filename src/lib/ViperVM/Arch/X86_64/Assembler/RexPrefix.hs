module ViperVM.Arch.X86_64.Assembler.RexPrefix
   ( Rex(..)
   , rexW
   , rexR
   , rexX
   , rexB
   , putRexPrefix
   ) where

import Data.Word
import Data.Bits
import ViperVM.Format.Binary.Put

newtype Rex = Rex Word8 deriving (Show)

-- | Write a REX prefix
putRexPrefix :: Rex -> Put
putRexPrefix (Rex x) = putWord8 (x .|. 0x40)

-- | Test W bit of REX prefix
rexW :: Rex -> Bool
rexW (Rex v) = testBit v 4

-- | Test R bit of REX prefix
rexR :: Rex -> Bool
rexR (Rex v) = testBit v 3

-- | Test X bit of REX prefix
rexX :: Rex -> Word8
rexX (Rex v) = if testBit v 2 then 1 else 0

-- | Test B bit of REX prefix
rexB :: Rex -> Word8
rexB (Rex v) = if testBit v 1 then 1 else 0

