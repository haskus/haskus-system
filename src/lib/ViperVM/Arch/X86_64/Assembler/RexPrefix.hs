module ViperVM.Arch.X86_64.Assembler.RexPrefix
   ( rexW
   , rexR
   , rexX
   , rexB
   , putRexPrefix
   , isRexPrefix
   ) where

import Data.Word
import Data.Bits

import ViperVM.Format.Binary.Put
import ViperVM.Arch.X86_64.Assembler.Opcode

-- | Write a REX prefix
putRexPrefix :: Rex -> Put
putRexPrefix (Rex x) = putWord8 (x .|. 0x40)

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

-- | Try to decode a REX prefix. See Note [REX prefix]
-- decodeREX :: X86Dec ()
-- decodeREX = do
--    mode <- gets decStateMode
-- 
--    when (is64bitMode mode) $ lookWord8 >>= \x ->
--       when (isRexPrefix x) $ do
--          skipWord8
--          let rex = Rex x
--          modify (\y -> y
--             { decStateBaseRegExt          = rexB rex
--             , decStateIndexRegExt         = rexX rex
--             , decStateRegExt              = rexR rex
--             , decStateUseExtRegs          = True
--             , decStateDefaultAddressSize  = AddrSize64
--             , decStateOpSize64            = rexW rex
--             , decStateHasRexPrefix        = True
--             })
-- 
