{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ViperVM.Arch.X86_64.Assembler.Decoder
   ( Instruction(..)
   , decode
   , decodeMany
   ) where

import Data.Bits
import qualified Data.Map as Map
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either


import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.VexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.X87

data Instruction
   = InsnX87 X87Instruction
   deriving (Show,Eq)


-- | Decode several instruction (until the end of the stream)
decodeMany :: X86Mode -> [InstructionSet] -> AddressSize -> OperandSize -> Get [Either DecodeError Instruction]
decodeMany mode sets defAddrSize defOprndSize =
   G.isEmpty >>= \case
      True  -> return []
      False -> do
         i <- decode mode sets defAddrSize defOprndSize
         is <- decodeMany mode sets defAddrSize defOprndSize
         return (i:is)

-- | Decode an instruction
decode :: X86Mode -> [InstructionSet] -> AddressSize -> OperandSize -> Get (Either DecodeError Instruction)
decode mode sets defAddrSize defOprndSize = evalStateT (runEitherT decodeInsn) initState
   where
      initState = X86State 
         { stateMode                = mode
         , stateSets                = sets
         , stateAddressSize         = defAddrSize
         , stateOperandSize         = defOprndSize
         , stateByteCount           = 0
         , stateLegacyPrefixes      = []
         , stateBaseRegExt          = 0
         , stateIndexRegExt         = 0
         , stateRegExt              = 0
         , stateOpSize64            = False
         , stateUseExtRegs          = False
         , stateHasRexPrefix        = False
         , stateMapSelect           = []
         , stateHasVexPrefix        = False
         , stateHasXopPrefix        = False
         , stateOpcodeExtE          = Nothing
         , stateAdditionalOp        = Nothing
         , stateVectorLength        = Nothing
         }

         
{-
 Note [Legacy opcodes]
 ~~~~~~~~~~~~~~~~~~~~~

 Legacy opcodes are up to 3 bytes long. They have the following forms:
    - 0x0F 0x38 <op>
    - 0x0F 0x3A <op> 
    - 0x0F 0x0F (3DNow! 1-byte opcode after (ModRM, [SIB], [Displacement]))
    - 0x0F <op>
    - <op>

 Some additional bits of the ModRM byte may be required to fully identify the
 opcode
-}

   





--
---- ------------------------------------------------------------------
----                        EVEX PREFIX
---- ------------------------------------------------------------------
--
---- | Decode an EVEX prefix
--decodePrefixEvex :: X86Dec ()
--decodePrefixEvex = do
--   let
--      smallCheck = (== 0x62) <$> lookaheadByte
--      -- EVEX prefixes are supported in 32-bit mode
--      -- They overload BOUND opcodes so that the first two bits
--      -- of what would be ModRM are invalid (11b) for BOUND
--      fullCheck  = smallCheck <&&> ((== 0xC0) . (.&. 0xC0) . snd <$> lookaheadWord16)
--
--   hasEvex <- (test64BitMode <&&> smallCheck) <||> (test32BitMode <&&> fullCheck)
--
--   when hasEvex $ do
--      consumeByte
--      v <- Evex <$> nextByte <*> nextByte <*> nextByte
--      modify (\y -> y { stateEvexPrefix = Just v })
--
--
---- | Test if an EVEX prefix is present
--hasEvexPrefix :: X86Dec Bool
--hasEvexPrefix = isJust . stateEvexPrefix <$> get
--
--


decodeInsn :: X86Dec Instruction
decodeInsn = do
   allowedSets <- getAllowedSets

   decodeLegacyPrefixes
   decodeREX
   decodeVEX
   decodeXOP

   -- Decode legacy opcode. See Note [Legacy opcodes]
   opcode <- nextWord8 >>= \case
      -- escaped opcode
      0x0F -> do
         assertNoVex ErrVexEscapedOpcode
         nextWord8 >>= \case
            0x0F | Set3DNow `elem` allowedSets -> right [0x0F,0x0F]
            0x38 -> nextWord8 >>= \y -> right [0x0F,0x38,y]
            0x3A -> nextWord8 >>= \y -> right [0x0F,0x3A,y]
            0x01 -> nextWord8 >>= \y -> right [0x0F,0x01,y]
            y    -> right [0x0F,y]
      -- Decode unescaped opcode
      y    -> right [y]


   case opcode of
      -- X87 instructions
      [x] |  SetX87 `elem` allowedSets 
          && x .&. 0xF8 == 0xD8        -> fmap InsnX87 $ decodeX87 x


      -- 3DNow! instructions
      [0x0F,0x0F] | Set3DNow `elem` allowedSets -> do
         -- read ModRM
         -- TODO
         -- read 3DNow! opcode
         opcode' <- nextWord8 >>= \x -> return (opcode ++ [x])
         undefined

      -- normal instructions
      x -> do
         -- try to identify the opcode
         insns <- case Map.lookup opcode insnOpcodeMap of
                     Nothing -> left (ErrUnknownOpcode opcode)
                     Just i  -> right i

         -- filter instructions
         let insns' = filter (\i -> Extension AVX `notElem` iProperties i) insns
         -- TODO
         let insn = undefined

         -- decode ModRM byte.
         modrm <- if requireModRM insn
                     then Just <$> nextWord8
                     else return Nothing

         -- decode SIB byte
         -- TODO

         -- decode disp
         -- TODO

         -- decode immediate
         -- TODO

         undefined
