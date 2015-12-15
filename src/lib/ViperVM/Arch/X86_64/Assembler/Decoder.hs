{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ViperVM.Arch.X86_64.Assembler.Decoder
   ( Instruction(..)
   , decode
   , decodeMany
   ) where

import Data.Bits
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Vector as V


import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.VexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.X87
import ViperVM.Arch.X86_64.Assembler.Addressing

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

   ps <- decodeLegacyPrefixes False False
   modify (\y -> y { stateLegacyPrefixes = ps })
   decodeREX
   decodeVEX
   decodeXOP

   -- Decode legacy opcode. See Note [Legacy opcodes]
   (opcodeMap,opcode) <- nextWord8 >>= \case
      -- escaped opcode
      0x0F -> do
         assertNoVex ErrVexEscapedOpcode
         nextWord8 >>= \case
            0x0F | Set3DNow `elem` allowedSets -> right (Map3DNow, 0x00)
            0x38 -> nextWord8 >>= \y -> right (Map0F38,y)
            0x3A -> nextWord8 >>= \y -> right (Map0F3A,y)
            0x01 -> nextWord8 >>= \y -> right (Map0F01,y)
            y    -> right (Map0F,y)
      -- Decode unescaped opcode
      y    -> right (MapPrimary,y)


   case (opcodeMap,opcode) of
      -- X87 instructions
      (MapPrimary,x) | SetX87 `elem` allowedSets 
                       && x .&. 0xF8 == 0xD8        -> fmap InsnX87 $ decodeX87 x


      -- 3DNow! instructions
      (Map3DNow,_) | Set3DNow `elem` allowedSets -> do
         -- read ModRM
         -- TODO
         -- read 3DNow! opcode
         opcode' <- nextWord8
         undefined

      (MapPrimary,x) -> do
         -- candidate instructions
         insns <- case opcodeMapPrimary V.! fromIntegral x of
            [] -> left (ErrUnknownOpcode opcodeMap opcode)
            xs -> right xs

         -- filter invalid instructions given the enabled instruction sets and
         -- execution modes
         -- TODO
         insns' <- return insns

         modrm <- case (any (requireModRM . fst) insns', all (requireModRM . fst) insns') of
            (True,True)   -> Just . ModRM <$> nextWord8
            (False,False) -> return Nothing
            _             -> error "Some candidates for the same opcode require ModRM, but some others don't. Please fix opcode tables."

         let
            insns'' = case modrm of
               Nothing -> insns'
               Just m  -> filter ((\e -> checkOpcodeExt e && checkOperands e) . fst) insns'
                  where
                     -- check opcode extension in ModRM.reg if applicable
                     checkOpcodeExt e = case encOpcodeExt e of
                        Just x | regField m /= x -> False
                        _                        -> True

                     -- some instructions are distinguished by the parameter types (i.e.
                     -- ModRM.mod). We check it here
                     checkOperands e = modField m /= 3 || case rmOp of
                           [x] -> maybeOpTypeReg (opType x)
                           _   -> False
                        where
                           rmOp = filter (\x -> opEnc x == E_ModRM) (encOperands e)

         -- finally find out the instruction
         let (enc,insn) = case insns'' of
               [x] -> x
               []  -> error "No matching instruction found"
               _   -> error "More than one candidates remaining. Fix the decoder"

         -- Decode operands
         -- First we read the address (SIB, displacement) encoded in ModRM.rm
         -- if applicable
         rmAddr <- case modrm of
            Just m | not (rmRegMode m) -> Just <$> getAddr m
            _                          -> return Nothing

         -- Read immediate if any
         imm <- case hasImmediate enc of
            True  -> undefined -- TODO read appropriate size
            False -> return Nothing

         -- associate operands
         -- (reverse operands if reversable bit is set)
         ops <- forM (encOperands enc) $ \op -> do
            -- TODO
            undefined

         undefined

data InsnInstance =
   InsnInstance X86Insn (Maybe OperandSize) [Op]
   deriving (Show)

legacyParsing :: [X86Insn] -> X86Dec (Maybe InsnInstance)
legacyParsing is = do

   -- Filter instructions that use legacy encoding and that are available in
   -- the current mode
   let
      is' :: [(LegEnc,X86Insn)]
      is' = [(e,i) | i <- is,
                     -- TODO: check mode and arch
                     LegacyEncoding e <- iEncoding i]
   
   -- some instructions have fields in their opcode (Sizable, Sign-extension,
   -- Direction, etc.). Generate the full list of opcodes
   let
      -- set a bit on the last opcode byte
      setLaspOpcodeBit [] _     = error "Empty opcode"
      setLaspOpcodeBit [o] n    = [setBit o n]
      setLaspOpcodeBit (o:os) n = setLaspOpcodeBit os n

   --   case legEncOpcodeFields e of

   --   genOpcodes i (Sizable n)  = 
   -- TODO

   -- create an opcode tree
   -- TODO

   -- parse opcode
   -- TODO

   -- check if ModRM is required (if it is, it should be by all instructions to
   -- avoid ambiguity).
   -- TODO

   -- check opcode extension if required
   -- TODO

   -- some instructions are distinguished by the parameter types (i.e.
   -- ModRM.mod). We check it here
   -- TODO

   -- there should be a single instruction remaining. Decode its parameters
   -- TODO

   undefined
   
