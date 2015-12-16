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

import Data.Bits
import Data.Word

import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.VexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.X87
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.OperandSize

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
         , stateDefaultOperandSize  = defOprndSize
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
         (enc,insn) <- case insns'' of
               [x] -> right x
               []  -> left (ErrUnknownOpcode opcodeMap opcode)
               _   -> error "More than one candidate instructions remaining. Fix the decoder"

         -- Decode operands
         -- First we read the address (SIB, displacement) encoded in ModRM.rm
         -- if applicable
         rmAddr <- case modrm of
            Just m | not (rmRegMode m) -> Just <$> getAddr m
            _                          -> return Nothing

         -- Compute operand size for variable sized operands
         opSize <- getOperandSize enc

         -- Read immediate if any
         imm <- case filter (isImmediate . opEnc) (encOperands enc) of
            []   -> return Nothing
            [im] -> do
               case (opEnc im, opType im) of
                  (E_Imm8_3_0, T_Mask)     -> Just . OpMask . SizedValue8 <$> nextWord8
                  (E_Imm8_7_4, T_V128_256) -> Just . OpRegId . (`shiftR` 4) <$> nextWord8
                  (E_Imm, T_Imm8)          -> Just . OpImmediate . SizedValue8 <$> nextWord8
                  (E_Imm, T_PTR_16_16)     -> Just <$> (OpPtr16_16 <$> nextWord16 <*> nextWord16)
                  (E_Imm, T_PTR_16_32)     -> Just <$> (OpPtr16_32 <$> nextWord16 <*> nextWord32)
                  (E_Imm, T_REL_16_32) -> case opSize of
                     OpSize8  -> error "Invalid operand size"
                     OpSize16 -> Just . OpRel . SizedValue16 <$> nextWord16
                     OpSize32 -> Just . OpRel . SizedValue32 <$> nextWord32
                     OpSize64 -> Just . OpRel . SizedValue32 <$> nextWord32
                  (E_Imm, T_Imm) -> case (encSizableBit enc, encSignExtendImmBit enc) of
                     (Just sz, Just se)
                        | testBit opcode sz && testBit opcode se ->
                              Just . OpSignExtendImmediate . SizedValue8 <$> nextWord8
                        | testBit opcode se ->
                              error "Sign-extend bit set but sizable bit unset"
                     (Nothing, Just _) ->
                              error "Invalid encoding: found sign-extend bit without a sizable bit"
                     (Just sz, _)
                        | testBit opcode sz -> case opSize of
                              OpSize16 -> Just . OpImmediate . SizedValue16 <$> nextWord16
                              OpSize32 -> Just . OpImmediate . SizedValue32 <$> nextWord32
                              OpSize64 -> Just . OpSignExtendImmediate . SizedValue32 <$> nextWord32
                              OpSize8  -> error "Invalid operand size for immediate"
                        | otherwise ->
                              Just . OpImmediate . SizedValue8 <$> nextWord8
                     (Nothing,Nothing) ->
                        error "Variable sized immediate operand without sizable bit"
                              

                  _  -> error $ "Don't know how to read immediate operand: " ++ show im
            _    -> error "Invalid encoding (more than one immediate operand)"

         -- associate operands
         --ops <- forM (encOperands enc) $ \op -> do
         --   case opEnc op of
         --      x | isImmediate x -> case imm of
         --         Just (OpReg rid) -> getFromRegId (opType op) rid -- TODO: convert OpRegId into OpReg
         --         Just o  -> return o
         --         Nothing -> error "Immediate operand expected, but nothing found"
         --      E_ModRM -> case modrm of
         --         Just m  -> getRMOp fam sz m
         --         Nothing -> error "ModRM expected, but nothing found"
         --      E_ModReg -> case modrm of
         --         Just m  -> getRegOp fam sz m
         --         Nothing -> error "ModRM expected, but nothing found"
         --      E_Implicit -> getImplicitOp (opType op)
         --      E_VexV     -> getFromRegId (opType op) (vexVVVV vex)
         --      E_OpReg    -> getFromRegId (opType op) (opcode .&. 0x07)

         --TODO
         -- reverse operands if reversable bit is set

         undefined

data InsnInstance =
   InsnInstance X86Insn (Maybe OperandSize) [Op]
   deriving (Show)

