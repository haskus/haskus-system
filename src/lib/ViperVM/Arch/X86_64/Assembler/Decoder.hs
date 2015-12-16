{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ViperVM.Arch.X86_64.Assembler.Decoder
   ( Instruction(..)
   , decode
   , decodeMany
   ) where

import Data.Bits
import Data.Word
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Vector as V

import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.VexPrefix
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
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
   | InsnX86 X86Insn Encoding (Maybe OperandSize) [Variant] [Op]
   deriving (Show)


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


   case opcodeMap of
      -- X87 instructions
      MapPrimary | SetX87 `elem` allowedSets 
                   && opcode .&. 0xF8 == 0xD8  -> fmap InsnX87 $ decodeX87 opcode

      -- 3DNow! instructions
      Map3DNow 
         | Set3DNow `elem` allowedSets -> do
            -- we use a dummy operand size and encoding for 3DNow! instructions
            -- because they all have the same and don't use variable sized
            -- operands
            modrm <- Just . ModRM <$> nextWord8
            ops <- decodeOperands OpSize8 amd3DNowEncoding modrm 0
            -- read 3DNow! opcode
            opcode' <- nextWord8
            let [(enc,insn)] = opcodeMap3DNow V.! fromIntegral opcode'
            return $ InsnX86 insn enc Nothing [] ops

         | otherwise -> left (ErrInvalidOpcodeMap opcodeMap)

      _ -> do
         -- lookup insn in opcode map
         (enc,insn,modrm) <- findInsn opcodeMap opcode

         -- Compute operand size for variable sized operands
         opSize <- case encSizableBit enc of
            Just b | not (testBit opcode b) -> return OpSize8
            _                               -> getOperandSize enc

         -- decode operands
         ops <- decodeOperands opSize enc modrm opcode

         -- variants
         let 
            ps'      = fmap toLegacyPrefix ps

            -- Check that a RW memory operand exists for "lock" prefix
            isRWmemOp (RW,OpMem _) = True
            isRWmemOp _            = False

            hasRWmemOp = any isRWmemOp (fmap opMode (encOperands enc) `zip` ops)

            -- check if insn is lockable and has a lock prefix
            vlocked  = if encLockable enc && PrefixLock `elem` ps' && hasRWmemOp
                        then [Locked]
                        else []

            -- check if insn is reversable, if the reversable bit is set
            -- and if there are only registers operands (because it is the only
            -- case for which there are two different encodings for the same
            -- instruction:
            --    ModRM.reg = r1, ModRM.rm = r2, reversed = False
            --    ModRM.reg = r2, ModRM.rm = r1, reversed = True
            isRegOp (OpReg _) = True
            isRegOp _         = False
            onlyRegOps        = all isRegOp ops
            vreverse = case encReversableBit enc of
               Just b | testBit opcode b && onlyRegOps -> [Reversed]
               _                                       -> []

            -- TODO segment override
            -- TODO repeat prefixes
            -- TODO branch hints
            -- TODO useless prefixes? (superfluous/multiple REX, etc.)
            variants = vlocked ++ vreverse

         return $ InsnX86 insn enc (Just opSize) variants ops


-- | Try to find the instruction
findInsn :: OpcodeMap -> Word8 -> X86Dec (Encoding,X86Insn,Maybe ModRM)
findInsn opcodeMap opcode = do
   -- opcode map
   omap <- case opcodeMap of
      MapPrimary  -> right $ opcodeMapPrimary
      Map0F       -> right $ opcodeMap0F
      Map0F38     -> right $ opcodeMap0F38
      Map0F3A     -> right $ opcodeMap0F3A
      Map0F01     -> right $ opcodeMap0F01
      MapVex 1    -> right $ opcodeMapVex1
      MapVex 2    -> right $ opcodeMapVex2
      MapVex 3    -> right $ opcodeMapVex3
      _           -> left  $ ErrInvalidOpcodeMap opcodeMap

   -- candidate instructions
   insns <- case omap V.! fromIntegral opcode of
      [] -> left (ErrUnknownOpcode opcodeMap opcode)
      xs -> right xs

   prefixes <- getLegacyPrefixes
   let
      -- check that mandatory prefix is here
      checkMandatoryprefix (enc,_) = case encMandatoryPrefix enc of
         Nothing -> True
         Just p  -> p `elem` prefixes

      -- filter invalid instructions given the enabled instruction sets and
      -- execution modes
      -- TODO TODO !!!!
      checkMode _ = True
      checkISet _ = True

      -- check that remaining prefixes are supported or ignored!
      -- TODO

      checkAll x = and
         [ checkMandatoryprefix x
         , checkMode x
         , checkISet x
         ]

      insns' = filter checkAll insns

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
                  Just b | regField m /= b -> False
                  _                        -> True

               -- some instructions are distinguished by the parameter types (i.e.
               -- ModRM.mod). We check it here
               checkOperands e = modField m /= 3 || case rmOp of
                     [b] -> maybeOpTypeReg (opType b)
                     _   -> False
                  where
                     rmOp = filter (\b -> opEnc b == E_ModRM) (encOperands e)

   -- finally find out the instruction
   case insns'' of
      [(enc,insn)] -> right (enc,insn,modrm)
      []           -> left (ErrUnknownOpcode opcodeMap opcode)
      _            -> error $ "More than one candidate instructions remaining. Fix the decoder: " ++ show (fmap snd insns'')


-- | Decode operands
decodeOperands :: OperandSize -> Encoding -> Maybe ModRM -> Word8 -> X86Dec [Op]
decodeOperands opSize enc modrm opcode = do

   -- First we read the address (SIB, displacement) encoded in ModRM.rm
   -- if applicable
   rmAddr <- case modrm of
      Just m | not (rmRegMode m) -> Just <$> getAddr m
      _                          -> return Nothing

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
            (E_Imm, T_Imm) -> case (opSize, encSignExtendImmBit enc) of
               (OpSize8, _)
                  -> Just . OpImmediate . SizedValue8 <$> nextWord8
               (_, Just se) | testBit opcode se
                  -> Just . OpSignExtendImmediate . SizedValue8 <$> nextWord8
               (OpSize16,_)
                  -> Just . OpImmediate . SizedValue16 <$> nextWord16
               (OpSize32,_)
                  -> Just . OpImmediate . SizedValue32 <$> nextWord32
               (OpSize64,_)
                  -> Just . OpSignExtendImmediate . SizedValue32 <$> nextWord32

            _  -> error $ "Don't know how to read immediate operand: " ++ show im
      _    -> error "Invalid encoding (more than one immediate operand)"

   -- associate operands
   ops <- forM (encOperands enc) $ \op -> do
      let doImm = case imm of
            Just (OpRegId rid) -> getOpFromRegId opSize (opType op) rid
            Just o  -> return o
            Nothing -> error "Immediate operand expected, but nothing found"

      case opEnc op of
         E_Imm      -> doImm
         E_Imm8_3_0 -> doImm
         E_Imm8_7_4 -> doImm
         E_ModRM -> case (modrm, rmAddr) of
            (_, Just addr) -> return (OpMem addr)
            (Just m, _)    -> getRMOp opSize (opType op) m
            (Nothing,_)    -> error "ModRM expected, but nothing found"
         E_ModReg -> case modrm of
            Just m  -> getRegOp opSize (opType op) m
            Nothing -> error "ModRM expected, but nothing found"
         E_Implicit -> getImplicitOp opSize (opType op)
         E_VexV     -> getAdditionalOperand >>= \case
            Just vvvv -> getOpFromRegId opSize (opType op) vvvv
            Nothing   -> error "Expecting additional operand (VEX.vvvv)"
         E_OpReg    -> getOpFromRegId opSize (opType op) (opcode .&. 0x07)

   -- reverse operands if reversable bit is set
   let ops' = case encReversableBit enc of
         Just b | testBit opcode b -> reverse ops
         _                         -> ops

   return ops'
