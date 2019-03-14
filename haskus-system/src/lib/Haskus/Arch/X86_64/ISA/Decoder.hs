{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.Arch.X86_64.ISA.Decoder
   ( getInstruction
   )
where

import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Register
import Haskus.Arch.X86_64.ISA.Memory
import Haskus.Arch.Common.Immediate
import Haskus.Arch.Common.Memory
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.X86_64.ISA.OpcodeMaps
import Haskus.Arch.X86_64.ISA.Insns
import Haskus.Arch.X86_64.ISA.Insn
import Haskus.Arch.X86_64.ISA.Encoding
import Haskus.Arch.X86_64.ISA.Operand

import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Get
import Haskus.Format.Binary.BitField
import qualified Haskus.Format.Binary.BitSet as BitSet

import Haskus.Utils.Solver
import Haskus.Utils.List (nub, (\\))
import Haskus.Utils.Maybe
import Haskus.Utils.Flow

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Control.Applicative

-- ===========================================================================
-- X86 Instruction
-- ===========================================================================


getInstruction :: ExecMode -> Get Insn
getInstruction mode = consumeAtMost 15 $ do
   -- An instruction is at most 15 bytes long

   ps  <- readLegacyPrefixes
   rex <- readRexPrefix mode

   -- read opcode
   oc <- readVexXopOpcode mode ps rex >>= \case
      Just op -> return op
      Nothing -> readLegacyOpcode mode ps rex

   case opcodeMap oc of

      -- Handle 3DNow! encoding: the opcode byte is the last byte of the
      -- instruction and the operand encoding is predefined (not opcode
      -- specific)
      MapLegacy Map3DNow -> do
         ops <- readOperands mode ps oc amd3DNowEncoding
         -- read opcode byte
         let OpLegacy ps' rx ocm _ = oc
         oc' <- OpLegacy ps' rx ocm <$> getWord8
         return $ Insn oc'
                ops
                amd3DNowEncoding
                (error "3DNow! instructions not supported") -- TODO
                BitSet.empty

      ocmap              -> do
         -- get candidate instructions for the opcode
         cs <- case Map.lookup ocmap opcodeMaps of
                  Nothing -> fail "No opcode map found"
                  Just t  -> return (t V.! fromIntegral (opcodeByte oc))

         when (null cs) $ fail "No candidate instruction found (empty opcode map cell)"

         -- check prefixes
         let
            arePrefixesValid c = all (encSupportPrefix (entryEncoding c)) ps

            cs2 = filter (\c -> hasMandatoryPrefix c && arePrefixesValid c) cs
            hasMandatoryPrefix i = case (encMandatoryPrefix (entryEncoding i), oc) of
               (mp, OpVex v _)        -> mp == vexPrefix v
               (mp, OpXop v _)        -> mp == vexPrefix v
               (Just mp, OpLegacy {}) -> mp `elem` ps
               (Nothing, _          ) -> True

         when (null cs2) $ fail "No candidate instruction found (invalid mandatory prefixes)"

         -- try to read ModRM
         modrm <- lookAhead $ remaining >>= \case
            x | x >= 1 -> Just <$> getWord8
            _          -> return Nothing

         -- filter out invalid opcode extensions:
         let
            cs3 = filter hasOpcodeExtension cs2
            hasOpcodeExtension i = fullext && regext && wext && lext
               where
                  e = entryEncoding i
                  -- extension in the whole second byte
                  fullext = case (modrm,encOpcodeFullExt e) of
                     (_, Nothing)      -> True
                     (Just m, Just x)  -> m == x
                     (Nothing, Just _) -> False
                  -- extension in ModRM.Reg
                  regext = case (modrm,encOpcodeExt e) of
                     (_, Nothing)      -> True
                     (Just m, Just x)  -> regField (ModRM (BitFields m)) == x
                     (Nothing, Just _) -> False
                  -- extension in REX.W, VEX.W, etc.
                  wext = case encOpcodeWExt e of
                     Nothing -> True
                     Just x  -> opcodeW oc == x
                  -- extension in VEX.L, etc.
                  lext = case encOpcodeLExt e of
                     Nothing -> True
                     Just x  -> opcodeL oc == Just x

         when (null cs3) $ fail ("No candidate instruction found (opcode extension filtering): " ++ show oc)

         -- filter out invalid ModRM.mod (e.g., only 11b)
         let
            cs4 = filter hasValidMod cs3
            hasValidMod i = case (modrm, encValidModRMMode (entryEncoding i)) of
               (Nothing, vm) -> vm == ModeNone
               (Just m,  vm) -> case vm of
                     ModeOnlyReg -> m' == 0b11
                     ModeOnlyMem -> m' /= 0b11
                     _           -> True
                  where m' = m `shiftR` 6

         when (null cs4) $ fail "No candidate instruction found (ModRM.mod filtering)"

         -- Filter out invalid enabled extensions
         -- and invalid execution mode
         let
            cs5 = filter (encSupportExecMode mode . entryEncoding) cs4
               
         when (null cs5) $ do
            -- get disabled extensions that may have filtered out the expected
            -- encoding
            let es = nub (concatMap (encRequiredExtensions . entryEncoding) cs4) \\ extensions mode

            fail ("No candidate instruction found, try enabling one of: "++ show es)

         -- If there are more than one instruction left, signal a bug
         MapEntry spec enc <- case cs5 of
            [x] -> return x
            xs  -> fail ("More than one instruction found (opcode table bug?): " ++ show (fmap (insnMnemonic . entryInsn) xs))

         -- Read params
         ops <- readOperands mode ps oc enc

         -- Variants
         let
            -- lock prefix
            vlocked  = if encLockable enc && LegacyPrefixF0 `elem` ps
                        then BitSet.singleton Locked
                        else BitSet.empty

            -- repeat prefixes
            vrepeat  = if encRepeatable enc
                        then if LegacyPrefixF3 `elem` ps
                           then BitSet.singleton RepeatZero
                           else if LegacyPrefixF2 `elem` ps
                              then BitSet.singleton RepeatNonZero
                              else BitSet.empty
                        else BitSet.empty

            -- branch hint prefixes
            vbranchhint = if encBranchHintable enc
                        then if LegacyPrefix3E `elem` ps
                           then BitSet.singleton BranchHintTaken
                           else if LegacyPrefix2E `elem` ps
                              then BitSet.singleton BranchHintNotTaken
                              else BitSet.empty
                        else BitSet.empty

            -- check if insn is reversable, if the reversable bit is set
            -- and if there are only registers operands (because it is the only
            -- case for which there are two different encodings for the same
            -- instruction:
            --    ModRM.reg = r1, ModRM.rm = r2, reversed = False
            --    ModRM.reg = r2, ModRM.rm = r1, reversed = True
            isRegOp (OpReg _) = True
            isRegOp _         = False
            onlyRegOps        = all isRegOp ops
            reversed = case encReversableBit enc of
               Just b  -> testBit (opcodeByte oc) b 
               Nothing -> False

            vreverse = if reversed && onlyRegOps
               then BitSet.singleton Reversed
               else BitSet.empty

            -- TODO: superfluous segment override
            -- TODO: explicit param variant

            variants = BitSet.unions [ vlocked
                                     , vreverse 
                                     , vrepeat
                                     , vbranchhint
                                     ]

         return $ Insn oc ops enc spec variants

-- ===========================================================================
-- Legacy encoding
-- ===========================================================================

-- | Read legacy prefixes (up to 5)
readLegacyPrefixes :: Get [LegacyPrefix]
readLegacyPrefixes = do
   let
      readLegacyPrefix :: Get (Maybe LegacyPrefix)
      readLegacyPrefix = lookAheadM (toLegacyPrefix <$> getWord8)

      -- | Check that legacy prefixes belong to different groups
      checkLegacyPrefixes :: [LegacyPrefix] -> Bool
      checkLegacyPrefixes ps =
         length ps == length (nub (map legacyPrefixGroup ps))

   -- read at most 5 legacy prefixes
   ws <- getManyAtMost 5 readLegacyPrefix

   -- check that legacy prefixes are valid (group-wise)
   if checkLegacyPrefixes ws
      then return ws
      else fail ("Invalid legacy prefixes: " ++ show ws)
   
---------------------------------------------------------------------------
-- REX prefix
-- ~~~~~~~~~~
--
-- In 64-bit mode, a REX prefix can be used after the legacy prefixes. A REX
-- prefix contains several fields, hence it ranges from 0x40 to 0x4F. In
-- non-64-bit mode, this range is used by the short variants of the INC/DEC
-- instructions, hence these forms are not usable in 64-bit mode.
--
-- There are several things to consider:
--    1) whether a REX prefix is present (whatever it contains)
--    2) the contents of the REX prefix fields.
--
-- Its presence implies:
--    - the use of the uniform byte registers (SIL, DIL, etc. instead of AH,
--    BH, etc.)
--
-- Its fields indicate:
--    - the use of the extended registers (one additional bit per register)
--    - the use of a 64-bit operand size (ignoring the operand-size overriding
--    legacy prefix)
--
-- Some instructions have default or fixed operand size set to 64bits in 64-bit
-- mode, hence they don't require the REX prefix.
--
-- The prefix has the following format:
--
-- |    4    | W | R | X | B |
--                         ^-- base register or ModRM.rm extension
--                     ^------ SIB.index register extension
--                 ^---------- ModRM.reg register extension
--             ^-------------- set to 1 for 64-bit operand size
--      ^--------------------- 4 bits set to 0xD
--
-- 
-- If more than one REX prefix is present, the behavior is undefined (however it
-- seems that the last one is used).
-- 
---------------------------------------------------------------------------

-- | Read optional REX prefix
readRexPrefix :: ExecMode -> Get (Maybe Rex)
readRexPrefix mode =
   
   -- REX is only supported in 64-bit mode
   if is64bitMode (x86Mode mode)
      then lookAheadM $ do
         x <- getWord8
         return $ if isRexPrefix x
            then Just (Rex x)
            else Nothing
      else return Nothing

---------------------------------------------------------------------------
-- Legacy opcodes
-- ~~~~~~~~~~~~~~
--
-- Legacy opcode can belong to one of the following opcode maps:
--    - Primary
--    - 0x0F
--    - 0x0F38
--    - 0x0F3A
--    - 3DNow! (escaped with 0x0F0F, opcode byte in last instruction byte)
--
---------------------------------------------------------------------------

-- | Read legacy opcode
readLegacyOpcode :: ExecMode -> [LegacyPrefix] -> Maybe Rex -> Get Opcode
readLegacyOpcode mode ps rex = do

   let
      is3DNowAllowed = mode `hasExtension` AMD3DNow
      ret m x = return (OpLegacy ps rex m x)

   getWord8 >>= \case
      0x0F -> getWord8 >>= \case
         -- the real 3DNow! opcode is stored in the last byte and
         -- will be set later
         0x0F | is3DNowAllowed -> ret Map3DNow 0
         0x3A                  -> ret Map0F3A =<< getWord8
         0x38                  -> ret Map0F38 =<< getWord8
         w2                    -> ret Map0F w2
      w1   -> ret MapPrimary w1

-- ===========================================================================
-- VEX/XOP encodings
-- ===========================================================================

---------------------------------------------------------------------------
-- VEX/XOP prefixes
-- ~~~~~~~~~~~~~~~~
--
-- VEX/XOP prefixes are different from the REX prefix: they don't extend
-- existing instructions but add new ones (new opcode maps). Moreover they are
-- mutually exclusive with the REX prefix as they subsume it.
--
-- Some legacy prefixes are supported: address-size and segment override.
--
---------------------------------------------------------------------------

-- | Read VEX/XOP encoded opcode
readVexXopOpcode :: ExecMode -> [LegacyPrefix] -> Maybe Rex -> Get (Maybe Opcode)
readVexXopOpcode mode ps rex = do
   let
      isXOPAllowed = mode `hasExtension` XOP
      isVEXAllowed = mode `hasExtension` VEX

      -- VEX prefixes are supported in 32-bit and 16-bit modes
      -- They overload LES and LDS opcodes so that the first two bits
      -- of what would be ModRM are invalid (11b) for LES/LDS
      testMod :: Word8 -> Bool
      testMod w    = w `uncheckedShiftR` 6 == 0x03

      isVexMode act = do
         c <- if is64bitMode (x86Mode mode)
                  then return True
                  else testMod <$> lookAhead getWord8
         if c
            then act
            else return Nothing

      -- Legacy prefixes in groups other than 2 or 3 aren't supported with
      -- VEX/XOP encoding. REX prefix isn't supported either.
      -- This function checks this
      checkVexPrefixes act = do
         let ps' = filter (\x -> legacyPrefixGroup x /= 2 
                              && legacyPrefixGroup x /= 3) ps
         case ps' of
            [] -> case rex of
               Nothing -> Just <$> act
               _       -> fail "REX prefix found with VEX/XOP opcode"
            _  -> fail ("Invalid legacy prefixes found with VEX/XOP opcode: "
                           ++ show ps')

   lookAheadM $ getWord8 >>= \case
      0x8F  |  isXOPAllowed -> checkVexPrefixes $
                  OpXop <$> (Vex3 <$> getWord8 <*> getWord8) <*> getWord8

      0xC4  |  isVEXAllowed -> isVexMode $ checkVexPrefixes $
                  OpVex <$> (Vex3 <$> getWord8 <*> getWord8) <*> getWord8

      0xC5  |  isVEXAllowed -> isVexMode $ checkVexPrefixes $
                  OpVex <$> (Vex2 <$> getWord8) <*> getWord8

      _ -> return Nothing

      
-- ===========================================================================
-- Operands
-- ===========================================================================

-- | Read instruction operands
readOperands :: ExecMode -> [LegacyPrefix] -> Opcode -> Encoding -> Get [Operand]
readOperands mode ps oc enc = do

   -- read ModRM
   modrm <- if encRequireModRM enc
            then (Just . ModRM . BitFields) <$> getWord8
            else return Nothing


   let
      -- we compute the overriden address size. It depends on:
      --    * the default address size
      --    * the presence of the 0x67 legacy prefix
      hasPrefix67 = LegacyPrefix67 `elem` ps
      addressSize = overriddenAddressSize hasPrefix67 mode

      -- we determine the effective operand size. It depends on:
      --   * the mode of execution
      --   * the presence of the 0x66 legacy prefix
      --   * the default operand size of the instruction in 64-bit mode (64-bit
      --   or not)
      --   * the value of the ForceNo8bit bit in the opcode (if applicable)
      --   * the value of REX.W/VEX.W/XOP.W (if applicable)
      hasPrefix66     = LegacyPrefix66 `elem` ps
      hasDefaultOp64  = DefaultOperandSize64 `elem` encProperties enc

      --finally we take into account the NoForce8bit bit in the opcode
      hasForce8bit = case encNoForce8Bit enc of
         Just b | not (testBit (opcodeByte oc) b) -> True
         _                                        -> False

      
      hasSignExtendBit = case encSignExtendImmBit enc of
         Just b | testBit (opcodeByte oc) b -> True
         _                                  -> False

      hasFPUSizeBit = case encFPUSizableBit enc of
         Just b | testBit (opcodeByte oc) b -> True
         _                                  -> False

      is64bitMode' = is64bitMode (x86Mode mode)
      isRegModRM   = case modrm of
         Nothing -> False
         Just m  -> rmRegMode m

      -- predicate oracle
      oracle = makeOracle <|
         [ (ContextPred (Mode m), UnsetPred) | m <- allModes
                                             , m /= x86Mode mode]
         ++
         [ (ContextPred (Mode (x86Mode mode)), SetPred)
         , (ContextPred CS_D         , if | is64bitMode'           -> UnsetPred
                                          | csDescriptorFlagD mode -> SetPred
                                          | otherwise              -> UnsetPred)
         , (ContextPred SS_B         , if | ssDescriptorFlagB mode -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred Prefix66      , if | hasPrefix66            -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred Prefix67      , if | hasPrefix67            -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred PrefixW       , if | opcodeW oc             -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred PrefixL       , case opcodeL oc of
                                          Nothing    -> UndefPred
                                          Just True  -> SetPred
                                          Just False -> UnsetPred)
         , (InsnPred Default64OpSize , if | hasDefaultOp64         -> SetPred
                                          | otherwise              -> UnsetPred)
         , (InsnPred Force8bit       , if | hasForce8bit           -> SetPred
                                          | otherwise              -> UnsetPred)
         , (InsnPred SignExtendBit   , if | hasSignExtendBit       -> SetPred
                                          | otherwise              -> UnsetPred)
         , (InsnPred FPUSizeBit      , if | hasFPUSizeBit          -> SetPred
                                          | otherwise              -> UnsetPred)
         , (InsnPred RegModRM        , if | isRegModRM             -> SetPred
                                          | otherwise              -> UnsetPred)
         , (EncodingPred PRexEncoding, case oc of
               (OpLegacy _ (Just _) _ _) -> SetPred
               (OpLegacy _ Nothing _ _)  -> UnsetPred
               _                         -> UndefPred)
         , (EncodingPred (case oc of
               OpLegacy {} -> PLegacyEncoding
               OpVex    {} -> PVexEncoding
               OpXop    {} -> PXopEncoding), SetPred)
         ]

      -- reduce operands predicates
      ops = encOperands enc
            ||> reducePredicates oracle
            ||> \case
               Match a     -> a
               DontMatch x -> error ("Operand doesn't reduce: "
                                 ++ show x
                                 ++ ". Terminals: "
                                 ++ show (getTerminals x))

               e           -> error ("Operand doesn't reduce: "++ show e)

   ----------------------
   -- SIB
   ----------------------

   let
      -- do we need to read an SIB byte?
      hasSIB = fromMaybe False (useSIB addressSize <$> modrm)

   -- read SIB byte if necessary
   msib <- if hasSIB
      then (Just . SIB) <$> getWord8
      else return Nothing

   ----------------------
   -- DISPLACEMENT
   ----------------------

   let
      -- Do we need to read a displacement? Which size?
      -- We can find the info in the SIB for usual memory addresses.
      --
      -- But some instructions have fixed displacement size (e.g., MOV
      -- moffs[8,16,32] or Jcc rel[8,16,32]): we can find the disp size in the
      -- operand spec.

      getMemDisp x = case opFam x of
         T_Mem m -> do
            a <- memFamAddr m
            addrFamDispSize a
         _       -> Nothing

      mDispSize = case mapMaybe getMemDisp ops of
         []  -> Nothing
         [d] -> Just d
         as  -> error ("Found more than one memory operand with displacement: "++ show as)

      dispSize = mDispSize <|> join (useDisplacement addressSize msib <$> modrm)

   -- read displacement if necessary
   disp <- forM dispSize getSize64

   ----------------------
   -- IMMEDIATE
   ----------------------

   let
      -- do we need to read some immediates?
      immTypeSize = \case
         T_Imm i    -> [immFamSize i]
         T_Pair x y -> concat [ immTypeSize x -- immediate pointer: 16:16 or 16:32
                              , immTypeSize y
                              ]
         T_Mem _    -> [] -- some T_Mem only displacement, hence they have S_Imm storage
                          -- but the displacement has been already read
         it         -> error ("Unhandled immediate type: " ++ show it)

      gimmSize x = case opStore x of
         S_Imm8h -> [OpSize8]
         S_Imm8l -> [OpSize8]
         S_Imm   -> immTypeSize (opFam x)
         _       -> error ("unhandled immediate encoding: " ++ show x)

      immSizes = case filter (isImmediate . opStore) ops of
         []    -> []
         [x]   -> gimmSize x
         [x,y] 
            | opStore x == S_Imm8h && opStore y == S_Imm8l -> [OpSize8]
            | opStore x == S_Imm8l && opStore y == S_Imm8h -> [OpSize8]
         xs    -> concatMap gimmSize xs

   -- read immediates if necessary
   imms <- forM immSizes getOpSize64

   ----------------------------------------------------------------------------
   -- at this point we have read the whole instruction (except in the 3DNow!
   -- case where there is a single opcode byte left to read). Now we can
   -- determine the operands.
   ----------------------------------------------------------------------------

   let

      -- update operand families with actual values

      readParam spec = case (opFam spec, opStore spec) of
         (x, S_Implicit)   -> x
         (T_Mem mem, S_RM) -> case modrm of
            Just modrm' -> T_Mem $ mem
               { memFamAddr = Just $ setAddrFam is64bitMode' ps oc addressSize useExtRegs modrm' msib
                                       dispSize disp
                                       (fromMaybe emptyAddrFam (memFamAddr mem))
               }
            Nothing     -> error "ModRM required"
                           
         (T_Reg fam, S_RM)         -> T_Reg (setRegFamId useExtRegs (fromIntegral modRMrm)     fam)
         (T_Reg fam, S_Reg)        -> T_Reg (setRegFamId useExtRegs (fromIntegral modRMreg)    fam)
         (T_Reg fam, S_Vvvv)       -> T_Reg (setRegFamId useExtRegs (fromIntegral vvvv)        fam)
         (T_Reg fam, S_OpcodeLow3) -> T_Reg (setRegFamId useExtRegs (fromIntegral opcodeRegId) fam)

         (T_Mem mem, S_Imm) -> T_Mem $ mem
            { memFamAddr = (\a -> a { addrFamDisp = disp}) <$> memFamAddr mem
            }

         (T_Pair (T_Imm i1) (T_Imm i2), S_Imm) -> case imms of
            [x,y] -> T_Pair (T_Imm (i1 { immFamValue = Just x }))
                            (T_Imm (i2 { immFamValue = Just y }))
            xs -> error ("Expected immediate pair, got: " ++ show xs)


         (T_Imm i, s)
            | s `elem` [S_Imm, S_Imm8l, S_Imm8h] -> case imms of
               [x] -> T_Imm (i { immFamValue = Just x })
               xs  -> error ("Expecting a single immediate, got: " ++ show xs)

         x -> error ("Unhandled param: " ++ show x)


      -- extended ModRM.reg (with REX.R, VEX.R, etc.)
      modRMreg = opcodeR oc `uncheckedShiftL` 3 .|. regField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm
            
      -- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
      modRMrm = opcodeB oc `uncheckedShiftL` 3 .|. rmField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm

      -- | Extended register id in opcode (with REX.B, VEX.B, etc.)
      opcodeRegId =  opcodeB oc `uncheckedShiftL` 3 .|. (opcodeByte oc .&. 0b111)

      -- VVVV field
      vvvv = case oc of
         OpLegacy {} -> error "Trying to read Vvvv with legacy opcode"
         OpVex v _   -> vexVVVV v
         OpXop v _   -> vexVVVV v

      
      -- when a REX prefix is used, some 8-bit registers cannot be encoded
      useExtRegs = case oc of
         OpLegacy _ Nothing _ _  -> False
         OpLegacy _ (Just _) _ _ -> True
         _ -> error ("useExtRegs: we shouldn't check for 8-bit registers with non-legacy opcode: " ++ show oc)

         
   -- fixup values in operands families
   let ops'' = fmap readParam ops

   -- convert operand families into operands
   let ops' = fmap (fromJust . opFamToOp) ops''

   -- reverse operands (FPU dest, reversable bit)
   let fops = case encReversableBit enc <|> encFPUDestBit enc of
               Just b
                  | testBit (opcodeByte oc) b -> reverse ops'
               _                              -> ops'

   return fops


