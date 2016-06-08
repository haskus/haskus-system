{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}

module ViperVM.Arch.X86_64.ISA.Decoder
   ( getInstruction
   , EncodingVariant(..)
   , Insn (..)
   , ExecMode (..)
   )
where

import ViperVM.Arch.X86_64.ISA.Mode
import ViperVM.Arch.X86_64.ISA.Registers
import ViperVM.Arch.X86_64.ISA.Size
import ViperVM.Arch.X86_64.ISA.Tables
import ViperVM.Arch.X86_64.ISA.Insns
import ViperVM.Arch.X86_64.ISA.Encoding

import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.BitSet (BitSet,CBitSet)
import qualified ViperVM.Format.Binary.BitSet as BitSet

import qualified Data.Map as Map
import Data.List (nub)
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad

-- ===========================================================================
-- X86 execution mode
-- ===========================================================================

-- | Execution mode
data ExecMode = ExecMode
   { x86Mode            :: X86Mode        -- ^ x86 mode
   , defaultAddressSize :: AddressSize    -- ^ Default address size (used in protected/compat mode)
   , defaultOperandSize :: OperandSize    -- ^ Default operand size (used in protected/compat mode)
   , extensions         :: [X86Extension] -- ^ Enabled extensions
   }

-- | Indicate if an extension is enabled
hasExtension :: ExecMode -> X86Extension -> Bool
hasExtension mode ext = ext `elem` extensions mode


-- ===========================================================================
-- X86 Instruction
-- ===========================================================================

data Insn = Insn
   { insnOpcode   :: Opcode
   , insnOperands :: [Operand]
   , insnEncoding :: Encoding
   , insnSpec     :: X86Insn
   , insnVariant  :: BitSet Word16 EncodingVariant
   }
   deriving (Show)

-- | Instruction variant encoding
data EncodingVariant
   = Locked                     -- ^ Locked memory access
   | Reversed                   -- ^ Parameters are reversed (useful when some instructions have two valid encodings, e.g. CMP reg8, reg8)
   | ExplicitParam              -- ^ A variant exists with an implicit parameter, but the explicit variant is used
   | RepeatZero                 -- ^ REP(Z) prefix
   | RepeatNonZero              -- ^ REPNZ prefix
   | LockEllisionAcquire        -- ^ XACQUIRE prefix
   | LockEllisionRelease        -- ^ XRELEASE prefix
   | BranchHintTaken            -- ^ Branch hint (branch taken)
   | BranchHintNotTaken         -- ^ Branch hint (not taken)
   | SuperfluousSegmentOverride -- ^ Segment override equal to default segment
   deriving (Show,Eq,Enum,CBitSet)


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
            isPrefixValid e x = Just x == encMandatoryPrefix e
               || case x of
                  -- operand-size prefix
                  LegacyPrefix66 -> True
                  -- address-size prefix
                  LegacyPrefix67 -> encMayHaveMemoryOperand e
                  -- CS segment override / Branch not taken hint
                  LegacyPrefix2E -> encMayHaveMemoryOperand e
                                    || encBranchHintable e
                  -- DS segment override / Branch taken hint
                  LegacyPrefix3E -> encMayHaveMemoryOperand e
                                    || encBranchHintable e
                  -- ES segment override
                  LegacyPrefix26 -> encMayHaveMemoryOperand e 
                  -- FS segment override
                  LegacyPrefix64 -> encMayHaveMemoryOperand e 
                  -- GS segment override
                  LegacyPrefix65 -> encMayHaveMemoryOperand e 
                  -- SS segment override
                  LegacyPrefix36 -> encMayHaveMemoryOperand e 
                  -- LOCK prefix
                  LegacyPrefixF0 -> encLockable e
                  -- REPZ / XRELEASE
                  LegacyPrefixF3 -> encRepeatable e
                                    || encSupportHLE XRelease e
                  -- REPNZ / XACQUIRE
                  LegacyPrefixF2 -> encRepeatable e
                                    || encSupportHLE XAcquire e

            arePrefixesValid c = all (isPrefixValid (entryEncoding c)) ps

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
                  fullext = case (modrm,encOpcodeExt e) of
                     (_, Nothing)      -> True
                     (Just m, Just x)  -> m == x
                     (Nothing, Just _) -> False
                  -- extension in ModRM.Reg
                  regext = case (modrm,encOpcodeFullExt e) of
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

         when (null cs3) $ fail "No candidate instruction found (ModRM extension filtering)"

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

         -- Filter out invalid enabled extensions/architecture. Return sensible error
         -- if no instruction left (e.g., in order to provide suggestion to enable an
         -- extension).
         -- Filter out invalid prefixes (LOCK, etc.)
         -- Filter invalid VEX.L/VEX.W
         -- FIXME
         let (errs,cs5) = (undefined,cs4)

         when (null cs5) $ fail errs

         -- If there are more than one instruction left, signal a bug
         MapEntry spec enc <- case cs5 of
            [x] -> return x
            xs  -> fail ("More than one instruction found (opcode table bug?): " ++ show xs)

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

---------------------------------------------------------------------------
-- Legacy prefixes
-- ~~~~~~~~~~~~~~~
-- 
-- An instruction optionally begins with up to five legacy prefixes, in any
-- order. These prefixes can:
--    1) modify the instruction's default address size
--    2) modify the instruction's default operand size
--    3) modify the instruction's memory address segment
--    4) be used as an opcode extension
--    5) provide atomic bus locking or hardware-lock elision (HLE)
--    6) repeat the instruction until a condition is met
--
-- Note: the effective sizes of the operands may not be the same: the shorter
-- may be sign-extended or zero-extended.
--
-- Legacy prefixes that are used as opcode extensions are mandatory.
--
--
-- Legacy prefix groups
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Legacy prefixes are organized in five groups. An instruction may include
-- at most one prefix from each group. The result of using multiple prefixes
-- from a single group is undefined.
--
-- We give the original meaning in parentheses, but prefixes can be used with
-- other meanings.
--
-- G1: 0x66 (Operand-size override)
-- G2: 0x67 (Address-size override)
-- G3: 0x2E (CS segment override)
--     0x3E (DS segment override)
--     0x26 (ES segment override)
--     0x64 (FS segment override)
--     0x65 (GS segment override)
--     0x36 (SS segment override)
-- G4: 0xF0 (atomic memory access (lock))
-- G5: 0xF3 (repeat while zero)
--     0xF2 (repeat while non-zero)
--
-- New opcode encodings (VEX, XOP, etc.) only support legacy prefixes from
-- groups G2 and G3.
---------------------------------------------------------------------------

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
   
-- | Get the legacy prefix group
legacyPrefixGroup :: LegacyPrefix -> Int
legacyPrefixGroup = \case
   LegacyPrefix66  -> 1
   LegacyPrefix67  -> 2
   LegacyPrefix2E  -> 3
   LegacyPrefix3E  -> 3
   LegacyPrefix26  -> 3
   LegacyPrefix64  -> 3
   LegacyPrefix65  -> 3
   LegacyPrefix36  -> 3
   LegacyPrefixF0  -> 4
   LegacyPrefixF3  -> 5
   LegacyPrefixF2  -> 5


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
-- Some legacy prefixes are supported: address-size and sesgment override.
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
      testMod w    = w `unsafeShiftR` 6 == 0x03

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
      -- we determine the effective address size. It depends on:
      --    * the mode of execution
      --    * the presence of the 0x67 legacy prefix
      --    * the default address size of the instruction in 64-bit mode
      hasAddressSizePrefix = LegacyPrefix67 `elem` ps
      addrSize16o32 = case (defaultAddressSize mode, hasAddressSizePrefix) of
         (AddrSize16, False) -> AddrSize16
         (AddrSize32, False) -> AddrSize32
         (AddrSize16, True)  -> AddrSize32
         (AddrSize32, True)  -> AddrSize16
         (a,_)               -> error ("Invalid default address size for the current mode: "
                                       ++ show (x86Mode mode) ++ " and "
                                       ++ show a)

      -- some instructions have 64-bit address size by default, in this case the
      -- prefix can switch back to 32-bit address size
      hasDefaultAddress64 = DefaultAddressSize64 `elem` encProperties enc
      addrSize32o64 = case (hasDefaultAddress64, hasAddressSizePrefix) of
         (True, False)  -> AddrSize64
         (True, True)   -> AddrSize32
         (False, False) -> AddrSize64
         (False, True)  -> AddrSize32

      addressSize = case x86Mode mode of
         -- old modes defaulting to 16-bit
         LegacyMode RealMode        -> AddrSize16
         LegacyMode Virtual8086Mode -> AddrSize16
         -- protected modes that can either be 16- or 32-bit
         -- The default mode is indicated in the segment descriptor (D "default
         -- size" flag)
         LegacyMode ProtectedMode   -> addrSize16o32
         LongMode CompatibilityMode -> addrSize16o32
         -- long mode that can be either 32- or 64-bit
         LongMode Long64bitMode     -> addrSize32o64

   let
      -- we determine the effective operand size. It depends on:
      --   * the mode of execution
      --   * the presence of the 0x66 legacy prefix
      --   * the default operand size of the instruction in 64-bit mode
      --   * the value of the ForceNo8bit bit in the opcode (if applicable)
      --   * the value of REX.W/VEX.W/XOP.W (if applicable)
      hasOperandSizePrefix = LegacyPrefix66 `elem` ps
      hasDefaultOp64       = DefaultOperandSize64 `elem` encProperties enc
      hasRexW              = opcodeW oc

      opSize16o32 = case (defaultOperandSize mode, hasOperandSizePrefix) of
         (OpSize16, False) -> OpSize16
         (OpSize32, False) -> OpSize32
         (OpSize16, True)  -> OpSize32
         (OpSize32, True)  -> OpSize16
         (a,_)               -> error ("Invalid default address size for the current mode: "
                                       ++ show (x86Mode mode) ++ " and "
                                       ++ show a)

      -- in 64-bit mode, most 64-bit instructions default to 32-bit operand
      -- size, except those with the DefaultOperandSize64 property.
      -- REX.W/VEX.W/XOP.W can be used to set a 64-bit operand size (it has
      -- precedence over the 0x66 legacy prefix)
      opSize16o32o64 = case (hasDefaultOp64, hasRexW, hasOperandSizePrefix) of
         (True,     _,      _) -> OpSize64
         (False, True,      _) -> OpSize64
         (False, False, False) -> OpSize32
         (False, False, True ) -> OpSize16

      defOperandSize = case x86Mode mode of
         -- old modes defaulting to 16-bit
         LegacyMode RealMode        -> OpSize16
         LegacyMode Virtual8086Mode -> OpSize16
         -- protected modes that can either be 16- or 32-bit
         -- The default mode is indicated in the segment descriptor (D "default
         -- size" flag)
         LegacyMode ProtectedMode   -> opSize16o32
         LongMode CompatibilityMode -> opSize16o32
         -- 64-bit mode can be either 16-, 32- or 64-bit
         LongMode Long64bitMode     -> opSize16o32o64

      --finally we take into account the NoForce8bit bit in the opcode
      operandSize = case encNoForce8Bit enc of
         Just b
            | testBit (opcodeByte oc) b -> defOperandSize
            | otherwise                 -> OpSize8
         _                              -> defOperandSize

      -- do we need to read an SIB byte?
      hasSIB = fromMaybe False (useSIB addressSize <$> modrm)

      -- do we need to read a MemOffset displacement?
      hasMemOffset = T_MemOffset `elem` fmap opType (encOperands enc)

      -- do we need to read a Relative displacement?
      hasRelOffset = filter isRel (fmap opType (encOperands enc))
         where isRel (T_Rel _) = True
               isRel _         = False

      -- do we need to read a displacement? Which size?
      dispSize = case (hasMemOffset, hasRelOffset) of
         (False, []) -> join (useDisplacement addressSize <$> modrm)
         (True,  []) -> case addressSize of
                           AddrSize16 -> Just Size16
                           AddrSize32 -> Just Size32
                           AddrSize64 -> Just Size64
         (False, [T_Rel rel]) -> case rel of
            Rel8     -> Just Size8
            Rel16o32 -> case operandSize of
               OpSize64 -> Just Size32
               OpSize32 -> Just Size32
               OpSize16 -> Just Size16
               OpSize8  -> error "Unsupported relative offset with 8-bit operand size"
         (_, xs) -> error ("Unsupported relative offsets: " ++ show xs)


   -- read SIB byte if necessary
   sib <- if hasSIB
      then (Just . SIB) <$> getWord8
      else return Nothing

   -- read displacement if necessary
   disp <- forM dispSize getSize


   let
      -- do we need to read some immediates?
      immTypeSize = \case
         T_MemOffset     -> [] -- already read above as displacement
         T_Rel _         -> [] -- already read above as displacement
         T_Imm ImmSize8  -> [Size8]
         T_Imm ImmSize16 -> [Size16]
         T_Imm ImmSizeOp -> case operandSize of
                              OpSize8  -> [Size8]
                              OpSize16 -> [Size16]
                              OpSize32 -> [Size32]
                              OpSize64 -> [Size64]
         T_Imm ImmSizeSE -> case encSignExtendImmBit enc of
                              -- if the sign-extendable bit is set, we read an
                              -- Imm8 that will be sign-extended to match the
                              -- operand size
                              Just t
                                 | testBit (opcodeByte oc) t -> [Size8]
                              _ -> case operandSize of
                                 OpSize8  -> [Size8]
                                 OpSize16 -> [Size16]
                                 OpSize32 -> [Size32]
                                 OpSize64 -> [Size32] -- sign-extended
         T_Pair x y      -> [ head (immTypeSize x) -- immediate pointer: 16:16 or 16:32
                            , head (immTypeSize y)]
         it              -> error ("Unhandled immediate type: " ++ show it)

      immSize x = case opEnc x of
         Imm8h -> [Size8]
         Imm8l -> [Size8]
         Imm   -> immTypeSize (opType x)
         _     -> error ("unhandled immediate encoding: " ++ show x)

      immSizes = case filter (isImmediate . opEnc) (encOperands enc) of
         []    -> []
         [x]   -> immSize x
         [x,y] 
            | opEnc x == Imm8h && opEnc y == Imm8l -> [Size8]
            | opEnc x == Imm8l && opEnc y == Imm8h -> [Size8]
         xs    -> concatMap immSize xs

   -- read immediates if necessary
   imms <- forM immSizes getSize

   ----------------------------------------------------------------------------
   -- at this point we have read the whole instruction (except in the 3DNow!
   -- case where there is a single byte left to read). Now we can determine the
   -- operands.
   ----------------------------------------------------------------------------

   let
      is64bitMode' = is64bitMode (x86Mode mode)

      readParam spec = case opType spec of
         -- One of the two types (for ModRM.rm)
         TME r m -> case modField <$> modrm of
            Just 0b11 -> readParam (spec { opType = r })
            Just _    -> readParam (spec { opType = m })
            Nothing   -> fail "Cannot read ModRM.mod"
         
         -- One of the two types depending on Vex.L
         TLE l128 l256 -> case opcodeL oc of
            Just False -> readParam (spec { opType = l128 })
            Just True  -> readParam (spec { opType = l256 })
            Nothing    -> fail "Cannot read VEX.L/XOP.L"

         -- One of the two types depending on Rex.W
         TWE now w     -> if hasRexW
                              then readParam (spec { opType = w })
                              else readParam (spec { opType = now })
         
         -- Memory address
         T_Mem mtype -> case modrm of
            Nothing     -> fail "ModRM required"
            Just modrm' -> return $ OpMem mtype $ Addr seg' base idx scl disp
               where
                  toR = case addressSize of
                           AddrSize32 -> reg32
                           AddrSize64 -> reg64
                           AddrSize16 -> error "Trying to use AddrSize16"
                  base = if addressSize == AddrSize16
                           then case (modField modrm', rmField modrm') of
                              (_,    0b000) -> Just R_BX
                              (_,    0b001) -> Just R_BX
                              (_,    0b010) -> Just R_BP
                              (_,    0b011) -> Just R_BP
                              (_,    0b100) -> Nothing    
                              (_,    0b101) -> Nothing    
                              (0b00, 0b110) -> Nothing    
                              (_,    0b110) -> Just R_BP
                              (_,    0b111) -> Just R_BX
                              _             -> error "Invalid 16-bit addressing"
                           else case (modField modrm', rmField modrm') of
                              (0b00, 0b101) -> if is64bitMode'
                                                   then Just R_RIP
                                                   else Nothing
                              -- SIB: if mod is 0b00, don't use EBP as base.
                              (0b00, 0b100)
                                 | baseField sib' == 0b101 -> Nothing
                              (_,    0b100) -> Just (toR sibBase)
                              _             -> Just (toR modRMrm)
                  idx = if addressSize == AddrSize16
                           then case rmField modrm' of
                              0b000 -> Just R_SI
                              0b001 -> Just R_DI
                              0b010 -> Just R_SI
                              0b011 -> Just R_DI
                              0b100 -> Just R_SI
                              0b101 -> Just R_DI
                              0b110 -> Nothing
                              0b111 -> Nothing
                              _     -> error "Invalid 16-bit addressing"
                        else case (rmField modrm', indexField sib') of
                           -- SIB: if index is 0b100 (should be ESP), don't
                           -- use any index
                           (0b100, 0b100) -> Nothing
                           (0b100, _    ) -> Just (toR sibIdx)
                           _              -> Nothing -- no SIB
                  scl = if addressSize /= AddrSize16 && rmField modrm' == 0b100
                           then Just (scaleField sib')
                           else Nothing
                  seg' = fromMaybe (defaultSegment base) segOverride
                        

         -- Register
         T_Reg rtype -> case rtype of
               RegVec64    -> return $ OpReg $ R_MMX regid
               RegVec128   -> return $ OpReg $ R_XMM regid
               RegVec256   -> return $ OpReg $ R_YMM regid
               RegFixed r  -> return $ OpReg r
               RegSegment  -> OpReg <$> case regid of
                                0 -> return R_ES
                                1 -> return R_CS
                                2 -> return R_SS
                                3 -> return R_DS
                                4 -> return R_FS
                                5 -> return R_GS
                                _ -> fail ("Invalid segment register id: " ++ show regid)
               RegControl  -> return $ OpReg $ R_CR regid
               RegDebug    -> return $ OpReg $ R_DR regid
               Reg8        -> return $ OpReg $ reg8 regid
               Reg16       -> return $ OpReg $ reg16 regid
               Reg32       -> return $ OpReg $ reg32 regid
               Reg64       -> return $ OpReg $ reg64 regid
               Reg32o64    -> return $ OpReg $ if is64bitMode'
                                 then reg64 regid
                                 else reg32 regid
               RegOpSize   -> return $ OpReg $ case operandSize of
                                OpSize8  -> reg8  regid
                                OpSize16 -> reg16 regid
                                OpSize32 -> reg32 regid
                                OpSize64 -> reg64 regid
               RegST       -> return $ OpReg $ R_ST regid
               RegCounter  -> return $ OpReg $ case addressSize of
                                AddrSize16 -> R_CX
                                AddrSize32 -> R_ECX
                                AddrSize64 -> R_RCX
               RegAccu     -> return $ OpReg $ gpr operandSize 0
               RegStackPtr -> return $ OpReg rSP
               RegBasePtr  -> return $ OpReg rBP
               RegFam rf   -> return $ case rf of
                                 RegFamAX -> OpReg $ gpr operandSize 0
                                 RegFamBX -> OpReg $ gpr operandSize 3
                                 RegFamCX -> OpReg $ gpr operandSize 1
                                 RegFamDX -> OpReg $ gpr operandSize 2
                                 RegFamSI -> OpReg $ gpr operandSize 6
                                 RegFamDI -> OpReg $ gpr operandSize 7
                                 RegFamDXAX -> case operandSize of
                                    OpSize8  -> OpReg R_AX
                                    OpSize16 -> OpRegPair R_DX R_AX
                                    OpSize32 -> OpRegPair R_EDX R_EAX
                                    OpSize64 -> OpRegPair R_RDX R_RAX
            where
               regid = case opEnc spec of
                  RM         -> modRMrm
                  Reg        -> modRMreg
                  Vvvv       -> vvvv
                  OpcodeLow3 -> opcodeRegId
                  e          -> error ("Invalid register encoding: " ++ show e)
         
         -- Sub-part of a register
         T_SubReg _ rtype -> readParam (spec {opType = T_Reg rtype})

         -- Pair (AAA:BBB)
         T_Pair (T_Reg (RegFixed r1)) (T_Reg (RegFixed r2)) -> return (OpRegPair r1 r2)

         T_Pair (T_Imm ImmSize16) (T_Imm ImmSizeOp) -> return $ case imms of
            [SizedValue16 x, SizedValue16 y] -> OpPtr16_16 x y
            [SizedValue16 x, SizedValue32 y] -> OpPtr16_32 x y
            xs -> error ("Invalid immediate operands for ptr16x: " ++ show xs)

         T_Pair (T_Imm ImmSize16) (T_Imm ImmSize8) -> return $ case imms of
            [SizedValue16 x, SizedValue8 y] -> OpStackFrame x y
            xs -> error ("Invalid immediate operands for ENTER: " ++ show xs)

         T_Pair x y -> error ("Unhandled operand pair: " ++ show (x,y))

         -- Immediate
         T_Imm (ImmConst n) ->
            -- 8-bit is currently enough for known instructions
            return (OpImmediate (SizedValue8 (fromIntegral n)))

         T_Imm _            -> case imms of
            [x] -> return (OpImmediate x)
            xs  -> error ("Invalid immediate: " ++ show xs)

         -- IP relative offset
         T_Rel _ -> return (OpCodeAddr Addr
            { addrSeg   = R_CS
            , addrBase  = Just rIP
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })
         
         -- Segment relative offset
         T_MemOffset -> return (OpCodeAddr Addr
            { addrSeg   = seg
            , addrBase  = Nothing
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })

         -- DS:EAX or DS:RAX (used by monitor)
         T_MemDSrAX -> return (OpMem MemVoid Addr
            { addrSeg   = R_DS
            , addrBase  = Just $ if is64bitMode' then R_RAX else R_EAX
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })

      -- The default segment is DS except
      --  * for some instruction (cf DefaultSegment property)
      --  * if rBP or rSP is used as base (in which case it is SS)
      --  * for string instructions' source operand (in which case it is ES)
      defSeg = case filter isD (encProperties enc) of
            []                 -> R_DS
            [DefaultSegment s] -> s
            _                  -> error "More than one default segment"
         where
            isD (DefaultSegment _) = True
            isD _                  = False

      -- segment override prefixes
      segOverride = case filter ((== 3) . legacyPrefixGroup) ps of
         []               -> Nothing
         [LegacyPrefix2E] -> Just R_CS
         [LegacyPrefix3E] -> Just R_DS
         [LegacyPrefix26] -> Just R_ES
         [LegacyPrefix64] -> Just R_FS
         [LegacyPrefix65] -> Just R_GS
         [LegacyPrefix36] -> Just R_SS
         xs -> error ("More than one segment-override prefix: "++show xs)

      -- memory segment (when override is allowed)
      seg = fromMaybe defSeg segOverride

      rSP = if is64bitMode'
               then R_RSP
               else case addressSize of
                 AddrSize16 -> R_SP
                 AddrSize32 -> R_ESP
                 AddrSize64 -> R_RSP
      
      rBP = if is64bitMode'
               then R_RBP  
               else case addressSize of
                 AddrSize16 -> R_BP
                 AddrSize32 -> R_EBP
                 AddrSize64 -> R_RBP

      rIP = case addressSize of
               AddrSize16 -> R_IP
               AddrSize32 -> R_EIP
               AddrSize64 -> R_RIP

      gpr osize r = case osize of
         OpSize8  -> reg8  r
         OpSize16 -> reg16 r
         OpSize32 -> reg32 r
         OpSize64 -> reg64 r

      reg8 = \case
            0              -> R_AL
            1              -> R_CL
            2              -> R_DL
            3              -> R_BL
            4 | useExtRegs -> R_SPL
              | otherwise  -> R_AH
            5 | useExtRegs -> R_BPL
              | otherwise  -> R_CH
            6 | useExtRegs -> R_SIL
              | otherwise  -> R_DH
            7 | useExtRegs -> R_DIL
              | otherwise  -> R_BH
            8              -> R_R8L
            9              -> R_R9L
            10             -> R_R10L
            11             -> R_R11L
            12             -> R_R12L
            13             -> R_R13L
            14             -> R_R14L
            15             -> R_R15L
            r              -> error ("Invalid reg8 id: " ++ show r)

      reg16 = \case
            0              -> R_AX
            1              -> R_CX
            2              -> R_DX
            3              -> R_BX
            4              -> R_SP
            5              -> R_BP
            6              -> R_SI
            7              -> R_DI
            8              -> R_R8W
            9              -> R_R9W
            10             -> R_R10W
            11             -> R_R11W
            12             -> R_R12W
            13             -> R_R13W
            14             -> R_R14W
            15             -> R_R15W
            r              -> error ("Invalid reg16 id: " ++ show r)

      reg32 = \case
            0              -> R_EAX
            1              -> R_ECX
            2              -> R_EDX
            3              -> R_EBX
            4              -> R_ESP
            5              -> R_EBP
            6              -> R_ESI
            7              -> R_EDI
            8              -> R_R8D
            9              -> R_R9D
            10             -> R_R10D
            11             -> R_R11D
            12             -> R_R12D
            13             -> R_R13D
            14             -> R_R14D
            15             -> R_R15D
            r              -> error ("Invalid reg32 id: " ++ show r)

      reg64 = \case
            0              -> R_RAX
            1              -> R_RCX
            2              -> R_RDX
            3              -> R_RBX
            4              -> R_RSP
            5              -> R_RBP
            6              -> R_RSI
            7              -> R_RDI
            8              -> R_R8
            9              -> R_R9
            10             -> R_R10
            11             -> R_R11
            12             -> R_R12
            13             -> R_R13
            14             -> R_R14
            15             -> R_R15
            r              -> error ("Invalid reg64 id: " ++ show r)


      -- extended ModRM.reg (with REX.R, VEX.R, etc.)
      modRMreg = opcodeR oc `unsafeShiftL` 3 .|. regField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm
            
      -- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
      modRMrm = opcodeB oc `unsafeShiftL` 3 .|. rmField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm

      sib' = fromJust sib

      -- | Extended SIB index (with REX.X, VEX.X, etc.)
      sibIdx = opcodeX oc `unsafeShiftL` 3 .|. indexField sib'
            
      -- | Extended SIB base (with REX.B, VEX.B, etc.)
      sibBase = opcodeB oc `unsafeShiftL` 3 .|. baseField sib'

      -- | Extended register id in opcode (with REX.B, VEX.B, etc.)
      opcodeRegId =  opcodeB oc `unsafeShiftL` 3 .|. (opcodeByte oc .&. 0b111)

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

         
   ops' <- forM (encOperands enc) readParam

   -- reverse operands (FPU dest, reversable bit)
   let ops = case (encReversableBit enc, encFPUDestBit enc) of
               (Just b, Nothing)
                  | testBit (opcodeByte oc) b -> reverse ops'
               (Nothing, Just b)
                  | testBit (opcodeByte oc) b -> reverse ops'
               _                              -> ops'

   return ops


-- | Give the default segment for the given base register
defaultSegment :: Maybe Register -> Register
defaultSegment = \case
   Just R_BP  -> R_SS
   Just R_SP  -> R_SS
   Just R_EBP -> R_SS
   Just R_ESP -> R_SS
   Just R_RBP -> R_SS
   Just R_RSP -> R_SS
   _          -> R_DS
