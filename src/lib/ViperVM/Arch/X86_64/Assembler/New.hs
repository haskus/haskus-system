{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}

module ViperVM.Arch.X86_64.Assembler.New
   ( getInstruction
   , ExecMode (..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Opcode
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Tables
import ViperVM.Arch.X86_64.Assembler.Insns

import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.BitField

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
   , defaultAddressSize :: AddressSize    -- ^ Default address size
   , defaultOperandSize :: Size           -- ^ Default operand size
   , extensions         :: [X86Extension] -- ^ Enabled extensions
   }

-- | Indicate if an extension is enabled
hasExtension :: ExecMode -> X86Extension -> Bool
hasExtension mode ext = ext `elem` extensions mode


-- ===========================================================================
-- X86 Instruction
-- ===========================================================================

getInstruction :: ExecMode -> Get ()
getInstruction mode = consumeAtMost 15 $ do
   -- An instruction is at most 15 bytes long

   ps  <- readLegacyPrefixes
   rex <- readRexPrefix mode

   -- read opcode
   oc <- readVexXopOpcode mode ps rex >>= \case
      Just op -> return op
      Nothing -> readLegacyOpcode mode ps rex

   -- handle 3DNow!
   case getOpcodeMap oc of
      MapLegacy Map3DNow -> fail "3DNow! not supported" -- TODO
      ocmap              -> do
         -- get candidate instructions for the opcode
         cs <- case Map.lookup ocmap opcodeMaps of
                  Nothing -> fail "No opcode map found"
                  Just t  -> return (t V.! fromIntegral (getOpcode oc))

         when (null cs) $ fail "No candidate instruction found (empty opcode map cell)"

         -- check for mandatory prefixes
         let
            cs2 = filter hasMandatoryPrefix cs
            hasMandatoryPrefix i = case toLegacyPrefix <$> encMandatoryPrefix (entryEncoding i) of
               Nothing -> True
               Just mp -> case oc of
                  OpLegacy {} -> fromJust mp `elem` ps
                  OpVex v _   -> mp == vexPrefix v
                  OpXop v _   -> mp == vexPrefix v

         when (null cs2) $ fail "No candidate instruction found (invalid mandatory prefixes)"

         -- try to read ModRM
         modrm <- lookAhead $ remaining >>= \case
            x | x >= 1 -> Just <$> getWord8
            _          -> return Nothing

         -- filter out invalid extensions (extension in the whole second byte or in
         -- ModRM.reg)
         let
            cs3 = filter hasModRMExtension cs2
            hasModRMExtension i = case (modrm, encOpcodeFullExt e, encOpcodeExt e) of
                  -- No extension
                  (_, Nothing, Nothing) -> True
                  -- invalid
                  ( _, Just _, Just _)  -> error ("Invalid entry (both full and ModRM.reg extensions: " ++ show i)
                  -- cannot read ModRM but require extension
                  (Nothing, Just _, _)  -> False
                  (Nothing, _, Just _)  -> False
                  -- full extension
                  (Just m, Just x, Nothing) -> m == x
                  -- ModRM.reg extension
                  (Just m, Nothing, Just x) -> regField (ModRM (BitFields m)) == x
               where
                  e = entryEncoding i

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
         -- TODO
         let (errs,cs5) = (undefined,cs4)

         when (null cs5) $ fail errs

         -- If there are more than one instruction left, signal a bug
         entry <- case cs5 of
            [x] -> return x
            xs  -> fail ("More than one instruction found (opcode table bug?): " ++ show xs)

         -- Read params
         ops <- readOperands mode ps rex oc (entryEncoding entry)

         return ()

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
      -- TODO: use mode, arch or sets...
      isXOPAllowed = True
      isVEXAllowed = True

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
readOperands :: ExecMode -> [LegacyPrefix] -> Maybe Rex -> Opcode -> Encoding -> Get [Operand]
readOperands mode ps rex oc enc = do

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

      -- some instructions have 64-bit address size by default
      hasDefaultAddress64 = DefaultAddressSize64 `elem` encProperties enc
      addrSize32o64 = case (hasDefaultAddress64, hasAddressSizePrefix) of
         (True, False)  -> AddrSize64
         (True, True)   -> AddrSize32
         (False, False) -> AddrSize32
         (False, True)  -> AddrSize64

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

   -- do we need to read a memory operand?
   hasMemoryOperand <- case (encMayHaveMemoryOperand enc, modField <$> modrm) of
      (False, _       ) -> return False
      (True, Nothing  ) -> fail "Memory operand required but we cannot read ModRM"
      (True, Just 0b11) -> return False -- ModRM.mod == 0b11 (register in ModRM.rm)
      (True, Just _   ) -> return True  -- ModRM.mod /= 0b11 (memory in ModRM.rm)

   let
      -- do we need to read an SIB byte?
      hasSIB = fromMaybe False (((== RMSIB) . rmMode addressSize) <$> modrm)

      -- do we need to read a displacement?
      dispSize = join (useDisplacement addressSize <$> modrm)


   -- read SIB byte if necessary
   sib <- if hasSIB
      then (Just . SIB) <$> getWord8
      else return Nothing

   -- read displacement if necessary
   disp <- forM dispSize getSize


   let
      -- we determine the effective operand size. It depends on:
      --   * the mode of execution
      --   * the presence of the 0x66 legacy prefix
      --   * the default operand size of the instruction in 64-bit mode
      --   * the value of the ForceNo8bit bit in the opcode (if applicable)
      --   * the value of REX.W/VEX.W/XOP.W (if applicable)
      operandSize = undefined

      -- do we need to read an immediate?
      immSize = undefined :: Maybe Size
      -- TODO


   imm <- forM immSize getSize

   undefined
-- 
--    asize <- mGet
--    
--    let
--       ps = encParams enc
--       es = fmap opEnc ps
-- 
--    -- read a memory address if necessary
-- 
--    let 
--        getAddr asize m = do
--           case asize of
--              -- if we are in 16-bit addressing mode, we don't care about the base
--              -- register extension
--              AddrSize16 -> case (modField m, rmField modrm) of
--                 (_,0) -> Addr (Just R_BX) (Just R_SI) disp Nothing
--                 (_,1) -> Addr (Just R_BX) (Just R_DI) disp Nothing
--                 (_,2) -> Addr (Just R_BP) (Just R_SI) disp Nothing
--                 (_,3) -> Addr (Just R_BP) (Just R_DI) disp Nothing
--                 (_,4) -> Addr (Just R_SI) Nothing     disp Nothing
--                 (_,5) -> Addr (Just R_DI) Nothing     disp Nothing
--                 (0,6) -> Addr Nothing     Nothing     disp Nothing
--                 (_,6) -> Addr (Just R_BP) Nothing     disp Nothing
--                 (_,7) -> Addr (Just R_BX) Nothing     disp Nothing
--                 _     -> error "Invalid 16-bit addressing"
--        
-- 
-- 
--    addr <- case modrm of
--       Just m' | not (rmRegMode m') -> Just <$> getAddr m'
--       _                            -> return Nothing
-- 
--    -- read an immediate if necessary
--    imm <- case filter isImmediate es of
--       []   -> return Nothing
--       [im] -> do
--          case (opEnc im, opType im) of
--             (E_Imm8_3_0, T_Mask)     -> Just . OpMask . SizedValue8 <$> nextWord8
--             (E_Imm8_7_4, T_V128_256) -> Just . OpRegId . (`shiftR` 4) <$> nextWord8
--             (E_Imm, T_Imm8)          -> Just . OpImmediate . SizedValue8 <$> nextWord8
--             (E_Imm, T_PTR_16_16)     -> Just <$> (OpPtr16_16 <$> nextWord16 <*> nextWord16)
--             (E_Imm, T_PTR_16_32)     -> Just <$> (OpPtr16_32 <$> nextWord16 <*> nextWord32)
--             (E_Imm, T_REL_16_32) -> case opSize of
--                OpSize8  -> error "Invalid operand size"
--                OpSize16 -> Just . OpRel . SizedValue16 <$> nextWord16
--                OpSize32 -> Just . OpRel . SizedValue32 <$> nextWord32
--                OpSize64 -> Just . OpRel . SizedValue32 <$> nextWord32
--             (E_Imm, T_Imm) -> case (opSize, encSignExtendImmBit enc) of
--                (OpSize8, _)
--                   -> Just . OpImmediate . SizedValue8 <$> nextWord8
--                (_, Just se) | testBit opcode se
--                   -> Just . OpSignExtendImmediate . SizedValue8 <$> nextWord8
--                (OpSize16,_)
--                   -> Just . OpImmediate . SizedValue16 <$> nextWord16
--                (OpSize32,_)
--                   -> Just . OpImmediate . SizedValue32 <$> nextWord32
--                (OpSize64,_)
--                   -> Just . OpSignExtendImmediate . SizedValue32 <$> nextWord32
-- 
--             _  -> error $ "Don't know how to read immediate operand: " ++ show im
--       _    -> error "Invalid encoding (more than one immediate operand)"
-- 
--    -- match what we have read with required parameters
--    ops <- forM (encOperands enc) $ \op -> do
--       let doImm = case imm of
--             Just (OpRegId rid) -> getOpFromRegId opSize (opType op) rid
--             Just o  -> return o
--             Nothing -> error "Immediate operand expected, but nothing found"
-- 
--       case opEnc op of
--          E_Imm      -> doImm
--          E_Imm8_3_0 -> doImm
--          E_Imm8_7_4 -> doImm
--          E_ModRM -> case (modrm, rmAddr) of
--             (_, Just addr) -> return (OpMem addr)
--             (Just m, _)    -> getRMOp opSize (opType op) m
--             (Nothing,_)    -> error "ModRM expected, but nothing found"
--          E_ModReg -> case modrm of
--             Just m  -> getRegOp opSize (opType op) m
--             Nothing -> error "ModRM expected, but nothing found"
--          E_Implicit -> getImplicitOp opSize (opType op)
--          E_VexV     -> gets decStateAdditionalOp >>= \case
--             Just vvvv -> getOpFromRegId opSize (opType op) vvvv
--             Nothing   -> error "Expecting additional operand (VEX.vvvv)"
--          E_OpReg    -> getOpFromRegId opSize (opType op) (opcode .&. 0x07)
-- 
--    -- reverse operands if reversable bit is set
--    let ops' = case encReversableBit enc of
--          Just b | testBit opcode b -> reverse ops
--          _                         -> ops
-- 
--    return ops'
-- 
--    -- reverse FPU operands if necessary (FPU dest bit set)
--    -- TODO
-- 
--    undefined



-- | Extended ModRM.reg (with REX.R, VEX.R, etc.)
getExtReg :: Opcode -> ModRM -> Word8
getExtReg oc m = do
   let
      f x = x `unsafeShiftL` 3 .|. regField m
      g x = if x then f 1 else f 0

   case oc of
      OpVex v _                 -> g (vexR v)
      OpXop v _                 -> g (vexR v)
      OpLegacy _ (Just rex) _ _ -> f (rexR rex)
      OpLegacy _ Nothing    _ _ -> regField m
      
-- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
getExtRM :: Opcode -> ModRM -> Word8
getExtRM oc m = do
   let
      f x = x `unsafeShiftL` 3 .|. rmField m
      g x = if x then f 1 else f 0
      h x = g (fromMaybe False x)

   case oc of
      OpVex v _                 -> h (vexB v)
      OpXop v _                 -> h (vexB v)
      OpLegacy _ (Just rex) _ _ -> f (rexB rex)
      OpLegacy _ Nothing    _ _ -> rmField m


data VectorLength
   = VL128
   | VL256
   deriving (Show,Eq)

-- | Get vector length (stored in VEX.L, XOP.L, etc.)
getVectorLength :: Opcode -> Maybe VectorLength
getVectorLength = \case
   OpVex v _ -> Just $ if vexL v
      then VL256
      else VL128
   OpXop v _ -> Just $ if vexL v
      then VL256
      else VL128
   _         -> Nothing

-- | Get the opcode map
getOpcodeMap :: Opcode -> OpcodeMap
getOpcodeMap = \case
   OpLegacy _ _ t _ -> MapLegacy t
   OpVex  v    _    -> vexMapSelect v
   OpXop  v    _    -> vexMapSelect v

-- | Get the opcode byte
getOpcode :: Opcode -> Word8
getOpcode = \case
   OpLegacy _ _ _ x -> x
   OpVex        _ x -> x
   OpXop        _ x -> x
