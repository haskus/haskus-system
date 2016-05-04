{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.X86_64.Assembler.New
   (
   )
where

import ViperVM.Arch.X86_64.Assembler.Opcode
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.Size

import ViperVM.Utils.MultiState
import ViperVM.Utils.HArray
import ViperVM.Format.Binary.Reader
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.BitField

import Data.List (nub)
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Identity

-- ===========================================================================
-- X86 Instruction
-- ===========================================================================

---------------------------------------------------------------------------
-- Instruction length
-- ~~~~~~~~~~~~~~~~~~
-- 
-- An instruction is at most 15 bytes long.
---------------------------------------------------------------------------

-- | Prepare a buffer for instruction reading
prepareBuffer :: Buffer -> Buffer
prepareBuffer = bufferTakeAtMost 15

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
--    2) modify the instruction's operand size
--    3) modify the instruction's address segment
--    4) be used as an opcode extension
--    5) provide atomic bus locking
--    6) repeat the instruction until a condition is met
--
-- The operand size and the default address size are not necessarily the
-- same: the pointed address-sized memory is sign-extended or zero-extended
-- to have the operand size.
--
-- Legacy prefixes that are used as opcode extensions are mandatory...
---------------------------------------------------------------------------

-- | Read legacy prefixes (up to 5)
readLegacyPrefixes :: 
   (ReaderM () s
   ) => MState s LegacyPrefixes
readLegacyPrefixes = do
   ws <- forM [0..4] $ \(_ :: Int) ->
      binTryReadIf isLegacyPrefix

   return $ LegacyPrefixes (checkLegacyPrefixes (catMaybes ws))

---------------------------------------------------------------------------
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
---------------------------------------------------------------------------

-- | Check that legacy prefixes belong to different groups
checkLegacyPrefixes :: [Word8] -> [Word8]
checkLegacyPrefixes ps = if test
      then ps
      else error "checkLegacyPrefixes: instruction using several prefixes from the same group"

   where
      prefixGroup x = case x of
         0x66 -> 1
         0x67 -> 2
         0x2E -> 3
         0x3E -> 3
         0x26 -> 3
         0x64 -> 3
         0x65 -> 3
         0x36 -> 3
         0xF0 -> 4
         0xF3 -> 5
         0xF2 -> 5
         _    -> error "checkLegacyPrefixes: invalid legacy prefix"

      test  = length ps /= length (nub (map prefixGroup ps))
   

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
readRexPrefix ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Rex)
readRexPrefix = do
   
   mode <- mGet

   -- REX is only supported in 64-bit mode
   if is64bitMode mode
      then fmap Rex <$> binTryReadIf isRexPrefix
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
---------------------------------------------------------------------------

-- | Read legacy opcode
readLegacyOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readLegacyOpcode = do
   ps  <- readLegacyPrefixes
   rex <- readRexPrefix

   let
      -- TODO: use mode or sets...
      is3DNowAllowed    = True
      ret m x = return (Just (OpLegacy ps rex m x))

   binWith $ \case
      0x0F -> binWith $ \case
         -- the real 3DNow! opcode is stored in the last byte and
         -- will be set later
         0x0F | is3DNowAllowed -> ret Map3DNow 0
         0x3A                  -> binWith (ret Map0F3A)
         0x38                  -> binWith (ret Map0F38)
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
---------------------------------------------------------------------------

-- | Read VEX/XOP encoded opcode
readVexXopOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readVexXopOpcode = do
   mode <- mGet

   let
      -- TODO: use mode or sets...
      isXOPAllowed = True
      isVEXAllowed = True

      -- VEX prefixes are supported in 32-bit and 16-bit modes
      -- They overload LES and LDS opcodes so that the first two bits
      -- of what would be ModRM are invalid (11b) for LES/LDS
      testMod :: Word8 -> Bool
      testMod w    = w `unsafeShiftR` 6 == 0x03
      isVexMode    = if is64bitMode mode
                        then return True
                        else binTryPeek >>= \case
                              Just w | testMod w -> return True
                              _                  -> return False
                        
   binWith $ \case
      0x8F  |  isXOPAllowed ->
                  binWith $ \a ->
                     binWith $ \b ->
                        binWith $ \c ->
                           return (Just (OpXop (Vex3 a b) c))

      0xC4  |  isVEXAllowed -> isVexMode >>= \case
                  False -> return Nothing
                  True  ->
                     binWith $ \a ->
                        binWith $ \b ->
                           binWith $ \c ->
                              return (Just (OpVex (Vex3 a b) c))

      0xC5  |  isVEXAllowed -> isVexMode >>= \case
                  False -> return Nothing
                  True  ->
                     binWith $ \a ->
                        binWith $ \b ->
                           return (Just (OpVex (Vex2 a) b))

      (_ :: Word8) -> return Nothing

      
-- ===========================================================================
-- Generic opcode reading
-- ===========================================================================


-- | Read the opcode encoding
readOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readOpcode = firstJust
      -- check for overloaded prefixes/opcodes first!
      [ readVexXopOpcode
      , readLegacyOpcode
      ]
   where
      -- return the first returned Just
      firstJust :: [MState s (Maybe a)] -> MState s (Maybe a)
      firstJust []     = return Nothing
      firstJust (x:xs) = do
         r <- x
         case r of
            Just _  -> return r
            Nothing -> firstJust xs


-- ===========================================================================
-- Identify the instruction
-- ===========================================================================
-- identifyInsn ::
--    ( ReaderM () s
--    , HArrayIndexT X86Mode s
--    ) => Opcode -> MState s (Maybe Insn)
-- identifyInsn oc = do
-- 
--    -- get the opcode table
--    table <- case getOpcodeMap oc of
--       MapLegacy
--       --TODO
--
--    -- handle 3DNow!
--    -- TODO
-- 
--    -- get the candidate instructions for the opcode
--    -- TODO
--    -- cs <-
-- 
--    -- if there is none, return an error
--    -- TODO
--
--    -- check for required prefixes
--    -- TODO
-- 
--    -- determine if we need to read the next byte
--    -- TODO
-- 
--    -- check that we can read the next byte
--    -- if we can read the next byte, do it
--    rem <- binRemaining
--    if rem == 0
--       then -- return an error
--       else -- 
-- 
--    m <- binPeek
-- 
--    -- filter out invalid full extension (extension in the whole second byte)
--    -- TODO
-- 
--    -- filter out invalid ModRM.reg extension
--    -- TODO
-- 
--    -- filter out invalid ModRM.mod (e.g., only 11b)
--    -- TODO
-- 
--    -- Filter out invalid enabled extensions/architecture. Return sensible error
--    -- if no instruction left (e.g., in order to provide suggestion to enable an
--    -- extension).
--    -- TODO
--    
--    -- If there are more than on instruction left, signal a bug
--    -- TODO
-- 
--    -- Return the instruction
--    -- TODO


-- ===========================================================================
-- Operands
-- ===========================================================================

----------------------------------------
-- Read operands
----------------------------------------


-- | Read instruction operands
-- readOperands ::
--    ( ReaderM () s
--    , HArrayIndexT X86Mode s
--    , HArrayIndexT AddressSize s
--    ) => Opcode -> Encoding -> MState s [Operand]
-- readOperands oc enc = do
-- 
--    mode  <- mGet
--    asize <- mGet
--    
--    let
--       ps = encParams enc
--       es = fmap opEnc ps
-- 
--    -- read ModRM if necessary
--    m <- if encRequireModRM enc
--             then binTryRead
--             else return Nothing
-- 
--    let modrm = ModRM . BitFields <$> m
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
getExtReg ::
   ( HArrayIndexT Opcode s
   ) => ModRM -> MState s Word8
getExtReg m = do
   let
      f x = x `unsafeShiftL` 3 .|. regField m
      g x = if x then f 1 else f 0

   mGet >>= \case
      OpVex v _                 -> return $ g (vexR v)
      OpXop v _                 -> return $ g (vexR v)
      OpLegacy _ (Just rex) _ _ -> return $ f (rexR rex)
      OpLegacy _ Nothing    _ _ -> return $ regField m
      
-- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
getExtRM ::
   ( HArrayIndexT Opcode s
   ) => ModRM -> MState s Word8
getExtRM m = do
   let
      f x = x `unsafeShiftL` 3 .|. rmField m
      g x = if x then f 1 else f 0
      h x = g (fromMaybe False x)

   mGet >>= \case
      OpVex v _                 -> return $ h (vexB v)
      OpXop v _                 -> return $ h (vexB v)
      OpLegacy _ (Just rex) _ _ -> return $ f (rexB rex)
      OpLegacy _ Nothing    _ _ -> return $ rmField m


data VectorLength
   = VL128
   | VL256
   deriving (Show,Eq)

-- | Get vector length (stored in VEX.L, XOP.L, etc.)
getVectorLength ::
   ( HArrayIndexT Opcode s
   ) => MState s (Maybe VectorLength)
getVectorLength = do
   op <- mGet
   case op of
      OpVex v _ -> return . Just $ if vexL v
         then VL256
         else VL128
      OpXop v _ -> return . Just $ if vexL v
         then VL256
         else VL128
      _         -> return Nothing

-- | Get the opcode map
getOpcodeMap :: Opcode -> OpcodeMap
getOpcodeMap = \case
   OpLegacy _ _ t _ -> MapLegacy t
   OpVex  v    _    -> vexMapSelect v
   OpXop  v    _    -> vexMapSelect v


getAddr :: ModRM -> MState s Addr
getAddr m = undefined

