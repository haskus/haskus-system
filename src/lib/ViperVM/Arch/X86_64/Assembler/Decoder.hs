{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ViperVM.Arch.X86_64.Assembler.Decoder
   ( Instruction(..)
   , decode
   , decodeMany
   ) where

import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Data.List (nub)
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either


import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
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
         , stateSegAddrSize         = defAddrSize
         , stateSegOperandSize      = defOprndSize
         , stateByteCount           = 0
         }

{-
   Note [Legacy prefixes]
   ~~~~~~~~~~~~~~~~~~~~~~

   There are up to 4 optional legacy prefixes at the beginning of an
   instruction. These prefixes can be in any order. 

   These prefixes are divided up into 4 groups: only a single prefix from each
   group must be used otherwise the behavior is undefined. It seems that some
   processors ignore the subsequent prefixes from the same group or use only
   the last prefix specified for any group.

   Legacy prefixes alter the meaning of the opcode. Originally it was only in
   minor ways (address or operand size, with bus locking, branch hints, etc.).
   Now, these prefixes are used to select totally different sets of
   instructions. In this case the prefix is said to be mandatory (for the
   instruction to be encoded). For example, SSE instructions require a legacy
   prefix.

-}

-- | Identify if the byte is a legacy prefix
isLegacyPrefix :: Word8 -> Bool
isLegacyPrefix x = x `elem` [0x66,0x67,0x2E,0x3E,0x26,0x64,0x65,0x36,0xF0,0xF3,0xF2]

-- | Get the legacy prefix group
legacyPrefixGroup :: Word8 -> Int
legacyPrefixGroup x = case x of
   -- group 1
   0xF0  -> 1
   0xF3  -> 1
   0xF2  -> 1
   -- group 2
   0x26  -> 2
   0x2E  -> 2
   0x36  -> 2
   0x3E  -> 2
   0x64  -> 2
   0x65  -> 2
   -- group 3
   0x66  -> 3
   -- group 4
   0x67  -> 4
   _     -> error "Not a legacy prefix"

-- | Decode legacy prefixes
decodeLegacyPrefixes :: X86Dec [Word8]
decodeLegacyPrefixes = rec 0 [] 
   where
      rec :: Int -> [Word8] -> X86Dec [Word8]
      rec n xs = do
         x <- lookWord8

         if not (isLegacyPrefix x)
            then do
               -- We check that there is a single prefix per group at most
               let groups = fmap legacyPrefixGroup xs
               if length groups == length (nub groups)
                  then right $ reverse xs
                  else left ErrInvalidLegacyPrefixGroups
            else do
               -- Skip prefix byte
               skipWord8
               -- We check that there is less than 4 prefixes
               if n + 1 > 4
                  then left ErrTooManyLegacyPrefixes
                  else rec (n+1) (x:xs)


         
{-
 Note [REX prefix]
 ~~~~~~~~~~~~~~~~~

 With the 64bit extension of the X86 ISA, a new prefix has been introduced.
 It is only available in Long mode and can be used to select the additional
 registers and use 64 bit operands. It overrides the one-byte alterinative
 encoding of INC and DEC in X86-32 ABI, hence these encodings must not be
 used in Long mode.

 The REX prefix is an optional single byte after the legacy prefixes (if
 any). If more than one REX prefix is present, the behavior is undefined
 (however it seems that the last one is used).

 The presence of the REX prefix disallows the use of the *H registers
 (AH,BH,etc.). Instead their codes encode new registers.

 There are 4 fields in the REX prefix:
    W: force 64 bit operand size if set
    R: 1-bit extension to the ModRM.reg field (see below)
    X: 1-bit extension to the SIB.index field (see below)
    B: 1-bit extension to the ModRM.rm field (see below)
-}

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

   


{- Note [FPU (x87)]
   ~~~~~~~~~~~~~~~~

 x87 floating-point instructions are encoded with two bytes. The first 5 bits
 are fixed: 11011 xxx xxxxxxxx

 The second byte is a kind of ModRM but which can be used differently to
 extend the first opcode byte. 
    
-}



---- ------------------------------------------------------------------
----                         VEX PREFIX
---- ------------------------------------------------------------------
--
---- | Decode a Vex prefix
--decodePrefixVex :: X86Dec ()
--decodePrefixVex = do
--
--   let
--      smallCheck = (== 0xC4) . (.&. 0xFE) <$> lookaheadByte
--      -- VEX prefixes are supported in 32-bit mode
--      -- They overload LES and LDS opcodes so that the first two bits
--      -- of what would be ModRM are invalid (11b) for LES/LDS
--      fullCheck  = smallCheck <&&> ((== 0xC0) . (.&. 0xC0) . snd <$> lookaheadWord16)
--
--   hasVex <- (test64BitMode <&&> smallCheck) <||> (test32BitMode <&&> fullCheck)
--   
--   when hasVex $ do
--
--      --  Check that there is no invalid prefix before the VEX one
--      let checkPrefix p = mayFail p =<< hasRealLegacyPrefix p 
--          mayFail p x  = if x
--            then error ("Invalid prefix " ++ show p ++ " before VEX prefix")
--            else return ()
--      
--      traverse_ checkPrefix 
--         [ PrefixLock
--         , PrefixOperandSizeOverride
--         , PrefixRepeat RepeatEqual
--         , PrefixRepeat RepeatNotEqual
--         ]
--      mayFail "PrefixRex" =<< hasRexPrefix
--
--      -- Read bytes
--      v <- nextByte >>= \case
--         0xC4 -> Vex2 <$> nextByte
--         0xC5 -> Vex3 <$> nextByte <*> nextByte
--
--      -- store VEX prefix
--      modify (\y -> y { stateVexPrefix = Just v })
--
--
---- | Test if a VEX prefix is present
--hasVexPrefix :: X86Dec Bool
--hasVexPrefix = isJust . stateVexPrefix <$> get
--
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
-----------------------------------------------------------------------
---- OPCODES
-----------------------------------------------------------------------
--
--decodeOpcode :: X86Dec ()
--decodeOpcode = do
--
--   op <- nextByte >>= \case
--      0x0F -> nextByte >>= \case
--         0x0F -> return [0x0F,0x0F]  -- 3D Now!
--         0x38 -> (([0x0F,0x38] ++) . return) <$> nextByte
--         0x3A -> (([0x0F,0x3A] ++) . return) <$> nextByte
--         x    -> return [0x0F,x]
--      x    -> return [x]
--
--   -- Test that there isn't more than one opcode byte with VEX/EVEX
--   when (length op /= 1) $
--      whenM (hasEvexPrefix <||> hasVexPrefix) $
--         error "Escaped opcodes are not allowed with VEX prefix"
--
--   modify (\y -> y { stateOpcode = op })
--   
--
-----------------------------------------------------------------------
---- IMMEDIATE
-----------------------------------------------------------------------
--
---- | Decode immediate if it is required by any operand
--decodeImmediate :: X86Dec ()
--decodeImmediate = do
--   insn <- fromJust . stateInsnDesc <$> get
--
--   let ops = insnOperands insn
--
--   -- Check if immediate is required and read it
--   let 
--      computeImmSize x = case x of
--         Enc_Imm     -> computeImmediateSize
--         Enc_FImm s  -> return $ Just s
--         Enc_RImm    -> return $ Just Size8
--         _           -> return Nothing
--
--      allEq []  = Nothing
--      allEq [x] = Just x
--      allEq (y:x:xs) = if x == y
--         then allEq (x:xs)
--         else error "Two immediate operands with different sizes required"
--
--   immSize <- allEq . catMaybes <$> traverse computeImmSize ops
--
--   case immSize of
--      Just s  -> getImmediate s
--      Nothing -> return ()
--
--
---- | Decode an immediate value of the given size
--getImmediate :: Size -> X86Dec ()
--getImmediate sz = whenM (isNothing . stateImmediate <$> get) $ do
--   i <- getSizedValue sz
--   modify (\y -> y { stateImmediate = Just i })
--
-----------------------------------------------------------------------
---- OPERANDS
-----------------------------------------------------------------------
--
---- | Decode operands (the whole instructions should have been read now)
--decodeOperands :: X86Dec ()
--decodeOperands = do
--   encs <- insnOperands . fromJust . stateInsnDesc <$> get
--   ops <- traverse decodeOperand encs
--   modify (\y -> y { stateOperands = ops})
--
---- | Decode one operand
--decodeOperand :: OpEnc -> X86Dec Operand
--decodeOperand enc = do
--
--   case enc of
--      Enc_Acc -> getEffectiveOperandSize >>= \case
--         OpSize8  -> return $ OpReg R_AL
--         OpSize16 -> return $ OpReg R_AX
--         OpSize32 -> return $ OpReg R_EAX
--         OpSize64 -> return $ OpReg R_RAX
--
--      Enc_Imm     -> OpImm . fromJust . stateImmediate <$> get
--
--      Enc_FImm _  -> OpImm . fromJust . stateImmediate <$> get
--
--      Enc_Reg     -> OpReg <$> (decodeReg =<< getRegField')
--
--
--decodeReg :: Word8 -> X86Dec Register
--decodeReg code = do
--   hasRex <- hasRexPrefix
--   sz <- getEffectiveOperandSize'
--   return (regFromCode RF_GPR (Just sz) hasRex code)
--
--getRegField' :: X86Dec Word8
--getRegField' = regField . fromJust . stateModRM <$> get
--
---------------------------------------------
---- State management functions
---------------------------------------------
--
---- | Compute immediate size given set flags and mode
----
---- This is a generic function, some instructions use fixed immediate sizes
--computeImmediateSize :: X86Dec (Maybe Size)
--computeImmediateSize = do
--   -- If sign-extended bit is set, immediate is byte sign-extended to full size
--   -- Otherwise, if "w" bit is cleared, operand size is byte
--   -- Otherwise, if "w" bit is set, full size is enabled, that is 16 bit or 32 bit
--   -- 64-bit mode uses 32-bit immediate for full size
--   s <- isSignExtended
--   w <- hasSizeBit
--
--   case (s,w) of
--      (Just True, Just True)  -> error "Instruction with both size and sign-extend bits set"
--      (Just True, _)          -> return $ Just Size8
--      (_, Just False)         -> return $ Just Size8
--      (_, _)                  -> do -- full size
--         opSize <- getEffectiveOperandSize
--         return . Just $ case opSize of
--            OpSize8    -> Size8    -- I don't think it can happen
--            OpSize16   -> Size16
--            OpSize32   -> Size32
--            OpSize64   -> Size32   -- 32-bit immediate sign-extended to 64-bit in 64-bit mode
--
--
---- | Indicate if sign-extended bit is set (if it exists)
--isSignExtended :: X86Dec (Maybe Bool)
--isSignExtended = do
--   let f x = case x of
--         F_SignExtendBit b -> Just b
--         _                 -> Nothing
--
--   bits <- mapMaybe f . insnFlags . fromJust . stateInsnDesc <$> get
--   op <- last . stateOpcode <$> get
--
--   return $ case bits of
--      []  -> Nothing
--      [b] -> Just (testBit op b)
--      _   -> error "More than 1 sign-extend bit for instruction found"
--
--   
---- | Indicate if "w" bit is set (if it exists)
--hasSizeBit :: X86Dec (Maybe Bool)
--hasSizeBit = do
--   let f x = case x of
--         F_SizeBit b -> Just b
--         _           -> Nothing
--
--   bits <- mapMaybe f . insnFlags . fromJust . stateInsnDesc <$> get
--   op <- last . stateOpcode <$> get
--
--   return $ case bits of
--      []  -> Nothing
--      [b] -> Just (testBit op b)
--      _   -> error "More than 1 size bit for instruction found"
--
---- | Get effective address size
----
---- Instructions that have fixed address sizes are not considered here
--getEffectiveAddressSize :: X86Dec AddressSize
--getEffectiveAddressSize = do
--   s <- get
--   return $ effectiveAddressSize (stateMode s) (stateLegacyPrefixes s) (stateSegAddrSize s)
--
---- | Get effective operand size
----
---- Instructions that have fixed operand sizes are not considered here
--getEffectiveOperandSize :: X86Dec OperandSize
--getEffectiveOperandSize = do
--   s <- get
--   let rex = stateRexPrefix s
--   return $ effectiveOperandSize (stateMode s) (stateLegacyPrefixes s) rex (stateSegOperandSize s)
--
--getEffectiveOperandSize' :: X86Dec Size
--getEffectiveOperandSize' = operandSize <$> getEffectiveOperandSize 
--
--getSizedValue :: Size -> X86Dec SizedValue
--getSizedValue sz = case sz of
--   Size8  -> SizedValue8  <$> nextByte
--   Size16 -> SizedValue16 <$> nextWord16le
--   Size32 -> SizedValue32 <$> nextWord32le
--   Size64 -> SizedValue64 <$> nextWord64le


getEffectiveAddressSize :: [Word8] -> X86Dec AddressSize
getEffectiveAddressSize prefixes = do
   mode        <- getMode
   asize       <- getAddressSize
   return (effectiveAddressSize mode (fmap toLegacyPrefix prefixes) asize)


decodeInsn :: X86Dec Instruction
decodeInsn = do
   allowedSets <- getAllowedSets
   mode        <- getMode

   -- Decode legacy prefixes. See Note [Legacy prefixes].
   prefixes <- decodeLegacyPrefixes

   -- compute the effective address size (i.e. some legacy prefixes can change
   -- the current address size)
   easize <- getEffectiveAddressSize prefixes

   -- Try to decode a REX prefix. See Note [REX prefix]
   rex <- if not (is64bitMode mode)
      then right Nothing
      else lookWord8 >>= \x -> do
         case (x .&. 0xF0) of
            0x40 -> do
               skipWord8
               right (Just (Rex x))
            _    -> right Nothing

   -- Decode legacy opcode. See Note [Legacy opcodes]
   opcode <- nextWord8 >>= \case
      -- escaped opcode
      0x0F -> do
         nextWord8 >>= \case
            0x0F | Set3DNow `elem` allowedSets -> right [0x0F,0x0F]
            0x38 -> nextWord8 >>= \y -> right [0x0F,0x38,y]
            0x3A -> nextWord8 >>= \y -> right [0x0F,0x3A,y]
            0x01 -> nextWord8 >>= \y -> right [0x0F,0x01,y]
            y    -> right [0x0F,y]
      -- Decode unescaped opcode
      y    -> right [y]


   -- addressing params (if an operand accesses memory)
   let aparams = case rex of
         Nothing -> AddrParams
            { addrParamBaseExt         = 0
            , addrParamIndexExt        = 0
            , addrParamUseExtRegisters = False
            , addrParamAddressSize     = easize
            }
         Just x  -> AddrParams
            { addrParamBaseExt         = rexB x
            , addrParamIndexExt        = rexX x
            , addrParamUseExtRegisters = True
            , addrParamAddressSize     = if rexW x 
                  then AddrSize64
                  else easize
            }

   case opcode of
      -- X87 instructions
      [x] |  SetX87 `elem` allowedSets 
          && x .&. 0xF8 == 0xD8        -> fmap InsnX87 $ getX87 aparams x


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
