{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ViperVM.Arch.X86_64.Assembler.Decoder
   ( decode
   , decodeMany
   , InstructionSet(..)
   , DecodeError(..)
   , Instruction(..)
   ) where

import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Data.List (nub)
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
--import Control.Conditional ((<&&>), (<||>), whenM)
import Control.Monad.State
import Control.Monad.Trans.Either


import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.ModRM

data Instruction
   = InsnX87 X87Instruction
   deriving (Show,Eq)

data DecodeError
   = ErrInsnTooLong                 -- ^ Instruction is more than 15 bytes long
   | ErrTooManyLegacyPrefixes       -- ^ More than 4 legacy prefixes
   | ErrInvalidLegacyPrefixGroups   -- ^ More than 1 legacy prefix for a single group
   | ErrUnknownOpcode [Word8]       -- ^ Unrecognized opcode
   deriving (Show,Eq)

type X86DecStateT = StateT X86State Get
type X86Dec a     = EitherT DecodeError X86DecStateT a

data InstructionSet
   = SetVEX
   | SetEVEX
   | SetXOP
   | Set3DNow
   | SetSMAP
   | SetX87
   deriving (Show,Eq)


-- The decoder is an automaton whose state is represented by X86State
data X86State = X86State
   { stateMode             :: X86Mode            -- ^ Architecture execution mode
   , stateSets             :: [InstructionSet]   -- ^ Available instruction sets

   , stateSegAddrSize      :: AddressSize        -- ^ Segment address size
   , stateSegOperandSize   :: OperandSize        -- ^ Segment operand size

   , stateByteCount        :: Int                -- ^ Number of bytes read (used to fail when > 15)
   }

-- | Get X86 Mode
getMode :: X86Dec X86Mode
getMode = stateMode <$> lift get

-- | Get allowed instruction sets
getAllowedSets :: X86Dec [InstructionSet]
getAllowedSets = stateSets <$> lift get

-- | Get current address size
getAddressSize :: X86Dec AddressSize
getAddressSize = stateSegAddrSize <$> lift get

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

-------------------------------------------------------------------------------
-- INSTRUCTION SIZE
--
-- A x86 instruction may be at most 15 bytes in length. We define the following
-- functions to ensure that when we read a byte, it is accounted. If we read
-- more than 15 bytes, we fail with the appropriate error

-- | Read some bytes
next :: Int -> Get a -> X86Dec a
next n getter = do
   -- Test that we don't read more than 15 bytes
   bc <- stateByteCount <$> lift get
   if bc + n > 15
      then left ErrInsnTooLong
      else do
         -- try to do the actual read
         w <- lift (lift getter)
         -- store the new number of bytes
         lift $ modify (\y -> y {stateByteCount = bc +n})
         right w

lookAhead :: Int -> Get a -> X86Dec a
lookAhead n getter = do
   -- Test that we don't read more than 15 bytes
   bc <- stateByteCount <$> lift get
   if bc + n > 15
      then left ErrInsnTooLong
      else lift (lift (G.lookAhead getter))

-- | Read 1 byte
nextWord8 :: X86Dec Word8
nextWord8 = next 1 G.getWord8

-- | Read 2 bytes
nextWord16 :: X86Dec Word16
nextWord16 = next 2 G.getWord16le

-- | Read 4 bytes
nextWord32 :: X86Dec Word32
nextWord32 = next 4 G.getWord32le

-- | Read 8 bytes
nextWord64 :: X86Dec Word64
nextWord64 = next 8 G.getWord64le

-- | Read 1 byte
lookWord8 :: X86Dec Word8
lookWord8 = lookAhead 1 G.getWord8

-- | Read 2 bytes
lookWord16 :: X86Dec Word16
lookWord16 = lookAhead 2 G.getWord16le

-- | Read 4 bytes
lookWord32 :: X86Dec Word32
lookWord32 = lookAhead 4 G.getWord32le

-- | Read 8 bytes
lookWord64 :: X86Dec Word64
lookWord64 = lookAhead 8 G.getWord64le

-- | Read 1 byte
skipWord8 :: X86Dec ()
skipWord8 = void nextWord8

-- | Read 2 bytes
skipWord16 :: X86Dec ()
skipWord16 = void nextWord16

-- | Read 4 bytes
skipWord32 :: X86Dec ()
skipWord32 = void nextWord32

-- | Read 8 bytes
skipWord64 :: X86Dec ()
skipWord64 = void nextWord64

-- | Get operand from a ModRM
getOperand :: AddressSize -> ModRM -> X86Dec Op
getOperand asize m = do
   let
      getDisp = case useDisplacement asize m of
         Nothing      -> return 0
         Just Size8   -> fromIntegral <$> nextWord8
         Just Size16  -> fromIntegral <$> nextWord16
         Just Size32  ->                  nextWord32
         Just s       -> error $ "Invalid displacement size: " ++ show s


   case rmMode asize m of
      RMRegister  -> right . OpReg $ rmField m
      RMBaseIndex -> OpAddr . AddrBaseDisp . BaseDisp (rmField m) <$> getDisp
      RMSIB       -> OpAddr . AddrScaleBaseIndex <$> do
         sib <- SIB <$> nextWord8
         disp <- getDisp
         return (ScaleIndexBase (baseField sib) (indexField sib) (scaleField sib) disp)

getAddr :: AddressSize -> ModRM -> X86Dec Addr
getAddr asize m = do
   op <- getOperand asize m
   case op of
      OpAddr a -> right a
      OpReg _  -> error "Not an address operand"

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
    
    Variation 1: 11011 0x1 111xxxxx : 6-bit opcode extension

    Variation 2: 11011 dp/mf 0 mod CC r/p r/m
       - CC (2bits): opcode extension 
       - r/p (1bit): case CC of
             00 -> r/p is opcode extension. 
             01 -> r/p is pop bit
             _  -> r/p reverses the operand order
       - mod (2bits): usual mod
       - r/m (3bits): usual r/m (registers are ST(i))
       - dp/mf: case mod of
          11 -> dp is direction bit (ST(0) or ST(i)) and pop bit
          _  -> mf is memory format
                   00 -> 32-bit real
                   10 -> 64-bit real
                   01 -> 32-bit integer
                   11 -> 16-bit integer
    3 others forms (TODO)
-}



-------------------------------------------------------------------------------
-- LEGACY OPERANDS (ModRM and SIB)
--
-- Depending on the decoded opcode and prefixes, we may need to decode some
-- operands. Legacy operands can be a combination of registers, memory address,
-- immediate value. Of course, not all the combinations are allowed.




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
---- | Try to identify the opcode from the given set of instructions
--identifyOpcode :: [InsnDesc] -> X86Dec ()
--identifyOpcode insns = do
--   op <- stateOpcode <$> get
--
--   -- Prepare instruction list
--   let 
--      masks = Map.fromListWith (++) $ fmap insnOpcodeMask insns `zip` fmap return insns
--      makeMap is = Map.fromListWith (++) $ fmap insnOpcode is `zip` fmap return is
--      masks' = Map.map makeMap masks
--   
--   -- Apply masks to opcode and find the matching one if any
--   let 
--      maskLast [] m     = []
--      maskLast [a] m    = [a .&. m]
--      maskLast (x:xs) m = x : maskLast xs m
--
--   let candidates = Map.foldMapWithKey (\mask is -> Map.findWithDefault [] (maskLast op mask) is) masks'
--
--   desc <- case candidates of
--      []  -> error "Cannot match instruction"
--      [i] -> return i
--      is -> do
--         -- filter candidates that use opcode extension in ModRM
--         let
--            isOpcodeExt (F_OpcodeExt _) = True
--            isOpcodeExt _               = False
--            opcodeExtFlag i = filter isOpcodeExt (insnFlags i)
--
--            isValid i = case opcodeExtFlag i of
--               [] -> return (Just i)
--               [F_OpcodeExt r] -> do
--                  requireModRM
--                  ext <- regField . fromJust . stateModRM <$> get
--                  if r == ext then return (Just i) else return Nothing
--               _               -> error $ "Instruction with several opcode extensions: " ++ show i
--
--         is' <- catMaybes <$> traverse isValid is
--
--         -- check that only one candidate left
--         case is' of
--            []  -> error "Cannot match any instruction"
--            [i] -> return i
--            _   -> error $ "Too many instruction candidates left: " ++ show is'
--
--   modify (\y -> y {stateInsnDesc = Just desc})
--            
--
--
--
-----------------------------------------------------------------------
---- MODRM, SIB, DISPLACEMENT
-----------------------------------------------------------------------
--   
---- | Decode ModRM byte, SIB byte and displacement
--requireModRM :: X86Dec ()
--requireModRM = whenM (isNothing . stateModRM <$> get) $ do
--
--   addrSize <- getEffectiveAddressSize
--
--   modrm <- ModRM <$> nextByte
--
--   sib <- if useSIB addrSize modrm
--      then Just . SIB <$> nextByte
--      else return Nothing
--
--   disp <- traverse getSizedValue (useDisplacement addrSize modrm)
--
--   s <- get
--   put $ s 
--      { stateModRM = Just modrm
--      , stateSIB   = sib
--      , stateDisplacement = disp
--      }
--
---- | Check if ModRM is required by the operands and decode it
--decodeModRM :: X86Dec ()
--decodeModRM = do
--   insn <- fromJust . stateInsnDesc <$> get
--
--   let ops = insnOperands insn
--
--   -- Check if ModRM is required and read it
--   let needModRM x = case x of
--         Enc_Reg  -> True
--         Enc_R128 -> True
--         Enc_RV   -> True
--         Enc_RM   -> True
--         Enc_RMV  -> True
--         _        -> False
--
--   when (any needModRM ops) requireModRM
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


decodeInsn :: X86Dec Instruction
decodeInsn = do
   allowedSets <- getAllowedSets
   mode <- getMode
   asize <- getAddressSize

   -- Decode legacy prefixes. See Note [legacy prefixes].
   prefixes <- decodeLegacyPrefixes

   let easize = effectiveAddressSize mode (fmap toLegacyPrefix prefixes) asize

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


   case opcode of
      -- X87 instructions
      [x] | SetX87 `elem` allowedSets && x .&. 0xF8 == 0xD8 -> do
         y <- lookWord8
         fmap InsnX87 $ case (x,y) of
            -- instructions without parameters
            (0xD9, 0xD0) -> skipWord8 >> right FNOP
            (0xD9, 0xE1) -> skipWord8 >> right FABS
            (0xD9, 0xE0) -> skipWord8 >> right FCHS
            (0xD9, 0xE5) -> skipWord8 >> right FXAM
            (0xD9, 0xE8) -> skipWord8 >> right FLD1
            (0xD9, 0xE9) -> skipWord8 >> right FLDL2T
            (0xD9, 0xEA) -> skipWord8 >> right FLDL2E
            (0xD9, 0xEB) -> skipWord8 >> right FLDPI
            (0xD9, 0xEC) -> skipWord8 >> right FLDLG2
            (0xD9, 0xED) -> skipWord8 >> right FLDLN2
            (0xD9, 0xEE) -> skipWord8 >> right FLDZ
            (0xD9, 0xF0) -> skipWord8 >> right F2XM1
            (0xD9, 0xF1) -> skipWord8 >> right FYL2X
            (0xD9, 0xF2) -> skipWord8 >> right FPTAN
            (0xD9, 0xF3) -> skipWord8 >> right FPATAN
            (0xD9, 0xF4) -> skipWord8 >> right FXTRACT
            (0xD9, 0xF5) -> skipWord8 >> right FPREM1
            (0xD9, 0xF6) -> skipWord8 >> right FDECSTP
            (0xD9, 0xF7) -> skipWord8 >> right FINCSTP
            (0xD9, 0xF8) -> skipWord8 >> right FPREM
            (0xD9, 0xF9) -> skipWord8 >> right FYL2XP1
            (0xD9, 0xFA) -> skipWord8 >> right FSQRT
            (0xD9, 0xFB) -> skipWord8 >> right FSINCOS
            (0xD9, 0xFC) -> skipWord8 >> right FRNDINT
            (0xD9, 0xFD) -> skipWord8 >> right FSCALE
            (0xD9, 0xFE) -> skipWord8 >> right FSIN
            (0xD9, 0xFF) -> skipWord8 >> right FCOS
            (0xDB, 0xE2) -> skipWord8 >> right FNCLEX
            (0xDA, 0xE9) -> skipWord8 >> right FUCOMPP
            (0xDD, 0xE4) -> skipWord8 >> right FTST
            (0xDE, 0xD9) -> skipWord8 >> right FCOMPP
            (0xDF, 0xE0) -> skipWord8 >> right FNSTSW_ax
            -- instructions with ModRM
            _ -> do
               let m = ModRM y
               case (x,regField m,rmRegMode m) of
                  (0xD8, 0, True ) -> skipWord8 >> FADD_st0_sti      . ST        <$> right (rmField m)
                  (0xD8, 0, False) -> skipWord8 >> FADD_m32          . M32FP     <$> getAddr asize m
                  (0xD9, 0, True ) -> skipWord8 >> FLD_st            . ST        <$> right (rmField m)
                  (0xD9, 0, False) -> skipWord8 >> FLD_m32           . M32FP     <$> getAddr asize m
                  (0xDA, 0, False) -> skipWord8 >> FIADD_m32         . M32INT    <$> getAddr asize m
                  (0xDB, 0, False) -> skipWord8 >> FILD_m32          . M32INT    <$> getAddr asize m
                  (0xDC, 0, True ) -> skipWord8 >> FADD_sti_st0      . ST        <$> right (rmField m)
                  (0xDC, 0, False) -> skipWord8 >> FADD_m64          . M64FP     <$> getAddr asize m
                  (0xDD, 0, False) -> skipWord8 >> FLD_m64           . M64FP     <$> getAddr asize m
                  (0xDD, 0, True ) -> skipWord8 >> FFREE             . ST        <$> right (rmField m)
                  (0xDE, 0, True ) -> skipWord8 >> FADDP_sti_st0     . ST        <$> right (rmField m)
                  (0xDE, 0, False) -> skipWord8 >> FIADD_m16         . M16INT    <$> getAddr asize m
                  (0xDF, 0, False) -> skipWord8 >> FILD_m16          . M16INT    <$> getAddr asize m

                  (0xD8, 1, False) -> skipWord8 >> FMUL_m32          . M32FP     <$> getAddr asize m
                  (0xD8, 1, True ) -> skipWord8 >> FMUL_st0_sti      . ST        <$> right (rmField m)
                  (0xD9, 1, True ) -> skipWord8 >> FXCH              . ST        <$> right (rmField m)
                  (0xDA, 1, False) -> skipWord8 >> FIMUL_m32         . M32INT    <$> getAddr asize m
                  (0xDC, 1, True ) -> skipWord8 >> FMUL_sti_st0      . ST        <$> right (rmField m)
                  (0xDC, 1, False) -> skipWord8 >> FMUL_m64          . M64FP     <$> getAddr asize m
                  (0xDE, 1, True ) -> skipWord8 >> FMULP_sti_st0     . ST        <$> right (rmField m)
                  (0xDE, 1, False) -> skipWord8 >> FIMUL_m16         . M16INT    <$> getAddr asize m

                  (0xDF, 2, False) -> skipWord8 >> FIST_m16          . M16INT    <$> getAddr asize m
                  (0xDB, 2, False) -> skipWord8 >> FIST_m32          . M32INT    <$> getAddr asize m
                  (0xD8, 2, True ) -> skipWord8 >> FCOM_st           . ST        <$> right (rmField m)
                  (0xD8, 2, False) -> skipWord8 >> FCOM_m32          . M32FP     <$> getAddr asize m
                  (0xD9, 2, False) -> skipWord8 >> FST_m32           . M32FP     <$> getAddr asize m
                  (0xDA, 2, False) -> skipWord8 >> FICOM_m32         . M32INT    <$> getAddr asize m
                  (0xDC, 2, False) -> skipWord8 >> FCOM_m64          . M64FP     <$> getAddr asize m
                  (0xDD, 2, False) -> skipWord8 >> FST_m64           . M64FP     <$> getAddr asize m
                  (0xDD, 2, True ) -> skipWord8 >> FST_st            . ST        <$> right (rmField m)
                  (0xDE, 2, False) -> skipWord8 >> FICOM_m16         . M16INT    <$> getAddr asize m

                  (0xD8, 3, True ) -> skipWord8 >> FCOMP_st          . ST        <$> right (rmField m)
                  (0xD8, 3, False) -> skipWord8 >> FCOMP_m32         . M32FP     <$> getAddr asize m
                  (0xD9, 3, False) -> skipWord8 >> FSTP_m32          . M32FP     <$> getAddr asize m
                  (0xDA, 3, False) -> skipWord8 >> FICOMP_m32        . M32INT    <$> getAddr asize m
                  (0xDB, 3, False) -> skipWord8 >> FISTP_m32         . M32INT    <$> getAddr asize m
                  (0xDC, 3, False) -> skipWord8 >> FCOMP_m64         . M64FP     <$> getAddr asize m
                  (0xDD, 3, True ) -> skipWord8 >> FSTP_st           . ST        <$> right (rmField m)
                  (0xDD, 3, False) -> skipWord8 >> FSTP_m64          . M64FP     <$> getAddr asize m
                  (0xDE, 3, False) -> skipWord8 >> FICOMP_m16        . M16INT    <$> getAddr asize m
                  (0xDF, 3, False) -> skipWord8 >> FISTP_m16         . M16INT    <$> getAddr asize m

                  (0xD8, 4, True ) -> skipWord8 >> FSUB_st0_st0_sti  . ST        <$> right (rmField m)
                  (0xD8, 4, False) -> skipWord8 >> FSUB_st0_st0_m32  . M32FP     <$> getAddr asize m
                  (0xD9, 4, False) -> skipWord8 >> FLDENV            . MENV      <$> getAddr asize m
                  (0xDA, 4, False) -> skipWord8 >> FISUB_m32         . M32INT    <$> getAddr asize m
                  (0xDC, 4, False) -> skipWord8 >> FSUB_st0_st0_m64  . M64FP     <$> getAddr asize m
                  (0xDC, 4, True ) -> skipWord8 >> FSUB_sti_st0_sti  . ST        <$> right (rmField m)
                  (0xDD, 4, True ) -> skipWord8 >> FUCOM             . ST        <$> right (rmField m)
                  (0xDD, 4, False) -> skipWord8 >> FRSTOR            . MSTATE    <$> getAddr asize m
                  (0xDE, 4, True ) -> skipWord8 >> FSUBP_sti_sti_st0 . ST        <$> right (rmField m)
                  (0xDE, 4, False) -> skipWord8 >> FISUB_m16         . M16INT    <$> getAddr asize m
                  (0xDF, 4, False) -> skipWord8 >> FBLD              . M80DEC    <$> getAddr asize m

                  (0xD8, 5, True ) -> skipWord8 >> FSUB_st0_sti_st0  . ST        <$> right (rmField m)
                  (0xD8, 5, False) -> skipWord8 >> FSUB_st0_m32_st0  . M32FP     <$> getAddr asize m
                  (0xD9, 5, False) -> skipWord8 >> FLDCW             . MCW       <$> getAddr asize m
                  (0xDA, 5, False) -> skipWord8 >> FISUBR_m32        . M32INT    <$> getAddr asize m
                  (0xDB, 5, False) -> skipWord8 >> FLD_m80           . M80FP     <$> getAddr asize m
                  (0xDB, 5, True ) -> skipWord8 >> FUCOMI            . ST        <$> right (rmField m)
                  (0xDC, 5, False) -> skipWord8 >> FSUB_st0_m64_st0  . M64FP     <$> getAddr asize m
                  (0xDC, 5, True ) -> skipWord8 >> FSUB_sti_sti_st0  . ST        <$> right (rmField m)
                  (0xDD, 5, True ) -> skipWord8 >> FUCOMP            . ST        <$> right (rmField m)
                  (0xDE, 5, True ) -> skipWord8 >> FSUBP_st0_st0_sti . ST        <$> right (rmField m)
                  (0xDE, 5, False) -> skipWord8 >> FISUBR_m16        . M16INT    <$> getAddr asize m
                  (0xDF, 5, True ) -> skipWord8 >> FUCOMIP           . ST        <$> right (rmField m)
                  (0xDF, 5, False) -> skipWord8 >> FILD_m64          . M64INT    <$> getAddr asize m

                  (0xD8, 6, True ) -> skipWord8 >> FDIV_st0_st0_sti  . ST        <$> right (rmField m)
                  (0xD8, 6, False) -> skipWord8 >> FDIV_m32          . M32FP     <$> getAddr asize m
                  (0xD9, 6, False) -> skipWord8 >> FNSTENV           . MENV      <$> getAddr asize m
                  (0xDA, 6, False) -> skipWord8 >> FIDIV_m32         . M32INT    <$> getAddr asize m
                  (0xDC, 6, True ) -> skipWord8 >> FDIV_sti_st0_sti  . ST        <$> right (rmField m)
                  (0xDC, 6, False) -> skipWord8 >> FDIV_m64          . M64FP     <$> getAddr asize m
                  (0xDD, 6, False) -> skipWord8 >> FNSAVE            . MSTATE    <$> getAddr asize m
                  (0xDE, 6, True ) -> skipWord8 >> FDIVRP            . ST        <$> right (rmField m)
                  (0xDE, 6, False) -> skipWord8 >> FIDIV_m16         . M16INT    <$> getAddr asize m
                  (0xDF, 6, False) -> skipWord8 >> FBSTP             . M80BCD    <$> getAddr asize m
                  (0xDF, 6, True ) -> skipWord8 >> FCOMIP            . ST        <$> right (rmField m)

                  (0xD8, 7, True ) -> skipWord8 >> FDIV_st0_sti_st0  . ST        <$> right (rmField m)
                  (0xD8, 7, False) -> skipWord8 >> FDIVR_m32         . M32FP     <$> getAddr asize m
                  (0xD9, 7, False) -> skipWord8 >> FNSTCW            . MCW       <$> getAddr asize m
                  (0xDA, 7, False) -> skipWord8 >> FIDIVR_m32        . M32INT    <$> getAddr asize m
                  (0xDB, 7, False) -> skipWord8 >> FSTP_m80          . M80FP     <$> getAddr asize m
                  (0xDC, 7, True ) -> skipWord8 >> FDIV_sti_sti_st0  . ST        <$> right (rmField m)
                  (0xDC, 7, False) -> skipWord8 >> FDIVR_m64         . M64FP     <$> getAddr asize m
                  (0xDD, 7, False) -> skipWord8 >> FNSTSW            . MSW       <$> getAddr asize m
                  (0xDE, 7, True ) -> skipWord8 >> FDIVP             . ST        <$> right (rmField m)
                  (0xDE, 7, False) -> skipWord8 >> FIDIVR_m16        . M16INT    <$> getAddr asize m
                  (0xDF, 7, False) -> skipWord8 >> FISTP_m64         . M64INT    <$> getAddr asize m

                  _                -> left $ ErrUnknownOpcode [x]


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
