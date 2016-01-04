module ViperVM.Arch.X86_64.Assembler.X86Dec
   ( InstructionSet(..)
   , X86Dec
   , X86State(..)
   , VectorLength (..)
   , DecodeError(..)
   , assertNoRex
   , assertNoVex
   , assertNoXop
   , assertNoLegacyPrefix
   , next
   , lookAhead
   , nextWord8
   , nextWord16
   , nextWord32
   , nextWord64
   , lookWord8
   , lookWord16
   , lookWord32
   , lookWord64
   , skipWord8
   , skipWord16
   , skipWord32
   , skipWord64
   , decodeLegacyPrefixes
   , getOperandSize
   , getAddr
   , getRMOp
   , getRMRegister
   , getRegOp
   , getEffectiveAddressSize
   , getOpFromRegId
   , getImplicitOp
   )
where

import Data.Word
import Data.Bits
import qualified Data.List as List
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.Utils
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.OperandSize
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix

data DecodeError
   = ErrInsnTooLong                    -- ^ Instruction is more than 15 bytes long
   | ErrTooManyLegacyPrefixes          -- ^ More than 4 legacy prefixes
   | ErrInvalidLegacyPrefixGroups      -- ^ More than 1 legacy prefix for a single group
   | ErrInvalidOpcodeMap OpcodeMap     -- ^ Invalid opcode map
   | ErrUnknownOpcode OpcodeMap Word8  -- ^ Unrecognized opcode
   | ErrLegacyPrefixBeforeVex [Word8]  -- ^ Invalid legacy prefixes found before VEX prefix
   | ErrLegacyPrefixBeforeXop [Word8]  -- ^ Invalid legacy prefixes found before XOP prefix
   | ErrRexPrefixBeforeVex             -- ^ REX prefix found before VEX prefix
   | ErrRexPrefixBeforeXop             -- ^ REX prefix found before XOP prefix
   | ErrVexPrefixBeforeXop             -- ^ VEX prefix found before XOP prefix
   | ErrXopPrefixBeforeVex             -- ^ XOP prefix found before VEX prefix
   | ErrVexEscapedOpcode               -- ^ VEX prefix found with escaped opcode (more than 1 byte)
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
   { decStateMode               :: X86Mode            -- ^ Architecture execution mode
   , decStateSets               :: [InstructionSet]   -- ^ Available instruction sets

   , decStateDefaultAddressSize :: AddressSize        -- ^ Current default address size
   , decStateDefaultOperandSize :: OperandSize        -- ^ Current default operand size (for legacy / compatibility modes)

   , decStateByteCount          :: Int                -- ^ Number of bytes read (used to fail when > 15)
   , decStateLegacyPrefixes     :: [Word8]            -- ^ Legacy prefixes
   , decStateBaseRegExt         :: Word8              -- ^ Extension for the base register (REX.B)
   , decStateIndexRegExt        :: Word8              -- ^ Extension for the index register (REX.X)
   , decStateRegExt             :: Word8              -- ^ Extension for a register operand (REX.R)
   , decStateOpSize64           :: Bool               -- ^ Indicate if the operand size is forced to 64-bit (REX.W)
   , decStateUseExtRegs         :: Bool               -- ^ Indicate if extended 64-bit registers have to be used (e.g. a REX prefix has been read)
   , decStateHasRexPrefix       :: Bool               -- ^ Indicate that a REX prefix has been read
   , decStateOpcodeMap          :: OpcodeMap          -- ^ Opcode map
   , decStateHasVexPrefix       :: Bool               -- ^ Indicate that a VEX prefix has been read
   , decStateHasXopPrefix       :: Bool               -- ^ Indicate that a XOP prefix has been read
   , decStateOpcodeExtE         :: Maybe Bool         -- ^ Opcode extension in VEX/XOP.E
   , decStateAdditionalOp       :: Maybe Word8        -- ^ Additional operand (VEX.vvvv)
   , decStateVectorLength       :: Maybe VectorLength -- ^ Vector length (VEX.L)
   }

data VectorLength = VL128 | VL256 deriving (Show,Eq)

-- | Assert that no Rex prefix has been decoded
assertNoRex :: DecodeError -> X86Dec ()
assertNoRex err = do
   hasRex <- gets decStateHasRexPrefix
   if hasRex
      then left err
      else right ()

-- | Assert that no Vex prefix has been decoded
assertNoVex :: DecodeError -> X86Dec ()
assertNoVex err = do
   hasVex <- gets decStateHasVexPrefix
   if hasVex
      then left err
      else right ()

-- | Assert that no Xop prefix has been decoded
assertNoXop :: DecodeError -> X86Dec ()
assertNoXop err = do
   hasXop <- gets decStateHasXopPrefix
   if hasXop
      then left err
      else right ()

-- | Assert that no legacy prefix in the list has been decoded
assertNoLegacyPrefix :: ([Word8] -> DecodeError) -> [Word8] -> X86Dec ()
assertNoLegacyPrefix err ps = do
   ps' <- gets decStateLegacyPrefixes
   case ps `List.intersect` ps' of
      [] -> right ()
      xs -> left (err xs)

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
   bc <- gets decStateByteCount
   if bc + n > 15
      then left ErrInsnTooLong
      else do
         -- try to do the actual read
         w <- lift (lift getter)
         -- store the new number of bytes
         lift $ modify (\y -> y {decStateByteCount = bc +n})
         right w

lookAhead :: Int -> Get a -> X86Dec a
lookAhead n getter = do
   -- Test that we don't read more than 15 bytes
   bc <- gets decStateByteCount
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

-- | Get operand size for the given instruction encoding
getOperandSize :: Encoding -> X86Dec OperandSize
getOperandSize enc = computeOperandSize
   <$> gets decStateMode
   <*> (fmap toLegacyPrefix <$> gets decStateLegacyPrefixes)
   <*> gets decStateDefaultOperandSize
   <*> gets decStateOpSize64
   <*> return enc

-- | Decode legacy prefixes. See Note [Legacy prefixes].
--
-- If allowMultiple is set, more than one prefix is allowed per group, but only
-- the last one is taken into account.
--
-- If allowMoreThan4 is set, more than 4 prefixes can be used (it requires
-- allowMultiple)
decodeLegacyPrefixes :: Bool -> Bool -> X86Dec [Word8]
decodeLegacyPrefixes allowMultiple allowMoreThan4 = do
      rec 0 (V.fromList [0,0,0,0])
   where
      rec :: Int -> V.Vector Word8 -> X86Dec [Word8]
      rec n _
         | n > 4 && not allowMoreThan4 = left ErrTooManyLegacyPrefixes

      rec n xs = do
         x <- lookWord8

         case legacyPrefixGroup x of
            Nothing -> return $ V.toList $ V.filter (/= 0) xs

            Just g  -> case xs V.! g of
               y | y /= 0 && not allowMultiple 
                  -> left ErrInvalidLegacyPrefixGroups
               _  -> skipWord8 >> rec (n+1) (V.modify (\v -> VM.write v g x) xs)

-- | Read the memory addressing in r/m field
getAddr :: ModRM -> X86Dec Addr
getAddr modrm = do
   baseExt  <- gets decStateBaseRegExt
   indexExt <- gets decStateIndexRegExt
   useExtendedRegisters <- gets decStateUseExtRegs
   asize    <- gets decStateDefaultAddressSize
   mode     <- gets decStateMode

   -- depending on the r/m field in ModRM and on the address size, we know if
   -- we must read a SIB byte
   sib   <- case useSIB asize modrm of
      True  -> Just . SIB <$> nextWord8
      False -> return Nothing

   -- depending on the mod field and the r/m in ModRM and on the address size,
   -- we know if we must read a displacement and its size
   disp <- case useDisplacement asize modrm of
      Nothing     -> return Nothing
      Just Size8  -> Just . SizedValue8  <$> nextWord8
      Just Size16 -> Just . SizedValue16 <$> nextWord16
      Just Size32 -> Just . SizedValue32 <$> nextWord32
      Just _      -> error "Invalid displacement size"

   return $ case asize of
      -- if we are in 16-bit addressing mode, we don't care about the base
      -- register extension
      AddrSize16 -> case (modField modrm, rmField modrm) of
         (_,0) -> Addr (Just R_BX) (Just R_SI) disp Nothing
         (_,1) -> Addr (Just R_BX) (Just R_DI) disp Nothing
         (_,2) -> Addr (Just R_BP) (Just R_SI) disp Nothing
         (_,3) -> Addr (Just R_BP) (Just R_DI) disp Nothing
         (_,4) -> Addr (Just R_SI) Nothing     disp Nothing
         (_,5) -> Addr (Just R_DI) Nothing     disp Nothing
         (0,6) -> Addr Nothing     Nothing     disp Nothing
         (_,6) -> Addr (Just R_BP) Nothing     disp Nothing
         (_,7) -> Addr (Just R_BX) Nothing     disp Nothing
         _     -> error "Invalid 16-bit addressing"

      
      -- 32-bit and 64-bit addressing
      _ -> Addr baseReg indexReg disp scale
         where
            -- size of the operand
            sz   = case asize of
               AddrSize16 -> error "Invalid address size"
               AddrSize32 -> Size32
               AddrSize64 -> Size64

            -- associate base/index register
            makeReg =  regFromCode RF_GPR (Just sz) useExtendedRegisters

            -- the extended base register is either in SIB or in r/m. In some
            -- cases, there is no base register or it is implicitly RIP/EIP
            baseReg = case (modField modrm, rmField modrm) of
               (0,5)
                  | isLongMode mode -> case asize of
                     AddrSize32 -> Just R_EIP
                     AddrSize64 -> Just R_RIP
                     AddrSize16 -> error "Invalid address size"
                  | otherwise -> Nothing
               _ -> Just . makeReg $ (baseExt `shiftL` 3) .|. br
                  where
                     br = case sib of
                        Nothing -> rmField modrm
                        Just s  -> baseField s

            -- if there is an index field, it is in sib
            indexReg = makeReg . ((indexExt `shiftL` 3) .|.) . indexField <$> sib

            -- if there is a scale, it is in sib
            scale = scaleField <$> sib

-- | Return effective address size
--
-- See Table 1-1 "Address-Size Overrides" in AMD Manual v3
--
-- The prefix also changes the size of RCX when it is implicitly accessed
--
-- Address size for implicit accesses to the stack segment is determined by D
-- bit in the stack segment descriptor or is 64 bit in 64 bit mode.
--
getEffectiveAddressSize :: X86Dec AddressSize
getEffectiveAddressSize = do
   mode        <- gets decStateMode
   asize       <- gets decStateDefaultAddressSize
   prefixes    <- fmap toLegacyPrefix <$> gets decStateLegacyPrefixes

   let isOverrided = PrefixAddressSizeOverride `elem` prefixes
   
   return $ case (mode, asize, isOverrided) of
      (LongMode Long64bitMode, _, False)     -> AddrSize64
      (LongMode Long64bitMode, _, True)      -> AddrSize32
      (_, AddrSize16, False)                 -> AddrSize16
      (_, AddrSize32, False)                 -> AddrSize32
      (_, AddrSize32, True)                  -> AddrSize16
      (_, AddrSize16, True)                  -> AddrSize32
      _ -> error "Invalid combination of modes and address sizes"
   
-- | Convert a register identifier into a register
getFromRegId :: OperandSize -> OperandType -> Word8 -> X86Dec Register
getFromRegId opSize typ rid = do
   useExtRegs   <- gets decStateUseExtRegs
   vectorLength <- gets decStateVectorLength
   let 
      fromCode fm sz = regFromCode fm sz useExtRegs
      osize = Just (operandSize opSize)
      sz16  = Just Size16
      sz32  = Just Size32
      sz64  = Just Size64

   return $ case typ of
      T_R      -> fromCode RF_GPR osize rid
      T_R16    -> fromCode RF_GPR sz16  rid
      T_R32    -> fromCode RF_GPR sz32  rid
      T_R16_32 -> case opSize of
         OpSize16 -> fromCode RF_GPR sz16  rid
         OpSize32 -> fromCode RF_GPR sz32  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 16 or 32)"
      T_R32_64 -> case opSize of
         OpSize32 -> fromCode RF_GPR sz32  rid
         OpSize64 -> fromCode RF_GPR sz64  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 32 or 64)"
      T_R16_32_64 -> case opSize of
         OpSize16 -> fromCode RF_GPR sz16  rid
         OpSize32 -> fromCode RF_GPR sz32  rid
         OpSize64 -> fromCode RF_GPR sz64  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 16, 32 or 64)"
      T_RM     -> fromCode RF_GPR osize rid
      T_RM16   -> fromCode RF_GPR sz16  rid
      T_RM32   -> fromCode RF_GPR sz32  rid
      T_RM16_32 -> case opSize of
         OpSize16 -> fromCode RF_GPR sz16  rid
         OpSize32 -> fromCode RF_GPR sz32  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 16 or 32)"
      T_RM32_64 -> case opSize of
         OpSize32 -> fromCode RF_GPR sz32  rid
         OpSize64 -> fromCode RF_GPR sz64  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 32 or 64)"
      T_RM16_32_64 -> case opSize of
         OpSize16 -> fromCode RF_GPR sz16  rid
         OpSize32 -> fromCode RF_GPR sz32  rid
         OpSize64 -> fromCode RF_GPR sz64  rid
         _        -> error $ "Invalid operand size: " ++ show opSize ++ " (expecting 16, 32 or 64)"

      T_V64          -> fromCode RF_VEC (Just Size64) rid
      T_VM64         -> fromCode RF_VEC (Just Size64) rid
      T_V128         -> fromCode RF_VEC (Just Size128) rid
      T_VM128        -> fromCode RF_VEC (Just Size128) rid
      T_V128_Low32   -> fromCode RF_VEC (Just Size128) rid
      T_VM128_Low32  -> fromCode RF_VEC (Just Size128) rid
      T_V128_Low64   -> fromCode RF_VEC (Just Size128) rid
      T_VM128_Low64  -> fromCode RF_VEC (Just Size128) rid

      T_V128_256 -> case vectorLength of
         Just VL128 -> fromCode RF_VEC (Just Size128) rid
         Just VL256 -> fromCode RF_VEC (Just Size256) rid
         Nothing    -> error "Expecting vector length 128 or 256"
      T_VM128_256 -> case vectorLength of
         Just VL128 -> fromCode RF_VEC (Just Size128) rid
         Just VL256 -> fromCode RF_VEC (Just Size256) rid
         Nothing    -> error "Expecting vector length 128 or 256"

      T_ST  -> fromCode RF_X87 Nothing rid

      _        -> error $ "Not a register operand type: " ++ show typ

getOpFromRegId :: OperandSize -> OperandType -> Word8 -> X86Dec Op
getOpFromRegId opSize typ rid = OpReg <$> getFromRegId opSize typ rid

getRegOp :: OperandSize -> OperandType -> ModRM -> X86Dec Op
getRegOp opSize typ modrm = do
   ext <- gets decStateRegExt
   OpReg <$> getFromRegId opSize typ ((ext `shiftL` 3) .|. regField modrm)

getRMRegister :: OperandSize -> OperandType -> ModRM -> X86Dec Register
getRMRegister opSize typ modrm = case rmRegMode modrm of
   True  -> do
      ext <- gets decStateBaseRegExt
      getFromRegId opSize typ ((ext `shiftL` 3) .|. rmField modrm)
   False -> error "Expecting register in ModRM.rm"

getRMOp :: OperandSize -> OperandType -> ModRM -> X86Dec Op
getRMOp opSize typ modrm = case rmRegMode modrm of
   True  -> OpReg <$> getRMRegister opSize typ modrm
   False -> OpMem <$> getAddr modrm

getImplicitOp :: OperandSize -> OperandType -> X86Dec Op
getImplicitOp opSize typ =
   return $ case typ of
      T_Accu -> case opSize of
         OpSize8  -> OpReg R_AL
         OpSize16 -> OpReg R_AX
         OpSize32 -> OpReg R_EAX
         OpSize64 -> OpReg R_RAX
      T_AX_EAX_RAX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> OpReg R_AX
         OpSize32 -> OpReg R_EAX
         OpSize64 -> OpReg R_RAX
      T_xDX_xAX -> case opSize of
         OpSize8  -> OpReg R_AX
         OpSize16 -> OpRegPair R_DX R_AX
         OpSize32 -> OpRegPair R_EDX R_EAX
         OpSize64 -> OpRegPair R_RDX R_RAX
      T_xCX_xBX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> OpRegPair R_CX R_BX
         OpSize32 -> OpRegPair R_ECX R_EBX
         OpSize64 -> OpRegPair R_RCX R_RBX
      T_xAX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> error "Invalid operand size"
         OpSize32 -> OpReg R_EAX
         OpSize64 -> OpReg R_RAX
      T_xBX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> error "Invalid operand size"
         OpSize32 -> OpReg R_EBX
         OpSize64 -> OpReg R_RBX
      T_xCX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> error "Invalid operand size"
         OpSize32 -> OpReg R_ECX
         OpSize64 -> OpReg R_RCX
      T_xDX -> case opSize of
         OpSize8  -> error "Invalid operand size"
         OpSize16 -> error "Invalid operand size"
         OpSize32 -> OpReg R_EDX
         OpSize64 -> OpReg R_RDX
      T_AL   -> OpReg R_AL
      T_AX   -> OpReg R_AX
      T_XMM0 -> OpReg (R_XMM 0)
      T_rSI -> case opSize of
         OpSize8  -> OpRegPair R_DS R_SI
         OpSize16 -> OpRegPair R_DS R_ESI
         OpSize32 -> error "Invalid operand size"
         OpSize64 -> error "Invalid operand size"
      T_rDI -> case opSize of
         OpSize8  -> OpRegPair R_ES R_DI
         OpSize16 -> OpRegPair R_ES R_EDI
         OpSize32 -> error "Invalid operand size"
         OpSize64 -> error "Invalid operand size"

      T_ST0 -> OpReg (R_ST 0)

      _ -> error $ "Invalid implicit operand type$ " ++ show typ
