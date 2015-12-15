module ViperVM.Arch.X86_64.Assembler.X86Dec
   ( InstructionSet(..)
   , X86Dec
   , X86State(..)
   , VectorLength (..)
   , DecodeError(..)
   , getMode
   , getAllowedSets
   , getLegacyPrefixes
   , getAddressSize
   , getOperandSize
   , getBaseRegExt
   , getIndexRegExt
   , getRegExt
   , getUseExtRegs
   , getOpSize64
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
   )
where

import Data.Word
import qualified Data.List as List
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns

data DecodeError
   = ErrInsnTooLong                    -- ^ Instruction is more than 15 bytes long
   | ErrTooManyLegacyPrefixes          -- ^ More than 4 legacy prefixes
   | ErrInvalidLegacyPrefixGroups      -- ^ More than 1 legacy prefix for a single group
   | ErrUnknownOpcode OpcodeMap Word8  -- ^ Unrecognized opcode
   | ErrRexPrefixBeforeVex             -- ^ REX prefix found before VEX prefix
   | ErrLegacyPrefixBeforeVex [Word8]  -- ^ Invalid legacy prefixes found before VEX prefix
   | ErrLegacyPrefixBeforeXop [Word8]  -- ^ Invalid legacy prefixes found before XOP prefix
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
   { stateMode             :: X86Mode            -- ^ Architecture execution mode
   , stateSets             :: [InstructionSet]   -- ^ Available instruction sets

   , stateAddressSize      :: AddressSize        -- ^ Address size
   , stateOperandSize      :: OperandSize        -- ^ Operand size

   , stateByteCount        :: Int                -- ^ Number of bytes read (used to fail when > 15)
   , stateLegacyPrefixes   :: [Word8]            -- ^ Legacy prefixes
   , stateBaseRegExt       :: Word8              -- ^ Extension for the base register (REX.B)
   , stateIndexRegExt      :: Word8              -- ^ Extension for the index register (REX.X)
   , stateRegExt           :: Word8              -- ^ Extension for a register operand (REX.R)
   , stateOpSize64         :: Bool               -- ^ Indicate if the operand size is forced to 64-bit (REX.W)
   , stateUseExtRegs       :: Bool               -- ^ Indicate if extended 64-bit registers have to be used (e.g. a REX prefix has been read)
   , stateHasRexPrefix     :: Bool               -- ^ Indicate that a REX prefix has been read
   , stateMapSelect        :: [Word8]            -- ^ Map selection
   , stateHasVexPrefix     :: Bool               -- ^ Indicate that a VEX prefix has been read
   , stateHasXopPrefix     :: Bool               -- ^ Indicate that a XOP prefix has been read
   , stateOpcodeExtE       :: Maybe Bool         -- ^ Opcode extension in VEX/XOP.E
   , stateAdditionalOp     :: Maybe Word8        -- ^ Additional operand (VEX.vvvv)
   , stateVectorLength     :: Maybe VectorLength -- ^ Vector length (VEX.L)
   }

data VectorLength = VL128 | VL256 deriving (Show,Eq)



-- | Get X86 Mode
getMode :: X86Dec X86Mode
getMode = stateMode <$> lift get

-- | Get allowed instruction sets
getAllowedSets :: X86Dec [InstructionSet]
getAllowedSets = stateSets <$> lift get

-- | Get legacy prefixes
getLegacyPrefixes :: X86Dec [Word8]
getLegacyPrefixes = stateLegacyPrefixes <$> lift get

-- | Get current address size
getAddressSize :: X86Dec AddressSize
getAddressSize = stateAddressSize <$> lift get

-- | Get current operand size
getOperandSize :: X86Dec OperandSize
getOperandSize = stateOperandSize <$> lift get

getBaseRegExt :: X86Dec Word8
getBaseRegExt = stateBaseRegExt <$> lift get

getIndexRegExt :: X86Dec Word8
getIndexRegExt = stateIndexRegExt <$> lift get

getRegExt :: X86Dec Word8
getRegExt = stateRegExt <$> lift get

getUseExtRegs :: X86Dec Bool
getUseExtRegs = stateUseExtRegs <$> lift get

getOpSize64 :: X86Dec Bool
getOpSize64 = stateOpSize64 <$> lift get

-- | Assert that no Rex prefix has been decoded
assertNoRex :: DecodeError -> X86Dec ()
assertNoRex err = do
   hasRex <- stateHasRexPrefix <$> lift get
   if hasRex
      then left err
      else right ()

-- | Assert that no Vex prefix has been decoded
assertNoVex :: DecodeError -> X86Dec ()
assertNoVex err = do
   hasVex <- stateHasVexPrefix <$> lift get
   if hasVex
      then left err
      else right ()

-- | Assert that no Xop prefix has been decoded
assertNoXop :: DecodeError -> X86Dec ()
assertNoXop err = do
   hasXop <- stateHasXopPrefix <$> lift get
   if hasXop
      then left err
      else right ()

-- | Assert that no legacy prefix in the list has been decoded
assertNoLegacyPrefix :: ([Word8] -> DecodeError) -> [Word8] -> X86Dec ()
assertNoLegacyPrefix err ps = do
   ps' <- getLegacyPrefixes
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
