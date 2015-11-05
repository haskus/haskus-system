module ViperVM.Arch.X86_64.Assembler.X86Dec
   ( InstructionSet(..)
   , X86Dec
   , X86State(..)
   , DecodeError(..)
   , getMode
   , getAllowedSets
   , getAddressSize
   , getOperandSize
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
   , getOperand
   , getAddr
   )
where

import Data.Word
import ViperVM.Format.Binary.Get (Get)
import qualified ViperVM.Format.Binary.Get as G
import Control.Monad.State
import Control.Monad.Trans.Either

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.ModRM

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

-- | Get current operand size
getOperandSize :: X86Dec OperandSize
getOperandSize = stateSegOperandSize <$> lift get

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

