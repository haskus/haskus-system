-- | Bindings from asm-generic/ioctl.h
--
-- Warning: some constants may be modified depending on the architecture. For now, we only support X86_64.
module ViperVM.Arch.Linux.Ioctl
   ( Command (..)
   , Direction(..)
   , CommandType
   , CommandNumber
   , CommandSize
   , encodeCommand
   , decodeCommand
   , signalCommand
   , readCommand
   , writeCommand
   , readWriteCommand
   )
where

import Control.Applicative ((<$>))
import Data.Word
import Data.Bits
import Foreign.Storable
import Foreign.Ptr

type CommandType   = Word8
type CommandNumber = Word8
type CommandSize   = Word16 -- is really 14 bits

-- | Direction of the IOCTL
data Direction = None | Read | Write | ReadWrite

-- | An IO Control Command
data Command = Command
   { ioctlType      :: CommandType   -- ^ IOCTL type
   , ioctlNumber    :: CommandNumber -- ^ IOCTL number
   , ioctlDirection :: Direction     -- ^ IOCTL direction (2 bits by default)
   , ioctlSize      :: CommandSize   -- ^ IOCTL size (14 bits by default)
   }

-- | Decode a command
decodeCommand :: Word32 -> Command
decodeCommand w = Command 
      (fromIntegral $ (w `shiftR` 8) .&. 0x000000FF)
      (fromIntegral $ w .&. 0x000000FF)
      (toDir (w `shiftR` 30))
      (fromIntegral $ (w `shiftR` 16) .&. 0x3FFF)
   where
      toDir 0 = None
      toDir 1 = Write
      toDir 2 = Read
      toDir 3 = ReadWrite
      toDir _ = error "Invalid direction" -- should never happen


-- | Encode a command
encodeCommand :: Command -> Word32
encodeCommand c = w
   where
      fromDir None      = 0
      fromDir Write     = 1
      fromDir Read      = 2
      fromDir ReadWrite = 3
      w =   fromIntegral (ioctlNumber c)
        .|. (fromIntegral (ioctlType c)  `shiftL` 8)
        .|. (fromDir (ioctlDirection  c) `shiftL` 30)
        .|. ((fromIntegral (ioctlSize c) .&. 0x3FFFF) `shiftL` 16)

-- | IOCTL control commands are stored in 32 bits: 
-- DDSS SSSS SSSS SSSS TTTT TTTT NNNN NNNN
-- where D = direction, S = Size, T = Type and N = Number
instance Storable Command where
   sizeOf _ = 4
   alignment _ = 4
   peek ptr = do
      decodeCommand <$> peek (castPtr ptr :: Ptr Word32)

   poke ptr = poke (castPtr ptr :: Ptr Word32) . encodeCommand

---------------------------------------------------
-- Helper functions to build commands
---------------------------------------------------

-- | Helper to check parameter size
paramSize :: Storable a => a -> Word16
paramSize x | sz .&. 0xC0 == 0 = fromIntegral sz
            | otherwise        = error "Invalid size (> 14 bits)"
   where sz = sizeOf x

-- | Create a basic commad: without additional parameter
signalCommand :: CommandType -> CommandNumber -> Command
signalCommand typ nr = Command typ nr None 0

-- | Create a Read command
readCommand :: Storable a => CommandType -> CommandNumber -> a -> Command
readCommand typ nr param = Command typ nr Read (paramSize param)

-- | Create a Write command
writeCommand :: Storable a => CommandType -> CommandNumber -> a -> Command
writeCommand typ nr param = Command typ nr Write (paramSize param)

-- | Create a ReadWrite command
readWriteCommand :: Storable a => CommandType -> CommandNumber -> a -> Command
readWriteCommand typ nr param = Command typ nr ReadWrite (paramSize param)
