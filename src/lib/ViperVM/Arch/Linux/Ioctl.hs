{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Linux IOCTL
--
-- Bindings from asm-generic/ioctl.h
--
-- Warning: some constants may be modified depending on the architecture. For
-- now, we only support X86_64.
module ViperVM.Arch.Linux.Ioctl
   ( IOCTL
   , Command (..)
   , Direction(..)
   , CommandType
   , CommandNumber
   , CommandSize
   -- * Command encoding/decoding
   , encodeCommand
   , decodeCommand
   -- * Signal
   , ioctlSignal
   -- * Read
   , ioctlRead
   , ioctlReadBytes
   , ioctlReadByteString
   , ioctlReadBuffer
   , ioctlReadWithRet
   -- * Write
   , ioctlWrite
   -- * Read / Write
   , ioctlReadWrite
   , ioctlReadWriteWithRet
   -- * Helpers
   , repeatIoctl
   )
where

import Data.Word
import Data.Int
import Data.Bits
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Marshal.Utils (with)
import qualified Data.ByteString as BS

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import ViperVM.Arch.X86_64.Linux.Syscall

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
paramSize x | sz .&. 0xC000 == 0 = fromIntegral sz
            | otherwise          = error "Invalid size (> 14 bits)"
   where sz = sizeOf x

-- | We abstract over the ioctl function
type IOCTL = FileDescriptor -> Int64 -> Int64 -> IO Int64

-- | Build a Read IOCTL
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. If there is no error, the read value is returned.
ioctlRead :: Storable a => IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> FileDescriptor -> SysRet a
ioctlRead ioctl typ nr test fd = fmap snd <$> ioctlReadWithRet ioctl typ nr test fd

-- | Build a Read IOCTL
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. If there is no error, the read value is returned.
ioctlReadWithRet :: Storable a => IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> FileDescriptor -> SysRet (Int64,a)
ioctlReadWithRet ioctl typ nr test fd = do
   alloca $ \(ptr :: Ptr a) -> do
      let cmd = Command typ nr Read (paramSize (undefined :: a))
      ret <- ioctl fd (toArg $ encodeCommand cmd) (toArg ptr)
      case test ret of
         Just err -> return (Left err)
         Nothing  -> Right . (ret,) <$> peek ptr

-- | Build a Read IOCTL
--
-- Read n bytes and return the ioctl returned value
ioctlReadBytes :: IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> Word16 -> Ptr a -> FileDescriptor -> SysRet Int64
ioctlReadBytes ioctl typ nr test n ptr fd = do
   let cmd = Command typ nr Read n
   ret <- ioctl fd (toArg $ encodeCommand cmd) (toArg ptr)
   case test ret of
      Just err -> return (Left err)
      Nothing  -> return (Right ret)

-- | Build a Read IOCTL
--
-- Read n bytes and return the ioctl returned value
ioctlReadByteString :: IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> Int -> FileDescriptor -> SysRet (Int64, BS.ByteString)
ioctlReadByteString ioctl typ nr test n fd = do
   allocaBytes n $ \ptr -> do
      ret <- ioctlReadBytes ioctl typ nr test (fromIntegral n) ptr fd
      case ret of
         Left err -> return (Left err)
         Right v  -> Right . (v,) <$> BS.packCStringLen (ptr, n)

-- | Build a Read IOCTL
--
-- The returned value is the size of the buffer
ioctlReadBuffer :: IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> (Word16 -> Ptr a -> IO b) -> Word16 -> FileDescriptor -> SysRet b
ioctlReadBuffer ioctl typ nr test f defn fd = go defn
   where
      go n = do
         -- try with the given buffer size
         allocaBytes (fromIntegral n) $ \ptr -> do
            ret <- ioctlReadBytes ioctl typ nr test n ptr fd
            case ret of
               Left err                      -> return (Left err)
               Right len 
                  | len <= (fromIntegral n)  -> Right <$> f n ptr
                  -- try with the returned buffer size
                  | otherwise                -> go (fromIntegral len)

-- | Build a Write IOCTL
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. 
ioctlWrite :: Storable a => IOCTL -> CommandType -> CommandNumber -> (Int64 -> SysRet b) -> FileDescriptor -> a -> SysRet b
ioctlWrite ioctl typ nr test fd arg = do
   with arg $ \ptr -> do
      let cmd = Command typ nr Write (paramSize arg)
      ret <- ioctl fd (toArg $ encodeCommand cmd) (toArg ptr)
      test ret

-- | Build a ReadWrite IOCTL
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. If there is no error, the read value is returned.
ioctlReadWrite :: Storable a => IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> FileDescriptor -> a -> SysRet a
ioctlReadWrite ioctl typ nr test fd arg = fmap snd <$> ioctlReadWriteWithRet ioctl typ nr test fd arg

-- | Build a ReadWrite IOCTL
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. If there is no error, the read value is returned.
ioctlReadWriteWithRet :: Storable a => IOCTL -> CommandType -> CommandNumber -> (Int64 -> Maybe ErrorCode) -> FileDescriptor -> a -> SysRet (Int64,a)
ioctlReadWriteWithRet ioctl typ nr test fd arg = do
   with arg $ \ptr -> do
      let cmd = Command typ nr ReadWrite (paramSize arg)
      ret <- ioctl fd (toArg $ encodeCommand cmd) (toArg ptr)
      case test ret of
         Just err -> return (Left err)
         Nothing  -> Right . (ret,) <$> peek ptr

-- | Build a Signal IOCTL (direction = None)
--
-- Execute the IOCTL command on the file descriptor, then `test` the result. 
ioctlSignal :: IOCTL -> CommandType -> CommandNumber -> (Int64 -> SysRet b) -> FileDescriptor -> SysRet b
ioctlSignal ioctl typ nr test fd = do
   let cmd = Command typ nr None 0
   ret <- ioctl fd (toArg $ encodeCommand cmd) (toArg nullPtr)
   test ret

-- | Call the IOCTL, restarting if interrupted
repeatIoctl :: IOCTL -> IOCTL
repeatIoctl ioctl fd nr arg = do
   ret <- ioctl fd nr arg
   case defaultCheck ret of
      Just EINTR  -> repeatIoctl ioctl fd nr arg
      Just EAGAIN -> repeatIoctl ioctl fd nr arg
      _           -> return ret
