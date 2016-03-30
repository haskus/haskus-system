{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Linux IOCTL
--
-- Bindings from asm-generic/ioctl.h
--
-- Warning: some constants may be modified depending on the architecture. For
-- now, we only support X86_64.
module ViperVM.Arch.Linux.Ioctl
   (
   -- * Command
     Command
   , Direction(..)
   , ioctlCommand
   -- * Signal
   , ioctlSignalCmd
   , ioctlSignal
   , ioctlSignalCmdRet
   , ioctlSignalRet
   -- * Read
   , ioctlReadCmd
   , ioctlRead
   , ioctlReadCmdRet
   , ioctlReadRet
   , ioctlReadBytes
   , ioctlReadByteString
   , ioctlReadBuffer
   -- * Write
   , ioctlWriteCmd
   , ioctlWrite
   , ioctlWriteCmdRet
   , ioctlWriteRet
   , ioctlWriteValue
   -- * Write / Read
   , ioctlWriteReadCmd
   , ioctlWriteRead
   , ioctlWriteReadCmdRet
   , ioctlWriteReadRet
   )
where

import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Control.Monad
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Marshal.Utils (with)
import qualified Data.ByteString as BS

import ViperVM.Format.Binary.BitField
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Ioctl
import ViperVM.Arch.Linux.Internals.Arg

---------------------------------------------------
-- IOCTL
---------------------------------------------------

-- | Send a custom command to a device
ioctl :: Arg a => Command -> a -> Handle -> IO Int64
ioctl (Command cmd) arg (Handle fd) =
   syscall_ioctl fd (fromIntegral (bitFieldsBits cmd)) (toArg arg)

-----------------------------------------------------------------------------
-- Write/Read
-----------------------------------------------------------------------------

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmdRet :: Storable a => Command -> a -> Handle -> SysRet (Int64,a)
ioctlWriteReadCmdRet cmd a fd =
   with a $ \pa -> do
      r <- ioctl cmd pa fd
      case defaultCheck r of
         Nothing  -> Right . (r,) <$> peek pa
         Just err -> return (Left err)

-- | Write and read a storable, return the valid returned value
ioctlWriteReadRet :: forall a. Storable a => CommandType -> CommandNumber -> a -> Handle -> SysRet (Int64,a)
ioctlWriteReadRet typ nr = ioctlWriteReadCmdRet cmd
   where
      cmd = ioctlCommand WriteRead typ nr (sizeOf (undefined :: a))

-- | Write and read a storable
ioctlWriteRead :: Storable a => CommandType -> CommandNumber -> a -> Handle -> SysRet a
ioctlWriteRead typ nr a fd = fmap snd <$> ioctlWriteReadRet typ nr a fd

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmd :: Storable a => Command -> a -> Handle -> SysRet a
ioctlWriteReadCmd cmd a fd = fmap snd <$> ioctlWriteReadCmdRet cmd a fd
      
-----------------------------------------------------------------------------
-- Read
-----------------------------------------------------------------------------

-- | Read a storable, use an arbitrary command
ioctlReadCmdRet :: Storable a => Command -> Handle -> SysRet (Int64,a)
ioctlReadCmdRet cmd fd =
   alloca $ \pa -> do
      r <- ioctl cmd pa fd
      case defaultCheck r of
         Nothing  -> Right . (r,) <$> peek pa
         Just err -> return (Left err)

-- | Read a storable, return the valid returned value
ioctlReadRet :: forall a. Storable a => CommandType -> CommandNumber -> Handle -> SysRet (Int64,a)
ioctlReadRet typ nr = ioctlReadCmdRet cmd
   where
      cmd = ioctlCommand Read typ nr (sizeOf (undefined :: a))

-- | Read a storable
ioctlRead :: Storable a => CommandType -> CommandNumber -> Handle -> SysRet a
ioctlRead typ nr fd = fmap snd <$> ioctlReadRet typ nr fd

-- | Read a storable, use an arbitrary command
ioctlReadCmd :: Storable a => Command -> Handle -> SysRet a
ioctlReadCmd cmd fd = fmap snd <$> ioctlReadCmdRet cmd fd

-----------------------------------------------------------------------------
-- Write
-----------------------------------------------------------------------------

-- | Write a storable, use an arbitrary command
ioctlWriteCmdRet :: Storable a => Command -> a -> Handle -> SysRet Int64
ioctlWriteCmdRet cmd a fd =
   with a $ \pa -> do
      r <- ioctl cmd pa fd
      return $ case defaultCheck r of
         Nothing  -> Right r
         Just err -> Left err

-- | Write a storable, return the valid returned value
ioctlWriteRet :: forall a. Storable a => CommandType -> CommandNumber -> a -> Handle -> SysRet Int64
ioctlWriteRet typ nr = ioctlWriteCmdRet cmd
   where
      cmd = ioctlCommand Write typ nr (sizeOf (undefined :: a))

-- | Write a storable
ioctlWrite :: Storable a => CommandType -> CommandNumber -> a -> Handle -> SysRet ()
ioctlWrite typ nr a fd = void <$> ioctlWriteRet typ nr a fd

-- | Write a storable, use an arbitrary command
ioctlWriteCmd :: Storable a => Command -> a -> Handle -> SysRet ()
ioctlWriteCmd cmd a fd = void <$> ioctlWriteCmdRet cmd a fd

-- | Build a Write IOCTL where the value is directly passed in the `arg`
-- parameter.
ioctlWriteValue :: (Storable a, Arg a) => CommandType -> CommandNumber -> a -> Handle -> SysRet ()
ioctlWriteValue typ nr arg fd = do
   let cmd = ioctlCommand Write typ nr (sizeOf arg)
   ret <- ioctl cmd arg fd
   return $ case defaultCheck ret of
      Nothing -> Right ()
      Just x  -> Left x

-----------------------------------------------------------------------------
-- signal (Direction = None)
-----------------------------------------------------------------------------

-- | Signal, use an arbitrary command
ioctlSignalCmdRet :: Arg a => Command -> a -> Handle -> SysRet Int64
ioctlSignalCmdRet cmd a fd = do
   r <- ioctl cmd a fd
   return $ case defaultCheck r of
      Nothing  -> Right r
      Just err -> Left err

-- | Signal, return the valid returned value
ioctlSignalRet :: Arg a => CommandType -> CommandNumber -> a -> Handle -> SysRet Int64
ioctlSignalRet typ nr = ioctlSignalCmdRet (ioctlCommand None typ nr 0)

-- | Signal
ioctlSignal :: Arg a => CommandType -> CommandNumber -> a -> Handle -> SysRet ()
ioctlSignal typ nr a fd = void <$> ioctlSignalRet typ nr a fd

-- | Signal, use an arbitrary command
ioctlSignalCmd :: Arg a => Command -> a -> Handle -> SysRet ()
ioctlSignalCmd cmd a fd = void <$> ioctlSignalCmdRet cmd a fd


-----------------------------------------------------------------------------
-- Read buffers
-----------------------------------------------------------------------------

-- | Build a Read ioctl that reads the given number of bytes
ioctlReadBytes :: CommandType -> CommandNumber -> Int -> Ptr a -> Handle -> SysRet Int64
ioctlReadBytes typ nr n ptr fd = do
   let cmd = ioctlCommand Read typ nr n
   ret <- ioctl cmd ptr fd
   case defaultCheck ret of
      Just err -> return (Left err)
      Nothing  -> return (Right ret)

-- | Build a Read ioctl that reads the given number of bytes and return them in
-- a ByteString
ioctlReadByteString :: CommandType -> CommandNumber -> Int -> Handle -> SysRet (Int64, BS.ByteString)
ioctlReadByteString typ nr n fd =
   allocaBytes n $ \ptr -> do
      ret <- ioctlReadBytes typ nr n ptr fd
      case ret of
         Left err -> return (Left err)
         Right v  -> Right . (v,) <$> BS.packCStringLen (ptr, n)

-- | Build a Read ioctl for variable sized buffers. We except the ioctl to
-- return the length of the data that can be read. We first try to read with a
-- buffer of `defn` bytes. If there are data left, we retry with a buffer of the
-- appropriate size.
ioctlReadBuffer :: CommandType -> CommandNumber -> (Int -> Ptr a -> IO b) -> Int -> Handle -> SysRet b
ioctlReadBuffer typ nr f defn fd = go defn
   where
      go n =
         -- try with the given buffer size
         allocaBytes n $ \ptr -> do
            ret <- ioctlReadBytes typ nr n ptr fd
            case ret of
               Left err                    -> return (Left err)
               Right len 
                  | len <= fromIntegral n  -> Right <$> f n ptr
                  -- try with the returned buffer size
                  | otherwise              -> go (fromIntegral len)



