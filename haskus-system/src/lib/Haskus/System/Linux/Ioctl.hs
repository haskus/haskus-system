{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Linux IOCTL
--
-- Bindings from asm-generic/ioctl.h
--
-- Warning: some constants may be modified depending on the architecture. For
-- now, we only support X86_64.
module Haskus.System.Linux.Ioctl
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
   , ioctlReadBuffer
   , ioctlReadVariableBuffer
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

import Foreign.Ptr
import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Ioctl
import Haskus.System.Linux.Internals.Arg
import Haskus.Utils.Flow

---------------------------------------------------
-- IOCTL
---------------------------------------------------

-- | Send a custom command to a device
ioctl :: (Arg a, MonadIO m) => Command -> a -> Handle -> Excepts '[ErrorCode] m Int64
ioctl (Command cmd) arg (Handle fd) =
   checkErrorCode =<< liftIO (syscall_ioctl fd (fromIntegral (bitFieldsBits cmd)) (toArg arg))

-----------------------------------------------------------------------------
-- Write/Read
-----------------------------------------------------------------------------

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmdRet :: (Storable a, MonadInIO m) => Command -> a -> Handle -> Excepts '[ErrorCode] m (Int64,a)
ioctlWriteReadCmdRet cmd a fd =
   with a $ \pa -> do
      r <- ioctl cmd pa fd
      (r,) <$> peek pa

-- | Write and read a storable, return the valid returned value
ioctlWriteReadRet :: forall a m.
   ( MonadInIO m
   , Storable a
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m (Int64,a)
ioctlWriteReadRet typ nr = ioctlWriteReadCmdRet cmd
   where
      cmd = ioctlCommand WriteRead typ nr (sizeOfT' @a)

-- | Write and read a storable
ioctlWriteRead ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m a
ioctlWriteRead typ nr a fd = snd <$> ioctlWriteReadRet typ nr a fd

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmd ::
   ( MonadInIO m
   , Storable a
   ) => Command -> a -> Handle -> Excepts '[ErrorCode] m a
ioctlWriteReadCmd cmd a fd = snd <$> ioctlWriteReadCmdRet cmd a fd
      
-----------------------------------------------------------------------------
-- Read
-----------------------------------------------------------------------------

-- | Read a storable, use an arbitrary command
ioctlReadCmdRet ::
   ( MonadInIO m
   , Storable a
   ) => Command -> Handle -> Excepts '[ErrorCode] m (Int64,a)
ioctlReadCmdRet cmd fd =
   alloca $ \pa -> do
      r <- ioctl cmd pa fd
      (r,) <$> peek pa

-- | Read a storable, return the valid returned value
ioctlReadRet :: forall a m.
   ( MonadInIO m
   , Storable a
   ) => CommandType -> CommandNumber -> Handle -> Excepts '[ErrorCode] m (Int64,a)
ioctlReadRet typ nr = ioctlReadCmdRet cmd
   where
      cmd = ioctlCommand Read typ nr (sizeOfT' @a)

-- | Read a storable
ioctlRead ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> Handle -> Excepts '[ErrorCode] m a
ioctlRead typ nr fd = snd <$> ioctlReadRet typ nr fd

-- | Read a storable, use an arbitrary command
ioctlReadCmd ::
   ( Storable a
   , MonadInIO m
   ) => Command -> Handle -> Excepts '[ErrorCode] m a
ioctlReadCmd cmd fd = snd <$> ioctlReadCmdRet cmd fd

-----------------------------------------------------------------------------
-- Write
-----------------------------------------------------------------------------

-- | Write a storable, use an arbitrary command
ioctlWriteCmdRet ::
   ( Storable a
   , MonadInIO m
   ) => Command -> a -> Handle -> Excepts '[ErrorCode] m Int64
ioctlWriteCmdRet cmd a fd =
   with a $ \pa -> ioctl cmd pa fd

-- | Write a storable, return the valid returned value
ioctlWriteRet :: forall a m.
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m Int64
ioctlWriteRet typ nr = ioctlWriteCmdRet cmd
   where
      cmd = ioctlCommand Write typ nr (sizeOfT' @a)

-- | Write a storable
ioctlWrite ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m ()
ioctlWrite typ nr a fd = void (ioctlWriteRet typ nr a fd)

-- | Write a storable, use an arbitrary command
ioctlWriteCmd ::
   ( Storable a
   , MonadInIO m
   ) => Command -> a -> Handle -> Excepts '[ErrorCode] m ()
ioctlWriteCmd cmd a fd = void (ioctlWriteCmdRet cmd a fd)

-- | Build a Write IOCTL where the value is directly passed in the `arg`
-- parameter.
ioctlWriteValue ::
   ( Storable a
   , Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m ()
ioctlWriteValue typ nr arg fd = do
   let cmd = ioctlCommand Write typ nr (sizeOf' arg)
   void (ioctl cmd arg fd)

-----------------------------------------------------------------------------
-- signal (Direction = None)
-----------------------------------------------------------------------------

-- | Signal, use an arbitrary command
ioctlSignalCmdRet ::
   ( Arg a
   , MonadIO m
   ) => Command -> a -> Handle -> Excepts '[ErrorCode] m Int64
ioctlSignalCmdRet cmd a fd = ioctl cmd a fd

-- | Signal, return the valid returned value
ioctlSignalRet ::
   ( Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m Int64
ioctlSignalRet typ nr = ioctlSignalCmdRet (ioctlCommand None typ nr 0)

-- | Signal
ioctlSignal ::
   ( Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Excepts '[ErrorCode] m ()
ioctlSignal typ nr a fd = void (ioctlSignalRet typ nr a fd)

-- | Signal, use an arbitrary command
ioctlSignalCmd ::
   ( Arg a
   , MonadIO m
   ) => Command -> a -> Handle -> Excepts '[ErrorCode] m ()
ioctlSignalCmd cmd a fd = void (ioctlSignalCmdRet cmd a fd)


-----------------------------------------------------------------------------
-- Read buffers
-----------------------------------------------------------------------------

-- | Build a Read ioctl that reads the given number of bytes
ioctlReadBytes :: MonadIO m => CommandType -> CommandNumber -> Word -> Ptr a -> Handle -> Excepts '[ErrorCode] m Int64
ioctlReadBytes typ nr n ptr fd = do
   let cmd = ioctlCommand Read typ nr n
   ioctl cmd ptr fd

-- | Build a Read ioctl that reads the given number of bytes and return them in
-- a Buffer
ioctlReadBuffer :: MonadInIO m => CommandType -> CommandNumber -> Word -> Handle -> Excepts '[ErrorCode] m (Int64, Buffer)
ioctlReadBuffer typ nr n fd =
   allocaBytes n $ \ptr -> do
      v <- ioctlReadBytes typ nr n ptr fd
      (v,) <$> liftIO (bufferPackPtr (fromIntegral n) ptr)

-- | Build a Read ioctl for variable sized buffers. We expect the ioctl to
-- return the length of the data that can be read. We first try to read with a
-- buffer of `defn` bytes. If there are data left, we retry with a buffer of the
-- appropriate size.
ioctlReadVariableBuffer ::
   ( MonadInIO m
   ) => CommandType -> CommandNumber -> (Word -> Ptr a -> m b) -> Word -> Handle -> Excepts '[ErrorCode] m b
ioctlReadVariableBuffer typ nr f n fd = do
   r <- allocaBytes n $ \ptr -> do
      len <- ioctlReadBytes typ nr n ptr fd
      if len <= fromIntegral n
         then Right <$> lift (f n ptr)
         else return (Left len)

   case r of
      Right b  -> return b
      -- try with the returned buffer size
      Left len -> ioctlReadVariableBuffer typ nr f (fromIntegral len) fd
