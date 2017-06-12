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
module Haskus.Arch.Linux.Ioctl
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

import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.Arch.Linux.Syscalls
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Internals.Ioctl
import Haskus.Arch.Linux.Internals.Arg
import Haskus.Utils.Flow

---------------------------------------------------
-- IOCTL
---------------------------------------------------

-- | Send a custom command to a device
ioctl :: (Arg a, MonadIO m) => Command -> a -> Handle -> Flow m '[Int64,ErrorCode]
ioctl (Command cmd) arg (Handle fd) =
   liftIO (syscall_ioctl fd (fromIntegral (bitFieldsBits cmd)) (toArg arg))
      ||> toErrorCode

-----------------------------------------------------------------------------
-- Write/Read
-----------------------------------------------------------------------------

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmdRet :: (Storable a, MonadInIO m) => Command -> a -> Handle -> Flow m '[(Int64,a),ErrorCode]
ioctlWriteReadCmdRet cmd a fd =
   with a $ \pa ->
      ioctl cmd pa fd >.~.> \r ->
         (r,) <$> peek pa

-- | Write and read a storable, return the valid returned value
ioctlWriteReadRet :: forall a m.
   ( MonadInIO m
   , Storable a
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[(Int64,a),ErrorCode]
ioctlWriteReadRet typ nr = ioctlWriteReadCmdRet cmd
   where
      cmd = ioctlCommand WriteRead typ nr (sizeOfT' @a)

-- | Write and read a storable
ioctlWriteRead ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[a,ErrorCode]
ioctlWriteRead typ nr a fd = ioctlWriteReadRet typ nr a fd >.-.> snd

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmd ::
   ( MonadInIO m
   , Storable a
   ) => Command -> a -> Handle -> Flow m '[a,ErrorCode]
ioctlWriteReadCmd cmd a fd = ioctlWriteReadCmdRet cmd a fd >.-.> snd
      
-----------------------------------------------------------------------------
-- Read
-----------------------------------------------------------------------------

-- | Read a storable, use an arbitrary command
ioctlReadCmdRet ::
   ( MonadInIO m
   , Storable a
   ) => Command -> Handle -> Flow m '[(Int64,a),ErrorCode]
ioctlReadCmdRet cmd fd =
   alloca $ \pa -> do
      ioctl cmd pa fd >.~.> \r ->
         (r,) <$> peek pa

-- | Read a storable, return the valid returned value
ioctlReadRet :: forall a m.
   ( MonadInIO m
   , Storable a
   ) => CommandType -> CommandNumber -> Handle -> Flow m '[(Int64,a),ErrorCode]
ioctlReadRet typ nr = ioctlReadCmdRet cmd
   where
      cmd = ioctlCommand Read typ nr (sizeOfT' @a)

-- | Read a storable
ioctlRead ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> Handle -> Flow m '[a,ErrorCode]
ioctlRead typ nr fd = ioctlReadRet typ nr fd >.-.> snd

-- | Read a storable, use an arbitrary command
ioctlReadCmd ::
   ( Storable a
   , MonadInIO m
   ) => Command -> Handle -> Flow m '[a,ErrorCode]
ioctlReadCmd cmd fd = ioctlReadCmdRet cmd fd >.-.> snd

-----------------------------------------------------------------------------
-- Write
-----------------------------------------------------------------------------

-- | Write a storable, use an arbitrary command
ioctlWriteCmdRet ::
   ( Storable a
   , MonadInIO m
   ) => Command -> a -> Handle -> Flow m '[Int64,ErrorCode]
ioctlWriteCmdRet cmd a fd =
   with a $ \pa -> ioctl cmd pa fd

-- | Write a storable, return the valid returned value
ioctlWriteRet :: forall a m.
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[Int64,ErrorCode]
ioctlWriteRet typ nr = ioctlWriteCmdRet cmd
   where
      cmd = ioctlCommand Write typ nr (sizeOfT' @a)

-- | Write a storable
ioctlWrite ::
   ( Storable a
   , MonadInIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[(),ErrorCode]
ioctlWrite typ nr a fd = ioctlWriteRet typ nr a fd >.-.> const ()

-- | Write a storable, use an arbitrary command
ioctlWriteCmd ::
   ( Storable a
   , MonadInIO m
   ) => Command -> a -> Handle -> Flow m '[(),ErrorCode]
ioctlWriteCmd cmd a fd = ioctlWriteCmdRet cmd a fd >.-.> const ()

-- | Build a Write IOCTL where the value is directly passed in the `arg`
-- parameter.
ioctlWriteValue ::
   ( Storable a
   , Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[(),ErrorCode]
ioctlWriteValue typ nr arg fd = do
   let cmd = ioctlCommand Write typ nr (sizeOf' arg)
   ioctl cmd arg fd >.-.> const ()

-----------------------------------------------------------------------------
-- signal (Direction = None)
-----------------------------------------------------------------------------

-- | Signal, use an arbitrary command
ioctlSignalCmdRet ::
   ( Arg a
   , MonadIO m
   ) => Command -> a -> Handle -> Flow m '[Int64,ErrorCode]
ioctlSignalCmdRet cmd a fd = ioctl cmd a fd

-- | Signal, return the valid returned value
ioctlSignalRet ::
   ( Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[Int64,ErrorCode]
ioctlSignalRet typ nr = ioctlSignalCmdRet (ioctlCommand None typ nr 0)

-- | Signal
ioctlSignal ::
   ( Arg a
   , MonadIO m
   ) => CommandType -> CommandNumber -> a -> Handle -> Flow m '[(),ErrorCode]
ioctlSignal typ nr a fd = ioctlSignalRet typ nr a fd >.-.> const ()

-- | Signal, use an arbitrary command
ioctlSignalCmd ::
   ( Arg a
   , MonadIO m
   ) => Command -> a -> Handle -> Flow m '[(),ErrorCode]
ioctlSignalCmd cmd a fd = ioctlSignalCmdRet cmd a fd >.-.> const ()


-----------------------------------------------------------------------------
-- Read buffers
-----------------------------------------------------------------------------

-- | Build a Read ioctl that reads the given number of bytes
ioctlReadBytes :: MonadIO m => CommandType -> CommandNumber -> Word -> Ptr a -> Handle -> Flow m '[Int64,ErrorCode]
ioctlReadBytes typ nr n ptr fd = do
   let cmd = ioctlCommand Read typ nr n
   ioctl cmd ptr fd

-- | Build a Read ioctl that reads the given number of bytes and return them in
-- a Buffer
ioctlReadBuffer :: MonadInIO m => CommandType -> CommandNumber -> Word -> Handle -> Flow m '[(Int64, Buffer),ErrorCode]
ioctlReadBuffer typ nr n fd =
   allocaBytes n $ \ptr ->
      ioctlReadBytes typ nr n ptr fd >.~.> \v ->
         (v,) <$> liftIO (bufferPackPtr (fromIntegral n) ptr)

-- | Build a Read ioctl for variable sized buffers. We expect the ioctl to
-- return the length of the data that can be read. We first try to read with a
-- buffer of `defn` bytes. If there are data left, we retry with a buffer of the
-- appropriate size.
ioctlReadVariableBuffer ::
   ( Liftable '[ErrorCode] '[b,ErrorCode]
   , MonadInIO m
   ) => CommandType -> CommandNumber -> (Word -> Ptr a -> m b) -> Word -> Handle -> Flow m '[b,ErrorCode]
ioctlReadVariableBuffer typ nr f n fd = allocaBytes n $ \ptr ->
   ioctlReadBytes typ nr n ptr fd
      >.~^> \len ->
         if len <= fromIntegral n
            then flowSet =<< f n ptr
            -- try with the returned buffer size
            else ioctlReadVariableBuffer typ nr f (fromIntegral len) fd
