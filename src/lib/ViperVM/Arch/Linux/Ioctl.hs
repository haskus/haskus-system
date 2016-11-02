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

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Storable
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Ioctl
import ViperVM.Arch.Linux.Internals.Arg
import ViperVM.Utils.Flow

---------------------------------------------------
-- IOCTL
---------------------------------------------------

-- | Send a custom command to a device
ioctl :: Arg a => Command -> a -> Handle -> IOErr Int64
ioctl (Command cmd) arg (Handle fd) =
   syscall @"ioctl" fd (fromIntegral (bitFieldsBits cmd)) (toArg arg)
      ||> toErrorCode

-----------------------------------------------------------------------------
-- Write/Read
-----------------------------------------------------------------------------

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmdRet :: Storable a => Command -> a -> Handle -> IOErr (Int64,a)
ioctlWriteReadCmdRet cmd a fd =
   with a $ \pa ->
      ioctl cmd pa fd >.~.> \r ->
         (r,) <$> peek pa

-- | Write and read a storable, return the valid returned value
ioctlWriteReadRet :: forall a. Storable a => CommandType -> CommandNumber -> a -> Handle -> IOErr (Int64,a)
ioctlWriteReadRet typ nr = ioctlWriteReadCmdRet cmd
   where
      cmd = ioctlCommand WriteRead typ nr (sizeOfT' @a)

-- | Write and read a storable
ioctlWriteRead :: Storable a => CommandType -> CommandNumber -> a -> Handle -> IOErr a
ioctlWriteRead typ nr a fd = ioctlWriteReadRet typ nr a fd >.-.> snd

-- | Write and read a storable, use an arbitrary command
ioctlWriteReadCmd :: Storable a => Command -> a -> Handle -> IOErr a
ioctlWriteReadCmd cmd a fd = ioctlWriteReadCmdRet cmd a fd >.-.> snd
      
-----------------------------------------------------------------------------
-- Read
-----------------------------------------------------------------------------

-- | Read a storable, use an arbitrary command
ioctlReadCmdRet :: Storable a => Command -> Handle -> IOErr (Int64,a)
ioctlReadCmdRet cmd fd =
   alloca $ \pa -> do
      ioctl cmd pa fd >.~.> \r ->
         (r,) <$> peek pa

-- | Read a storable, return the valid returned value
ioctlReadRet :: forall a. Storable a => CommandType -> CommandNumber -> Handle -> IOErr (Int64,a)
ioctlReadRet typ nr = ioctlReadCmdRet cmd
   where
      cmd = ioctlCommand Read typ nr (sizeOfT' @a)

-- | Read a storable
ioctlRead :: Storable a => CommandType -> CommandNumber -> Handle -> IOErr a
ioctlRead typ nr fd = ioctlReadRet typ nr fd >.-.> snd

-- | Read a storable, use an arbitrary command
ioctlReadCmd :: Storable a => Command -> Handle -> IOErr a
ioctlReadCmd cmd fd = ioctlReadCmdRet cmd fd >.-.> snd

-----------------------------------------------------------------------------
-- Write
-----------------------------------------------------------------------------

-- | Write a storable, use an arbitrary command
ioctlWriteCmdRet :: Storable a => Command -> a -> Handle -> IOErr Int64
ioctlWriteCmdRet cmd a fd =
   with a $ \pa -> ioctl cmd pa fd

-- | Write a storable, return the valid returned value
ioctlWriteRet :: forall a. Storable a => CommandType -> CommandNumber -> a -> Handle -> IOErr Int64
ioctlWriteRet typ nr = ioctlWriteCmdRet cmd
   where
      cmd = ioctlCommand Write typ nr (sizeOfT' @a)

-- | Write a storable
ioctlWrite :: Storable a => CommandType -> CommandNumber -> a -> Handle -> IOErr ()
ioctlWrite typ nr a fd = ioctlWriteRet typ nr a fd >.-.> const ()

-- | Write a storable, use an arbitrary command
ioctlWriteCmd :: Storable a => Command -> a -> Handle -> IOErr ()
ioctlWriteCmd cmd a fd = ioctlWriteCmdRet cmd a fd >.-.> const ()

-- | Build a Write IOCTL where the value is directly passed in the `arg`
-- parameter.
ioctlWriteValue :: (Storable a, Arg a) => CommandType -> CommandNumber -> a -> Handle -> IOErr ()
ioctlWriteValue typ nr arg fd = do
   let cmd = ioctlCommand Write typ nr (sizeOf' arg)
   ioctl cmd arg fd >.-.> const ()

-----------------------------------------------------------------------------
-- signal (Direction = None)
-----------------------------------------------------------------------------

-- | Signal, use an arbitrary command
ioctlSignalCmdRet :: Arg a => Command -> a -> Handle -> IOErr Int64
ioctlSignalCmdRet cmd a fd = ioctl cmd a fd

-- | Signal, return the valid returned value
ioctlSignalRet :: Arg a => CommandType -> CommandNumber -> a -> Handle -> IOErr Int64
ioctlSignalRet typ nr = ioctlSignalCmdRet (ioctlCommand None typ nr 0)

-- | Signal
ioctlSignal :: Arg a => CommandType -> CommandNumber -> a -> Handle -> IOErr ()
ioctlSignal typ nr a fd = ioctlSignalRet typ nr a fd >.-.> const ()

-- | Signal, use an arbitrary command
ioctlSignalCmd :: Arg a => Command -> a -> Handle -> IOErr ()
ioctlSignalCmd cmd a fd = ioctlSignalCmdRet cmd a fd >.-.> const ()


-----------------------------------------------------------------------------
-- Read buffers
-----------------------------------------------------------------------------

-- | Build a Read ioctl that reads the given number of bytes
ioctlReadBytes :: CommandType -> CommandNumber -> Int -> Ptr a -> Handle -> IOErr Int64
ioctlReadBytes typ nr n ptr fd = do
   let cmd = ioctlCommand Read typ nr n
   ioctl cmd ptr fd

-- | Build a Read ioctl that reads the given number of bytes and return them in
-- a Buffer
ioctlReadBuffer :: CommandType -> CommandNumber -> Int -> Handle -> IOErr (Int64, Buffer)
ioctlReadBuffer typ nr n fd =
   allocaBytes n $ \ptr ->
      ioctlReadBytes typ nr n ptr fd >.~.> \v ->
         (v,) <$> bufferPackPtr (fromIntegral n) ptr

-- | Build a Read ioctl for variable sized buffers. We expect the ioctl to
-- return the length of the data that can be read. We first try to read with a
-- buffer of `defn` bytes. If there are data left, we retry with a buffer of the
-- appropriate size.
ioctlReadVariableBuffer ::
   ( Liftable '[ErrorCode] '[b,ErrorCode]
   ) => CommandType -> CommandNumber -> (Int -> Ptr a -> IO b) -> Int -> Handle -> IOErr b
ioctlReadVariableBuffer typ nr f n fd = allocaBytes n $ \ptr ->
   ioctlReadBytes typ nr n ptr fd
      >.~^> \len ->
         if len <= fromIntegral n
            then flowRet0 =<< f n ptr
            -- try with the returned buffer size
            else ioctlReadVariableBuffer typ nr f (fromIntegral len) fd



