{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Read/write
module Haskus.Arch.Linux.FileSystem.ReadWrite
   ( IOVec(..)
   , maxIOVec
   -- * Read
   , ReadErrors
   , ReadErrors'
   , sysRead
   , sysReadWithOffset
   , sysReadMany
   , sysReadManyWithOffset
   , handleRead
   , handleReadBuffer
   -- * Write
   , sysWrite
   , sysWriteWithOffset
   , sysWriteMany
   , sysWriteManyWithOffset
   , writeBuffer
   )
where

import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word (Word64, Word32)
import Haskus.Format.Binary.Bits (shiftR)
import Haskus.Format.Binary.Buffer
import Haskus.Arch.Linux.Error
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Syscalls
import Haskus.Arch.Linux.Internals.FileSystem (IOVec (..), maxIOVec)
import Haskus.Utils.Flow

type ReadErrors
   = '[ RetryLater
      , InvalidHandle
      , MemoryError
      , Interrupted
      , InvalidParam
      , FileSystemIOError
      , InvalidIsDirectory
      , ErrorCode
      ]

-- | Read cound bytes from the given file descriptor and put them in "buf"
-- Returns the number of bytes read or 0 if end of file
sysRead :: MonadIO m => Handle -> Ptr () -> Word64 -> Flow m (Word64 ': ReadErrors)
sysRead (Handle fd) ptr count =
   liftIO (syscall @"read" fd ptr count)
      ||> toErrorCodePure fromIntegral
      >..%~^> \case
         EAGAIN -> flowSet RetryLater
         EBADF  -> flowSet InvalidHandle
         EFAULT -> flowSet MemoryError
         -- We shouldn't use blocking calls with the primop "read" syscall,
         -- hence we shouldn't be interrupted
         EINTR  -> flowSet Interrupted
         EINVAL -> flowSet InvalidParam
         EIO    -> flowSet FileSystemIOError
         EISDIR -> flowSet InvalidIsDirectory
         err    -> flowSet err -- other errors may occur, depending on fd

type ReadErrors'
   = '[ RetryLater
      , InvalidHandle
      , MemoryError
      , Interrupted
      , InvalidParam
      , FileSystemIOError
      , InvalidIsDirectory
      , ErrorCode
      , InvalidRange
      , Overflow
      ]

-- | Read a file descriptor at a given position
sysReadWithOffset :: MonadIO m => Handle -> Word64 -> Ptr () -> Word64 -> Flow m (Word64 ': ReadErrors')
sysReadWithOffset (Handle fd) offset ptr count =
   liftIO (syscall @"pread64" fd ptr count offset)
      ||> toErrorCodePure fromIntegral
      >..%~^> \case
         EAGAIN    -> flowSet RetryLater
         EBADF     -> flowSet InvalidHandle
         ESPIPE    -> flowSet InvalidHandle
         EFAULT    -> flowSet MemoryError
         -- We shouldn't use blocking calls with the primop "read" syscall,
         -- hence we shouldn't be interrupted
         EINTR     -> flowSet Interrupted
         EINVAL    -> flowSet InvalidParam
         EIO       -> flowSet FileSystemIOError
         EISDIR    -> flowSet InvalidIsDirectory
         ENXIO     -> flowSet InvalidRange
         EOVERFLOW -> flowSet Overflow
         err       -> flowSet err -- other errors may occur, depending on fd

-- | Read "count" bytes from a handle (starting at optional "offset") and put
-- them at "ptr" (allocated memory should be large enough).  Returns the number
-- of bytes read or 0 if end of file
handleRead :: MonadIO m => Handle -> Maybe Word64 -> Ptr () -> Word64 -> Flow m (Word64 ': ReadErrors')
handleRead hdl Nothing ptr sz       = flowLift (sysRead hdl ptr sz)
handleRead hdl (Just offset) ptr sz = sysReadWithOffset hdl offset ptr sz

-- | Read n bytes in a buffer
handleReadBuffer :: MonadIO m => Handle -> Maybe Word64 -> Word64 -> Flow m (Buffer ': ReadErrors')
handleReadBuffer hdl offset size = do
   b <- mallocBytes (fromIntegral size)
   handleRead hdl offset b (fromIntegral size)
      -- free the pointer on error
      >..~=> const (free b)
      -- otherwise return the buffer
      >.~.> \sz -> bufferUnsafePackPtr (fromIntegral sz) (castPtr b)


-- | Like read but uses several buffers
sysReadMany :: MonadInIO m => Handle -> [(Ptr a, Word64)] -> Flow m '[Word64,ErrorCode]
sysReadMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      liftIO (syscall @"readv" fd (castPtr bufs') count)
         ||> toErrorCodePure fromIntegral

-- | Like readMany, with additional offset in file
sysReadManyWithOffset :: MonadInIO m => Handle -> Word64 -> [(Ptr a, Word64)] -> Flow m '[Word64,ErrorCode]
sysReadManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      liftIO (syscall @"preadv" fd (castPtr bufs') count ol oh)
         ||> toErrorCodePure fromIntegral

-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: MonadIO m => Handle -> Ptr a -> Word64 -> Flow m '[Word64,ErrorCode]
sysWrite (Handle fd) buf count =
   liftIO (syscall @"write" fd (castPtr buf) count)
      ||> toErrorCodePure fromIntegral

-- | Write a file descriptor at a given position
sysWriteWithOffset :: MonadIO m => Handle -> Word64 -> Ptr () -> Word64 -> Flow m '[Word64,ErrorCode]
sysWriteWithOffset (Handle fd) offset buf count =
   liftIO (syscall @"pwrite64" fd buf count offset)
      ||> toErrorCodePure fromIntegral


-- | Like write but uses several buffers
sysWriteMany :: MonadInIO m => Handle -> [(Ptr a, Word64)] -> Flow m '[Word64,ErrorCode]
sysWriteMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count       = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      liftIO (syscall @"writev" fd (castPtr bufs') count)
         ||> toErrorCodePure fromIntegral

-- | Like writeMany, with additional offset in file
sysWriteManyWithOffset :: MonadInIO m => Handle -> Word64 -> [(Ptr a, Word64)] -> Flow m '[Word64,ErrorCode]
sysWriteManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      liftIO (syscall @"pwritev" fd (castPtr bufs') count ol oh)
         ||> toErrorCodePure fromIntegral

-- | Write a buffer
writeBuffer :: MonadInIO m => Handle -> Buffer -> Flow m '[(),ErrorCode]
writeBuffer fd bs = bufferUnsafeUsePtr bs go
   where
      go _ 0     = flowSetN @0 ()
      go ptr len = sysWrite fd ptr (fromIntegral len)
         >.~^> \c -> go (ptr `indexPtr` fromIntegral c)
                        (len - fromIntegral c)
