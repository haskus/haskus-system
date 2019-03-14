{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

-- | Read/write
module Haskus.System.Linux.FileSystem.ReadWrite
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

import Foreign.Ptr
import Foreign.Marshal.Alloc (mallocBytes,free)
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word (Word64, Word32)
import Haskus.Format.Binary.Bits (shiftR)
import Haskus.Format.Binary.Buffer
import Haskus.System.Linux.Error
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Internals.FileSystem (IOVec (..), maxIOVec)
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
sysRead :: MonadIO m => Handle -> Ptr () -> Word64 -> Excepts ReadErrors m Word64
sysRead (Handle fd) ptr count = do
   r <- liftIO (syscall_read fd ptr count)
   n <- checkErrorCode r
         |> catchLiftLeft \case
               EAGAIN -> throwE RetryLater
               EBADF  -> throwE InvalidHandle
               EFAULT -> throwE MemoryError
               -- We shouldn't use blocking calls with the primop "read" syscall,
               -- hence we shouldn't be interrupted
               EINTR  -> throwE Interrupted
               EINVAL -> throwE InvalidParam
               EIO    -> throwE FileSystemIOError
               EISDIR -> throwE InvalidIsDirectory
               err    -> throwE err -- other errors may occur, depending on fd
   return (fromIntegral n)

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
sysReadWithOffset :: MonadIO m => Handle -> Word64 -> Ptr () -> Word64 -> Excepts ReadErrors' m Word64
sysReadWithOffset (Handle fd) offset ptr count = do
   r <- liftIO (syscall_pread64 fd ptr count offset)
   n <- checkErrorCode r
         |> catchLiftLeft \case
               EAGAIN    -> throwE RetryLater
               EBADF     -> throwE InvalidHandle
               ESPIPE    -> throwE InvalidHandle
               EFAULT    -> throwE MemoryError
               -- We shouldn't use blocking calls with the primop "read" syscall,
               -- hence we shouldn't be interrupted
               EINTR     -> throwE Interrupted
               EINVAL    -> throwE InvalidParam
               EIO       -> throwE FileSystemIOError
               EISDIR    -> throwE InvalidIsDirectory
               ENXIO     -> throwE InvalidRange
               EOVERFLOW -> throwE Overflow
               err       -> throwE err -- other errors may occur, depending on fd
   return (fromIntegral n)

-- | Read "count" bytes from a handle (starting at optional "offset") and put
-- them at "ptr" (allocated memory should be large enough).  Returns the number
-- of bytes read or 0 if end of file
handleRead :: MonadIO m => Handle -> Maybe Word64 -> Ptr () -> Word64 -> Excepts ReadErrors' m Word64
handleRead hdl Nothing ptr sz       = liftE <| sysRead hdl ptr sz
handleRead hdl (Just offset) ptr sz = sysReadWithOffset hdl offset ptr sz

-- | Read n bytes in a buffer
handleReadBuffer :: MonadIO m => Handle -> Maybe Word64 -> Word64 -> Excepts ReadErrors' m Buffer
handleReadBuffer hdl offset size = do
   b <- liftIO <| mallocBytes (fromIntegral size)
   sz <- handleRead hdl offset b (fromIntegral size)
         -- free the pointer on error
         |> onE_ (liftIO (free b))
   -- otherwise return the buffer
   bufferUnsafePackPtr (fromIntegral sz) (castPtr b)


-- | Like read but uses several buffers
sysReadMany :: MonadInIO m => Handle -> [(Ptr a, Word64)] -> Excepts '[ErrorCode] m Word64
sysReadMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' -> do
      r <- liftIO (syscall_readv fd (castPtr bufs') count)
      n <- checkErrorCode r
      return (fromIntegral n)

-- | Like readMany, with additional offset in file
sysReadManyWithOffset :: MonadInIO m => Handle -> Word64 -> [(Ptr a, Word64)] -> Excepts '[ErrorCode] m Word64
sysReadManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' -> do
      r <- liftIO (syscall_preadv fd (castPtr bufs') count ol oh)
      n <- checkErrorCode r
      return (fromIntegral n)

-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: MonadIO m => Handle -> Ptr a -> Word64 -> Excepts '[ErrorCode] m Word64
sysWrite (Handle fd) buf count = do
   r <- liftIO (syscall_write fd (castPtr buf) count)
   n <- checkErrorCode r
   return (fromIntegral n)

-- | Write a file descriptor at a given position
sysWriteWithOffset :: MonadIO m => Handle -> Word64 -> Ptr () -> Word64 -> Excepts '[ErrorCode] m Word64
sysWriteWithOffset (Handle fd) offset buf count = do
   r <- liftIO (syscall_pwrite64 fd buf count offset)
   n <- checkErrorCode r
   return (fromIntegral n)


-- | Like write but uses several buffers
sysWriteMany :: MonadInIO m => Handle -> [(Ptr a, Word64)] -> Excepts '[ErrorCode] m Word64
sysWriteMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count       = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' -> do
      r <- liftIO (syscall_writev fd (castPtr bufs') count)
      n <- checkErrorCode r
      return (fromIntegral n)

-- | Like writeMany, with additional offset in file
sysWriteManyWithOffset :: MonadInIO m => Handle -> Word64 -> [(Ptr a, Word64)] -> Excepts '[ErrorCode] m Word64
sysWriteManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' -> do
      r <- liftIO (syscall_pwritev fd (castPtr bufs') count ol oh)
      n <- checkErrorCode r
      return (fromIntegral n)

-- | Write a buffer
writeBuffer :: MonadInIO m => Handle -> Buffer -> Excepts '[ErrorCode] m ()
writeBuffer fd bs = bufferUnsafeUsePtr bs go
   where
      go _ 0     = return ()
      go ptr len = do
         c <- sysWrite fd ptr (fromIntegral len)
         -- if we are interrupted, continue with the remaining bytes to write
         go (ptr `plusPtr` fromIntegral c) (len - fromIntegral c)
