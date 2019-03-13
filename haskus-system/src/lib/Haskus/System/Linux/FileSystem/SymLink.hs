{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Symbolic links
module Haskus.System.Linux.FileSystem.SymLink
   ( sysSymlink
   , ReadSymLinkErrors
   , readSymbolicLink
   )
where

import Haskus.System.Linux.Error
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Syscalls
import Haskus.Format.String
import Haskus.Format.Binary.Storable
import Haskus.Utils.Flow

type ReadSymLinkErrors
   = '[ NotAllowed
      , NotSymbolicLink
      , FileSystemIOError
      , SymbolicLinkLoop
      , TooLongPathName
      , FileNotFound
      , OutOfKernelMemory
      , InvalidPathComponent
      ]

-- | Read the path in a symbolic link
readSymbolicLink :: MonadInIO m => Maybe Handle -> FilePath -> Excepts ReadSymLinkErrors m String
readSymbolicLink hdl path = do
   sysReadLinkAt hdl path
      `catchLiftLeft` \case
         EACCES       -> throwE NotAllowed
         EINVAL       -> throwE NotSymbolicLink
         EIO          -> throwE FileSystemIOError
         ELOOP        -> throwE SymbolicLinkLoop
         ENAMETOOLONG -> throwE TooLongPathName
         ENOENT       -> throwE FileNotFound
         ENOMEM       -> throwE OutOfKernelMemory
         ENOTDIR      -> throwE InvalidPathComponent
         EBADF        -> error "readSymbolicLink: invalid handle"
         -- EFAULT: shouldn't happen (or is a haskus-system bug)
         e            -> unhdlErr "readSymbolicLink" e


-- | Wrapper for readlinkat syscall
sysReadLinkAt :: MonadInIO m => Maybe Handle -> FilePath -> Excepts '[ErrorCode] m String
sysReadLinkAt hdl path = tryReadLinkAt 2048
   where
      -- if no handle is passed, we assume the path is absolute and we give a
      -- (-1) file descriptor which should be ignored. If the path is relative,
      -- hopefully we will get a EBADF error
      fd = case hdl of
            Just (Handle x) -> x
            Nothing         -> maxBound

      -- allocate a buffer and try to readlinkat.
      tryReadLinkAt size = do
         mv <- allocaBytes size $ \ptr ->
                  withCString path $ \path' -> do
                     n <- checkErrorCode =<< liftIO (syscall_readlinkat fd path' ptr (fromIntegral size))
                     if fromIntegral n == size
                        then return Nothing
                        else Just <$> peekCStringLen (fromIntegral n) ptr
         case mv of
            Nothing -> tryReadLinkAt (2*size) -- retry with double buffer size
            Just v  -> return v

-- | Create a symbolic link
sysSymlink :: MonadInIO m => FilePath -> FilePath -> Excepts '[ErrorCode] m ()
sysSymlink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         checkErrorCode_ =<< liftIO (syscall_symlink src' dest')
