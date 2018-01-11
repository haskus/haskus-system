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
readSymbolicLink :: MonadInIO m => Maybe Handle -> FilePath -> Flow m (String ': ReadSymLinkErrors)
readSymbolicLink hdl path = do
   sysReadLinkAt hdl path
      >%~^> \case
         EACCES       -> flowSet NotAllowed
         EINVAL       -> flowSet NotSymbolicLink
         EIO          -> flowSet FileSystemIOError
         ELOOP        -> flowSet SymbolicLinkLoop
         ENAMETOOLONG -> flowSet TooLongPathName
         ENOENT       -> flowSet FileNotFound
         ENOMEM       -> flowSet OutOfKernelMemory
         ENOTDIR      -> flowSet InvalidPathComponent
         EBADF        -> error "readSymbolicLink: invalid handle"
         -- EFAULT: shouldn't happen (or is a Haskus bug)
         e            -> unhdlErr "readSymbolicLink" e


-- | Wrapper for readlinkat syscall
sysReadLinkAt :: MonadInIO m => Maybe Handle -> FilePath -> Flow m '[String,ErrorCode]
sysReadLinkAt hdl path = go' 2048
   where
      -- if no handle is passed, we assume the path is absolute and we give a
      -- (-1) file descriptor which should be ignored. If the path is relative,
      -- hopefully we will get a EBADF error
      fd = case hdl of
            Just (Handle x) -> x
            Nothing         -> maxBound

      go' size = go size >.~^> \case
                  Nothing -> go' (2*size)
                  Just s  -> flowSetN @0 s

      go size =
         allocaBytes size $ \ptr ->
            withCString path $ \path' ->
               liftIO (syscall_readlinkat fd path' ptr (fromIntegral size))
                  ||> toErrorCode
                  >.~.> (\n ->
                     if fromIntegral n == size
                        then return Nothing
                        else Just <$> peekCStringLen (fromIntegral n) ptr)

-- | Create a symbolic link
sysSymlink :: MonadInIO m => FilePath -> FilePath -> Flow m '[(),ErrorCode]
sysSymlink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         liftIO (syscall_symlink src' dest')
            ||> toErrorCodeVoid

