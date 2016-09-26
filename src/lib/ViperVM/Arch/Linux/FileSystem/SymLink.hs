{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Symbolic links
module ViperVM.Arch.Linux.FileSystem.SymLink
   ( sysSymlink
   , readSymbolicLink
   )
where

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.String

import ViperVM.Utils.Flow
import ViperVM.System.Sys

import Foreign.Marshal.Alloc (allocaBytes)

-- | Read the path in a symbolic link
readSymbolicLink :: 
   ( errs ~ '[ NotAllowed
             , NotSymbolicLink
             , FileSystemIOError
             , SymbolicLinkLoop
             , TooLongPathName
             , FileNotFound
             , OutOfKernelMemory
             , InvalidPathComponent
             ]
   ) => Maybe Handle -> FilePath -> SysV (String ': errs)
readSymbolicLink hdl path = do
   sysIO (sysReadLinkAt hdl path)
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
         -- EFAULT: shouldn't happen (or is a ViperVM bug)
         e            -> unhdlErr "readSymbolicLink" e


-- | Wrapper for readlinkat syscall
sysReadLinkAt :: Maybe Handle -> FilePath -> SysRet String
sysReadLinkAt hdl path = go' 2048
   where
      -- if no handle is passed, we assume the path is absolute and we give a
      -- (-1) file descriptor which should be ignored. If the path is relative,
      -- hopefully we will get a EBADF error
      fd = case hdl of
            Just (Handle x) -> x
            Nothing         -> (-1)

      go' size = go size >.~^> \case
                  Nothing -> go' (2*size)
                  Just s  -> flowRet0 s

      go size =
         allocaBytes size $ \ptr ->
            withCString path $ \path' ->
               onSuccessIO (syscall_readlinkat fd path' ptr (fromIntegral size)) $ \n ->
                  if fromIntegral n == size
                     then return Nothing
                     else Just <$> peekCStringLen (ptr, fromIntegral n)

-- | Create a symbolic link
sysSymlink :: FilePath -> FilePath -> SysRet ()
sysSymlink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall_symlink src' dest') (const ())

