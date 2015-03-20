{-# LANGUAGE LambdaCase #-}
module ViperVM.Arch.Linux.FileSystem.OpenClose
   ( withOpenAt
   , OpenFlag(..)
   , Device(..)
   , FilePermission(..)
   )
where

import Control.Monad (void)

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.FileSystem

withOpenAt :: FileDescriptor -> FilePath -> [OpenFlag] -> [FilePermission] -> (FileDescriptor -> SysRet a) -> SysRet a
withOpenAt fd path flags perm act = do
   sysOpenAt fd path flags perm >>= \case
      Left err  -> return (Left err)
      Right fd2 -> do
         res <- act fd2
         void (sysClose fd2)
         return res

