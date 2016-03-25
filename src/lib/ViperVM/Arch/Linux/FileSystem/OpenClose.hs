{-# LANGUAGE LambdaCase #-}
module ViperVM.Arch.Linux.FileSystem.OpenClose
   ( withOpenAt
   , HandleFlag(..)
   , Device(..)
   , FilePermission(..)
   )
where

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileSystem
import ViperVM.System.Sys

withOpenAt :: FileDescriptor -> FilePath -> HandleFlags -> FilePermissions -> (FileDescriptor -> Sys a) -> Sys (Either ErrorCode a)
withOpenAt fd path flags perm act = do
   fd1 <- sysCallWarn "Open file" $ sysOpenAt fd path flags perm
   case fd1 of
      Left err  -> return (Left err)
      Right fd2 -> do
         res <- act fd2
         sysCallAssert "Close file" $ sysClose fd2
         return (Right res)

