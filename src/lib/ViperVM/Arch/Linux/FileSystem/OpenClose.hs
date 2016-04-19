{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Open/Close
module ViperVM.Arch.Linux.FileSystem.OpenClose
   ( withOpenAt
   , HandleFlag(..)
   , Device(..)
   , FilePermission(..)
   )
where

import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileSystem
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Utils.HList

import GHC.TypeLits

-- | Open at
withOpenAt :: 
   ( KnownNat (Length xs)
   ) => Handle -> FilePath -> HandleFlags -> FilePermissions -> (Handle -> Flow Sys xs) -> Flow Sys (Concat xs '[ErrorCode])
withOpenAt fd path flags perm act =
   sysCallWarn "Open file" (sysOpenAt fd path flags perm)
      >.~:> \fd1 -> do
         res <- act fd1
         sysCallAssert "Close file" $ sysClose fd1
         return res

