{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | File control (fcntl)
module ViperVM.Arch.Linux.Internals.Fcntl
   ( sysFcntl
   , FcntlCommand (..)
   )
where

import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Internals.Handle
import ViperVM.Arch.Linux.Internals.Arg
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word

-- =============================================================
--    From linux/include/uapi/asm-generic/fcntl.h
-- =============================================================

-- | File control command
data FcntlCommand
   = FcntlDupHandle        -- ^ dup
   | FcntlGetHandle        -- ^ get close_on_exec
   | FcntlSetHandle        -- ^ set/clear close_on_exec
   | FcntlGetFlags         -- ^ get flags
   | FcntlSetFlags
   | FcntlGetLock
   | FcntlSetLock
   | FcntlSetLockWait
   | FcntlSetSocketOwner
   | FcntlGetSocketOwner
   | FcntlSetSocketSignal
   | FcntlGetSocketSignal
   | FcntlGetLock64
   | FcntlSetLock64
   | FcntlSetLockWait64
   | FcntlSetSockerOwnerEx
   | FcntlGetSocketOwnerEx
   | FcntlGetOwnerUIDs
   deriving (Show,Eq,Enum,CEnum)

-- | Fcntl syscall
sysFcntl :: Arg a => Handle -> FcntlCommand -> a -> IO Int64
sysFcntl (Handle fd) cmd arg = syscall @"fcntl" fd (fromCEnum cmd) (toArg arg)
