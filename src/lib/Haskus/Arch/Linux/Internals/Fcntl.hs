{-# LANGUAGE DeriveAnyClass #-}

-- | File control (fcntl)
module Haskus.Arch.Linux.Internals.Fcntl
   ( FcntlCommand (..)
   )
where

import Haskus.Format.Binary.Enum

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
