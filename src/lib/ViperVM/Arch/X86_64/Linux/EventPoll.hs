module ViperVM.Arch.X86_64.Linux.EventPoll
   ( EventPollFlag(..)
   , sysEventPollCreate
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.Linux.FileDescriptor

import Data.Bits ((.|.))
import Data.List (foldl')
import Data.Word (Word64)

data EventPollFlag
   = EventPollCloseOnExec
   deriving (Show,Eq)

fromFlag :: EventPollFlag -> Word64
fromFlag EventPollCloseOnExec = 0x80000

fromFlags :: [EventPollFlag] -> Word64
fromFlags = foldl' (.|.) 0 . fmap fromFlag

sysEventPollCreate :: [EventPollFlag] -> SysRet FileDescriptor
sysEventPollCreate flags =
   onSuccess (syscall1 291 (fromFlags flags)) (FileDescriptor . fromIntegral)
