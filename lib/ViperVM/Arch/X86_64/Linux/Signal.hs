module ViperVM.Arch.X86_64.Linux.Signal (
   sysPause, sysAlarm
) where

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode

import Data.Word

sysPause :: SysRet ()
sysPause = onSuccess (syscall0 34) (const ())

sysAlarm :: Word-> SysRet Word
sysAlarm seconds =
   onSuccess (syscall1 37 seconds) fromIntegral
