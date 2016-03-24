module ViperVM.Arch.Linux.Power
   ( disableRebootKeys
   , enableRebootKeys
   , halt
   , executeLoadedKernel
   , powerOff
   , restart
   , restartWithCommand
   , softSuspend
   )
where

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Internals.Reboot

-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: SysRet ()
disableRebootKeys = sysPower PowerDisableRebootKeys

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: SysRet ()
enableRebootKeys = sysPower PowerEnableRebootKeys

-- | Stop OS and give system control to ROM monitor, if any.
halt :: Sys ()
halt = sysCallAssert "Halting" $ sysPower PowerHalt

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: SysRet ()
executeLoadedKernel = sysPower PowerKernelExec

-- | Stop OS and remove all power from system, if possible.
powerOff :: Sys ()
powerOff = sysCallAssert "Powering off" $ sysPower PowerOff

-- | Restart system using default command and mode.
restart :: Sys ()
restart = sysCallAssert "Restarting" $ sysPower PowerRestart

-- | Restart system using given command string.
restartWithCommand :: String -> SysRet ()
restartWithCommand = sysPower . PowerRestartCommand

-- | Suspend system using software suspend if compiled in.
softSuspend :: SysRet ()
softSuspend = sysPower PowerSoftSuspend
