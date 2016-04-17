{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- | Power-off, reboot, etc.
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
import ViperVM.System.Sys
import ViperVM.Utils.Flow

-- | sysPower lifted in Sys
sysPower' :: PowerCommand -> Flow Sys '[(),ErrorCode]
sysPower' = sysIO . sysPower

-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: Flow Sys '[(),NotAllowed]
disableRebootKeys = sysPower' PowerDisableRebootKeys >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "disableRebootKeys" e

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: Flow Sys '[(),NotAllowed]
enableRebootKeys = sysPower' PowerEnableRebootKeys >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "enableRebootKeys" e

-- | Stop OS and give system control to ROM monitor, if any.
halt :: Flow Sys '[(),NotAllowed]
halt = sysPower' PowerHalt >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "halt" e

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: Flow Sys '[(),NotAllowed]
executeLoadedKernel = sysPower' PowerKernelExec >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "executeLoadedKernel" e

-- | Stop OS and remove all power from system, if possible.
powerOff :: Flow Sys '[(),NotAllowed]
powerOff = sysPower' PowerOff >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "powerOff" e

-- | Restart system using default command and mode.
restart :: Flow Sys '[(),NotAllowed]
restart = sysPower' PowerRestart >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "restart" e

-- | Restart system using given command string.
restartWithCommand :: String -> Flow Sys '[(),NotAllowed,MemoryError,InvalidRestartCommand]
restartWithCommand cmd = sysPower' (PowerRestartCommand cmd) >%~#> \case
   EPERM  -> flowSet NotAllowed
   EFAULT -> flowSet MemoryError
   EINVAL -> flowSet InvalidRestartCommand
   e      -> unhdlErr "restart" e

-- | Suspend system using software suspend if compiled in.
softSuspend :: Flow Sys '[(),NotAllowed]
softSuspend = sysPower' PowerSoftSuspend >%~#> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "softSuspend" e
