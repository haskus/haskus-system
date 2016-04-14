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
sysPower' :: PowerCommand -> Sys (Either ErrorCode ())
sysPower' = sysIO . sysPower

-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: Flow Sys '[(),NotAllowed]
disableRebootKeys = sysPower' PowerDisableRebootKeys >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "disableRebootKeys" e

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: Flow Sys '[(),NotAllowed]
enableRebootKeys = sysPower' PowerEnableRebootKeys >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "enableRebootKeys" e

-- | Stop OS and give system control to ROM monitor, if any.
halt :: Flow Sys '[(),NotAllowed]
halt = sysPower' PowerHalt >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "halt" e

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: Flow Sys '[(),NotAllowed]
executeLoadedKernel = sysPower' PowerKernelExec >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "executeLoadedKernel" e

-- | Stop OS and remove all power from system, if possible.
powerOff :: Flow Sys '[(),NotAllowed]
powerOff = sysPower' PowerOff >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "powerOff" e

-- | Restart system using default command and mode.
restart :: Flow Sys '[(),NotAllowed]
restart = sysPower' PowerRestart >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "restart" e

-- | Restart system using given command string.
restartWithCommand :: String -> Flow Sys '[(),NotAllowed,MemoryError,InvalidRestartCommand]
restartWithCommand cmd = sysPower' (PowerRestartCommand cmd) >>= \case
   Right ()    -> flowRet ()
   Left EPERM  -> flowSet NotAllowed
   Left EFAULT -> flowSet MemoryError
   Left EINVAL -> flowSet InvalidRestartCommand
   Left e      -> unhdlErr "restart" e

-- | Suspend system using software suspend if compiled in.
softSuspend :: Flow Sys '[(),NotAllowed]
softSuspend = sysPower' PowerSoftSuspend >>= \case
   Right ()   -> flowRet ()
   Left EPERM -> flowSet NotAllowed
   Left e     -> unhdlErr "softSuspend" e
