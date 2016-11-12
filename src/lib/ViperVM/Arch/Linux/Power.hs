{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Power-off, reboot, etc.
module ViperVM.Arch.Linux.Power
   ( sysPower
   , disableRebootKeys
   , enableRebootKeys
   , halt
   , executeLoadedKernel
   , powerOff
   , restart
   , restartWithCommand
   , softSuspend
   )
where

import ViperVM.Arch.Linux.Internals.Reboot
import ViperVM.Arch.Linux.Internals.Error
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Format.Binary.Word (Word64)
import ViperVM.Format.String (withCString)
import ViperVM.Format.Binary.Ptr (nullPtr)
import ViperVM.System.Sys
import ViperVM.Utils.Flow

-- | reboot syscall
sysPower :: MonadInIO m => PowerCommand -> Flow m '[(),ErrorCode]
sysPower cmd = case cmd of
      PowerRestartCommand cmdPath -> withCString cmdPath f
      _                           -> f nullPtr
   where
      f path = liftIO (syscall @"reboot" magic1 magic2 cmd' path)
                  ||> toErrorCodeVoid
      magic1 = 0xfee1dead :: Word64
      magic2 = 0x28121969 :: Word64
      cmd'   = powerCommandNumber cmd


-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: Flow Sys '[(),NotAllowed]
disableRebootKeys = sysPower PowerDisableRebootKeys >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "disableRebootKeys" e

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: Flow Sys '[(),NotAllowed]
enableRebootKeys = sysPower PowerEnableRebootKeys >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "enableRebootKeys" e

-- | Stop OS and give system control to ROM monitor, if any.
halt :: Flow Sys '[(),NotAllowed]
halt = sysPower PowerHalt >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "halt" e

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: Flow Sys '[(),NotAllowed]
executeLoadedKernel = sysPower PowerKernelExec >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "executeLoadedKernel" e

-- | Stop OS and remove all power from system, if possible.
powerOff :: Flow Sys '[(),NotAllowed]
powerOff = sysPower PowerOff >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "powerOff" e

-- | Restart system using default command and mode.
restart :: Flow Sys '[(),NotAllowed]
restart = sysPower PowerRestart >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "restart" e

-- | Restart system using given command string.
restartWithCommand :: String -> Flow Sys '[(),NotAllowed,MemoryError,InvalidRestartCommand]
restartWithCommand cmd = sysPower (PowerRestartCommand cmd) >%~^> \case
   EPERM  -> flowSet NotAllowed
   EFAULT -> flowSet MemoryError
   EINVAL -> flowSet InvalidRestartCommand
   e      -> unhdlErr "restartWithCommand" e

-- | Suspend system using software suspend if compiled in.
softSuspend :: Flow Sys '[(),NotAllowed]
softSuspend = sysPower PowerSoftSuspend >%~^> \case
   EPERM -> flowSet NotAllowed
   e     -> unhdlErr "softSuspend" e
