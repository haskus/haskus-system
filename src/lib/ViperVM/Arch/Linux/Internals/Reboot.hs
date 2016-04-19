-- | Linux 'reboot' syscall
module ViperVM.Arch.Linux.Internals.Reboot
   ( PowerCommand(..)
   , sysPower
   )
where

import Data.Word (Word64)
import ViperVM.Arch.Linux.ErrorCode
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)

import ViperVM.Arch.Linux.Syscalls

-- =============================================================
--    From linux/include/uapi/linux/reboot.h
-- =============================================================

-- | Commands supported by the reboot syscall
data PowerCommand
   = PowerDisableRebootKeys     -- ^ Ctrl-Alt-Del sequence sends SIGINT to init task.
   | PowerEnableRebootKeys      -- ^ Ctrl-Alt-Del sequence causes RESTART command.
   | PowerHalt                  -- ^ Stop OS and give system control to ROM monitor, if any.
   | PowerKernelExec            -- ^ Restart system using a previously loaded Linux kernel
   | PowerOff                   -- ^ Stop OS and remove all power from system, if possible.
   | PowerRestart               -- ^ Restart system using default command and mode.
   | PowerRestartCommand String -- ^ Restart system using given command string.
   | PowerSoftSuspend           -- ^ Suspend system using software suspend if compiled in.
   deriving (Show,Eq)

fromPowerCommand :: PowerCommand -> Word64
fromPowerCommand x = case x of
   PowerDisableRebootKeys  -> 0x00000000
   PowerEnableRebootKeys   -> 0x89ABCDEF
   PowerHalt               -> 0xCDEF0123
   PowerKernelExec         -> 0x45584543
   PowerOff                -> 0x4321FEDC
   PowerRestart            -> 0x01234567
   PowerRestartCommand _   -> 0xA1B2C3D4
   PowerSoftSuspend        -> 0xD000FCE2

-- | reboot syscall
sysPower :: PowerCommand -> SysRet ()
sysPower cmd = case cmd of
      PowerRestartCommand cmdPath -> withCString cmdPath f
      _                           -> f nullPtr
   where
      f path = onSuccessVoid (syscall_reboot magic1 magic2 cmd' path)
      magic1 = 0xfee1dead :: Word64
      magic2 = 0x28121969 :: Word64
      cmd'   = fromPowerCommand cmd


