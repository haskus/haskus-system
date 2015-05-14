module ViperVM.Arch.Linux.Power
   ( PowerCommand(..)
   , sysPower
   )
where

import Data.Word (Word64)
import ViperVM.Arch.Linux.ErrorCode
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)

import ViperVM.Arch.Linux.Syscalls

data PowerCommand
   = PowerDisableRebootKeys
   | PowerEnableRebootKeys
   | PowerHalt
   | PowerKernelExec
   | PowerOff
   | PowerRestart
   | PowerRestartCommand String
   | PowerHibernate

fromPowerCommand :: PowerCommand -> Word64
fromPowerCommand x = case x of
   PowerDisableRebootKeys  -> 0x00000000
   PowerEnableRebootKeys   -> 0x89ABCDEF
   PowerHalt               -> 0xCDEF0123
   PowerKernelExec         -> 0x45584543
   PowerOff                -> 0x4321FEDC
   PowerRestart            -> 0x01234567
   PowerRestartCommand _   -> 0xA1B2C3D4
   PowerHibernate          -> 0xD000FCE2

-- | reboot syscall
sysPower :: PowerCommand -> SysRet ()
sysPower cmd = case cmd of
      PowerRestartCommand cmdPath -> withCString cmdPath f
      _                           -> f nullPtr
   where
      f path = onSuccess (syscall_reboot magic1 magic2 cmd' path) (const ())
      magic1 = 0xfee1dead :: Word64
      magic2 = 0x28121969 :: Word64
      cmd' = fromPowerCommand cmd
