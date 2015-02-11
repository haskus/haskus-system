module ViperVM.Arch.X86_64.Linux.Power
   ( PowerCommand(..)
   , sysPower
   )
where

import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import ViperVM.Arch.Linux.ErrorCode
import Foreign.C.String (withCString)
import ViperVM.Arch.X86_64.Linux.Syscall

data PowerCommand
   = PowerDisableRebootKeys
   | PowerEnableRebootKeys
   | PowerHalt
   | PowerKernelExec
   | PowerOff
   | PowerRestart
   | PowerRestartCommand
   | PowerHibernate

instance Enum PowerCommand where
   fromEnum x = case x of
      PowerDisableRebootKeys  -> 0x00000000
      PowerEnableRebootKeys   -> 0x89ABCDEF
      PowerHalt               -> 0xCDEF0123
      PowerKernelExec         -> 0x45584543
      PowerOff                -> 0x4321FEDC
      PowerRestart            -> 0x01234567
      PowerRestartCommand     -> 0xA1B2C3D4
      PowerHibernate          -> 0xD000FCE2

   toEnum x = case x of
      0x00000000 -> PowerDisableRebootKeys
      0x89ABCDEF -> PowerEnableRebootKeys
      0xCDEF0123 -> PowerHalt
      0x45584543 -> PowerKernelExec
      0x4321FEDC -> PowerOff
      0x01234567 -> PowerRestart
      0xA1B2C3D4 -> PowerRestartCommand
      0xD000FCE2 -> PowerHibernate
      _          -> error "Unknown power command"


-- | reboot syscall
sysPower :: PowerCommand -> Maybe String -> SysRet ()
sysPower cmd customCommand =
   let
      c = fromMaybe "" customCommand
      magic1 = 0xfee1dead :: Word64
      magic2 = 0x28121969 :: Word64
   in
      withCString c $ \c' ->
         onSuccess (syscall4 169 magic1 magic2 (fromEnum cmd) c') (const ())
