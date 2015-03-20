-- | SysFS wrapper
module ViperVM.Arch.Linux.System.SysFS
   ( SysFS(..)
   )
where

import ViperVM.Arch.Linux.FileDescriptor

data SysFS = SysFS
   { sysfsDescriptor :: FileDescriptor
   }
