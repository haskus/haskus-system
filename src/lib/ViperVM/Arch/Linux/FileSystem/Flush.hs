module ViperVM.Arch.Linux.FileSystem.Flush
   ( flush
   )
where

import ViperVM.System.Sys
import ViperVM.Arch.Linux.FileSystem (sysSync)
import ViperVM.Arch.Linux.Syscalls

-- | Flush all buffered modifications to file metadata and data to the
-- underlying filesystems
flush :: Sys ()
flush = sysCallAssert "Flush files on disk" $ sysSync
