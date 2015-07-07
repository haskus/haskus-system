module ViperVM.Arch.Linux.FileSystem.Flush
   ( flush
   )
where

import ViperVM.Arch.Linux.FileSystem (sysSync)
import ViperVM.Arch.Linux.ErrorCode

-- | Flush all buffered modifications to file metadata and data to the
-- underlying filesystems
flush :: IO ()
flush = runCatchFail $ do
   sysTry "flush files on disk" $ sysSync
