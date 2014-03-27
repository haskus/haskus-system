module ViperVM.Arch.X86_64.Linux.Process (
   sysExit
) where

import Control.Monad (void)
import Data.Int (Int64)

import ViperVM.Arch.X86_64.Linux.Syscall

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall1 60 n)
