-- | Linux system calls (syscalls)
module Haskus.Arch.Linux.Syscalls 
   ( module Arch.Syscalls
   )
   where

--TODO: use conditional import here when we will support different
--architectures
import Haskus.Arch.X86_64.Linux.Syscalls as Arch.Syscalls
