{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Linux system calls (syscalls)
module ViperVM.Arch.Linux.Syscalls
   ( syscall
   )
where

--TODO: use conditional import here when we will support different
--architectures
import ViperVM.Arch.X86_64.Linux.Syscalls
