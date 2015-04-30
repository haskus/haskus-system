module ViperVM.Arch.X86_64.Linux.Power
   ( syscall_reboot
   )
where

import Data.Word (Word64)
import Data.Int (Int64)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)
import ViperVM.Arch.X86_64.Linux.Syscall

-- | Reboot syscall
syscall_reboot :: Word64 -> Word64 -> Word64 -> Ptr CChar -> IO Int64
syscall_reboot magic1 magic2 cmd cmdPath = syscall4 169 magic1 magic2 cmd cmdPath

{-# INLINE syscall_reboot #-}
