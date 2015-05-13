module ViperVM.Arch.X86_64.Linux.FileSystem.Mount
   ( sysMount
   , sysUnmount
   )
where

import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr)

import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem.Mount (MountFlags,UnmountFlags)
import ViperVM.Arch.X86_64.Linux.Syscall

-- | Mount a file system
sysMount :: String -> String -> String -> MountFlags -> Ptr () -> SysRet ()
sysMount source target fstype flags dat = do
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            onSuccess (syscall5 165 source' target' fstype' (BitSet.toBits flags) dat) (const ())


-- | Unmount a file system
sysUnmount :: String -> UnmountFlags -> SysRet ()
sysUnmount target flags =
   withCString target $ \target' ->
      onSuccess (syscall2 166 target' (BitSet.toBits flags)) (const ())
