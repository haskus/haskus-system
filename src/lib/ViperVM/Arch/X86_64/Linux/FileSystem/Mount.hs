module ViperVM.Arch.X86_64.Linux.FileSystem.Mount
   ( sysMount
   , sysUnmount
   )
where

import Data.Word (Word64)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr)

import ViperVM.Utils.EnumSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem.Mount (MountFlag,UnmountFlag)
import ViperVM.Arch.X86_64.Linux.Syscall

-- | Mount a file system
sysMount :: String -> String -> String -> [MountFlag] -> Ptr () -> SysRet ()
sysMount source target fstype flags dat = do
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            onSuccess (syscall5 165 source' target' fstype' (toBitSet flags :: Word64) dat) (const ())


-- | Unmount a file system
sysUnmount :: String -> [UnmountFlag] -> SysRet ()
sysUnmount target flags =
   withCString target $ \target' ->
      onSuccess (syscall2 166 target' (toBitSet flags :: Word64)) (const ())
