module ViperVM.Arch.Linux.Modules
   ( loadModuleFromFile
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls

import Foreign.C.String (withCString)

loadModuleFromFile :: FileDescriptor -> String -> Int -> SysRet ()
loadModuleFromFile (FileDescriptor fd) params flags = do
   withCString params $ \params' ->
      onSuccess (syscall_finit_module fd  params' flags) (const ())
