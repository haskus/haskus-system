module ViperVM.Arch.Linux.Modules
   ( loadModuleFromFile
   , loadModuleFromMemory
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls

import Data.Word
import Foreign.Ptr
import Foreign.C.String (withCString)

-- | Load a module from a file
loadModuleFromFile :: FileDescriptor -> String -> Int -> SysRet ()
loadModuleFromFile (FileDescriptor fd) params flags = do
   withCString params $ \params' ->
      onSuccess (syscall_finit_module fd  params' flags) (const ())

-- | Load a module from memory
loadModuleFromMemory :: Ptr () -> Word64 -> String -> SysRet ()
loadModuleFromMemory ptr sz params =
   withCString params $ \params' ->
      onSuccess (syscall_init_module ptr sz params') (const ())
