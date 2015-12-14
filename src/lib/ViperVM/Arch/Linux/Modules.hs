module ViperVM.Arch.Linux.Modules
   ( loadModuleFromFile
   , loadModuleFromMemory
   , LoadModuleFlag(..)
   , LoadModuleFlags
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.BitSet
import qualified ViperVM.Format.Binary.BitSet as BitSet

import Data.Word
import Foreign.Ptr
import Foreign.C.String (withCString)

data LoadModuleFlag
   = IgnoreSymbolVersions
   | IgnoreKernelVersion
   deriving (Show,Eq,Enum)

instance EnumBitSet LoadModuleFlag

type LoadModuleFlags = BitSet Word LoadModuleFlag

-- | Load a module from a file
loadModuleFromFile :: FileDescriptor -> String -> LoadModuleFlags -> SysRet ()
loadModuleFromFile (FileDescriptor fd) params flags = do
   withCString params $ \params' ->
      onSuccess (syscall_finit_module fd  params' (BitSet.toBits flags)) (const ())

-- | Load a module from memory
loadModuleFromMemory :: Ptr () -> Word64 -> String -> SysRet ()
loadModuleFromMemory ptr sz params =
   withCString params $ \params' ->
      onSuccess (syscall_init_module ptr sz params') (const ())
