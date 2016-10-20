{-# LANGUAGE DeriveAnyClass #-}

-- | Kernel module management
module ViperVM.Arch.Linux.Modules
   ( loadModuleFromFile
   , loadModuleFromMemory
   , LoadModuleFlag(..)
   , LoadModuleFlags
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.String (withCString)


-- | Load module flag
data LoadModuleFlag
   = IgnoreSymbolVersions
   | IgnoreKernelVersion
   deriving (Show,Eq,Enum,CBitSet)

-- | Load module flags
type LoadModuleFlags = BitSet Word LoadModuleFlag

-- | Load a module from a file
loadModuleFromFile :: Handle -> String -> LoadModuleFlags -> IOErr ()
loadModuleFromFile (Handle fd) params flags = do
   withCString params $ \params' ->
      onSuccess (syscall_finit_module fd  params' (BitSet.toBits flags)) (const ())

-- | Load a module from memory
loadModuleFromMemory :: Ptr () -> Word64 -> String -> IOErr ()
loadModuleFromMemory ptr sz params =
   withCString params $ \params' ->
      onSuccess (syscall_init_module ptr sz params') (const ())
