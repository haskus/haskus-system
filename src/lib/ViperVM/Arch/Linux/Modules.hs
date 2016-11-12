{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}


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
import ViperVM.Utils.Flow


-- | Load module flag
data LoadModuleFlag
   = IgnoreSymbolVersions
   | IgnoreKernelVersion
   deriving (Show,Eq,Enum,CBitSet)

-- | Load module flags
type LoadModuleFlags = BitSet Word LoadModuleFlag

-- | Load a module from a file
loadModuleFromFile :: MonadInIO m => Handle -> String -> LoadModuleFlags -> Flow m '[(),ErrorCode]
loadModuleFromFile (Handle fd) params flags = do
   withCString params $ \params' ->
      liftIO (syscall @"finit_module" fd  params' (BitSet.toBits flags))
         ||> toErrorCodeVoid

-- | Load a module from memory
loadModuleFromMemory :: MonadInIO m => Ptr () -> Word64 -> String -> Flow m '[(),ErrorCode]
loadModuleFromMemory ptr sz params =
   withCString params $ \params' ->
      liftIO (syscall @"init_module" ptr sz params')
         ||> toErrorCodeVoid
