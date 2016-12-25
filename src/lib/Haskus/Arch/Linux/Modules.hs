{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}


-- | Kernel module management
module Haskus.Arch.Linux.Modules
   ( loadModuleFromFile
   , loadModuleFromMemory
   , LoadModuleFlag(..)
   , LoadModuleFlags
   )
where

import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Syscalls
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.String (withCString)
import Haskus.Utils.Flow


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
