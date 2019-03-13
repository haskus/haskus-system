{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}


-- | Kernel module management
module Haskus.System.Linux.Modules
   ( loadModuleFromFile
   , loadModuleFromMemory
   , LoadModuleFlag(..)
   , LoadModuleFlags
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Syscalls
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Foreign.Ptr
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
loadModuleFromFile :: MonadInIO m => Handle -> String -> LoadModuleFlags -> Excepts '[ErrorCode] m ()
loadModuleFromFile (Handle fd) params flags = do
   withCString params $ \params' ->
      checkErrorCode_ =<< liftIO (syscall_finit_module fd  params' (BitSet.toBits flags))

-- | Load a module from memory
loadModuleFromMemory :: MonadInIO m => Ptr () -> Word64 -> String -> Excepts '[ErrorCode] m ()
loadModuleFromMemory ptr sz params =
   withCString params $ \params' ->
      checkErrorCode_ =<< liftIO (syscall_init_module ptr sz params')
