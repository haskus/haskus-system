{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

-- | System
module ViperVM.System.System
   ( System(..)
   , defaultSystemInit
   , systemInit
   -- * Memory map
   , getProcessMemoryMap
   , memoryMapToBufferList
   , memoryMapToBuffer
   , MemoryMapEntry (..)
   , MappingType (..)
   , Perm (..)
   , Sharing (..)
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.Mount
import ViperVM.Arch.Linux.Process.MemoryMap

import ViperVM.System.Sys
import ViperVM.System.FileSystem
import ViperVM.System.Devices
import ViperVM.Utils.Flow

import System.FilePath

import Prelude hiding (init,tail)

data System = System
   { systemProcFS        :: Handle            -- ^ procfs
   , systemDeviceManager :: DeviceManager     -- ^ Device manager
   }

-- | Initialize the system
defaultSystemInit :: Sys System
defaultSystemInit = systemInit "/system"

-- | Create a system object
--
-- Create the given @path@ if it doesn't exist and mount the system in it
systemInit :: FilePath -> Sys System
systemInit path = sysLogSequence "Initialize the system" $ do

   let 
      createDir p = sysCreateDirectory Nothing p (BitSet.fromList [PermUserRead,PermUserWrite,PermUserExecute]) False
      sysfsPath  = path </> "sys"
      procfsPath = path </> "proc"
      devicePath = path </> "dev"

   -- create root path (allowed to fail if it already exists)
   sysCallAssert "Create root directory" $ do
      createDir path >%~#> \case
         EEXIST -> flowRet ()
         e      -> flowSet e

   -- mount a tmpfs in root path
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount path

   -- mount sysfs
   sysCallAssert "Create sysfs directory" $ createDir sysfsPath
   sysCallAssert "Mount sysfs" $ mountSysFS sysMount sysfsPath
   sysfd <- sysCallAssert "Open sysfs directory" $ sysOpen sysfsPath BitSet.empty BitSet.empty

   -- mount procfs
   sysCallAssert "Create procfs directory" $ createDir procfsPath
   sysCallAssert "Mount procfs" $ mountProcFS sysMount procfsPath
   procfd <- sysCallAssert "Open procfs directory" $ sysOpen procfsPath BitSet.empty BitSet.empty

   -- create device directory
   sysCallAssert "Create device directory" $ createDir devicePath
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount devicePath
   devfd <- sysCallAssert "Open device directory" $ sysOpen devicePath BitSet.empty BitSet.empty

   -- init device manager
   dm <- initDeviceManager sysfd devfd

   return $ System
      { systemProcFS        = procfd
      , systemDeviceManager = dm
      }

-- | Get process memory mappings
getProcessMemoryMap :: System -> SysV '[[MemoryMapEntry],ErrorCode]
getProcessMemoryMap sys =
   atomicReadBuffer (systemProcFS sys) "self/maps"
   >.-.> parseMemoryMap
