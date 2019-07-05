{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | System
module Haskus.System.System
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
   , writeSysTextAttribute
   , writeProcTextAttribute
   )
where

import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem
import Haskus.System.Linux.FileSystem.Directory
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.System.Linux.FileSystem.Mount
import Haskus.System.Linux.Process.MemoryMap

import Haskus.System.Sys
import Haskus.System.FileSystem
import Haskus.System.Devices
import Haskus.Utils.Flow
import Haskus.Utils.Types.List
import qualified Data.Text as Text

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
   assertLogShowErrorE "Create root directory" <| do
      createDir path
         |> catchLiftLeft \case
               EEXIST -> return ()
               e      -> failureE e

   -- mount a tmpfs in root path
   assertLogShowErrorE "Mount tmpfs" <| mountTmpFS sysMount path

   -- mount sysfs
   assertLogShowErrorE "Create sysfs directory" <| createDir sysfsPath
   assertLogShowErrorE "Mount sysfs" <| mountSysFS sysMount sysfsPath
   sysfd <- open Nothing sysfsPath BitSet.empty BitSet.empty
            |> assertLogShowErrorE "open sysfs directory"

   -- mount procfs
   assertLogShowErrorE "Create procfs directory" <| createDir procfsPath
   assertLogShowErrorE "Mount procfs" <| mountProcFS sysMount procfsPath
   procfd <- open Nothing procfsPath BitSet.empty BitSet.empty
             |> assertLogShowErrorE "open procfs directory"

   -- create device directory
   assertLogShowErrorE "Create device directory" <| createDir devicePath
   assertLogShowErrorE "Mount tmpfs" <| mountTmpFS sysMount devicePath
   devfd <- open Nothing devicePath BitSet.empty BitSet.empty
            |> assertLogShowErrorE "open device directory"

   -- init device manager
   dm <- initDeviceManager sysfd devfd

   return $ System
      { systemProcFS        = procfd
      , systemDeviceManager = dm
      }

-- | Get process memory mappings
getProcessMemoryMap :: System -> Excepts (Union ReadErrors' OpenErrors) Sys [MemoryMapEntry]
getProcessMemoryMap sys =
   handleAtomicReadBuffer (systemProcFS sys) "self/maps"
      ||> parseMemoryMap

-- | Write a text attribute in sysfs
writeSysTextAttribute :: (MonadSys m,MonadInIO m) => System -> FilePath -> String -> m ()
writeSysTextAttribute sys path value = do
   withOpenAt (dmSysFS (systemDeviceManager sys)) path (BitSet.fromList [HandleWriteOnly]) (BitSet.fromList [PermUserWrite])
      (\hdl -> handleWriteStrLn hdl value)
   |> assertLogShowErrorE ("Write sysfs text attribute: " <> Text.pack path)

-- | Write a text attribute in procfs
writeProcTextAttribute :: (MonadSys m,MonadInIO m) => System -> FilePath -> String -> m ()
writeProcTextAttribute sys path value = do
   withOpenAt (systemProcFS sys) path (BitSet.fromList [HandleWriteOnly]) (BitSet.fromList [PermUserWrite])
      (\hdl -> handleWriteStrLn hdl value)
   |> assertLogShowErrorE ("Write procfs text attribute: " <> Text.pack path)
