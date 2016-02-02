{-# LANGUAGE OverloadedStrings #-}

-- | System
module ViperVM.Arch.Linux.System.System
   ( System(..)
   , systemInit
   , openDevice
   , openDeviceDir
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Terminal
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.Mount

import System.FilePath

data System = System
   { systemDevFS  :: FileDescriptor    -- ^ root of the tmpfs used to create device nodes
   , systemSysFS  :: FileDescriptor    -- ^ systemfs (SysFS)
   }


-- | Create a system object
--
-- Create the given @path@ if it doesn't exist and mount the system in it
systemInit :: FilePath -> Sys System
systemInit path = sysLogSequence "Initialize the system" $ do

   let 
      createDir p = sysCreateDirectory Nothing p (BitSet.fromList [PermUserRead,PermUserWrite,PermUserExecute]) False
      systemPath = path </> "sys"
      devicePath = path </> "dev"

   -- create root path (allowed to fail if it already exists)
   sysCallAssert "Create root directory" $ do
      r <- createDir path
      case r of
         Left EEXIST -> return (Right ())
         _           -> return r

   -- mount a tmpfs in root path
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount path

   -- mount sysfs
   sysCallAssert "Create system directory" $ createDir systemPath
   sysCallAssert "Mount sysfs" $ mountSysFS sysMount systemPath
   sysfd <- sysCallAssert "Open sysfs directory" $ sysOpen systemPath [OpenReadOnly] BitSet.empty

   -- create device directory
   sysCallAssert "Create device directory" $ createDir devicePath
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount devicePath
   devfd <- sysCallAssert "Open device directory" $ sysOpen devicePath [OpenReadOnly] BitSet.empty

   return (System devfd sysfd)

-- | Open a device
--
-- Linux doesn't provide an API to open a device directly from its major and
-- minor numbers. Instead we must create a special device file with mknod in
-- the VFS and open it. This is what this function does. Additionally, we
-- remove the file once it is opened.
openDevice :: System -> DeviceType -> Device -> SysRet FileDescriptor
openDevice system typ dev = do

   let 
      devname = "./dummy"
      devfd   = systemDevFS system

   runCatch $ do
      sysTry "Create device special file" $ createDeviceFile devfd devname typ BitSet.empty dev
      fd  <- sysTry "Open device special file" $ sysOpenAt devfd devname [OpenReadWrite] BitSet.empty
      sysTry "Remove device special file" $ sysUnlinkAt devfd devname False
      return fd

-- | Find device path by number (major, minor)
openDeviceDir :: System -> DeviceType -> Device -> SysRet FileDescriptor
openDeviceDir system typ dev = sysOpenAt (systemDevFS system) path [OpenReadOnly,OpenDirectory] BitSet.empty
   where
      path = "./dev/" ++ typ' ++ "/" ++ ids
      typ' = case typ of
         CharDevice  -> "char"
         BlockDevice -> "block"
      ids  = show (deviceMajor dev) ++ ":" ++ show (deviceMinor dev)
