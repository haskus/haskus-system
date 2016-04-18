{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

-- | System
module ViperVM.System.System
   ( System(..)
   , defaultSystemInit
   , systemInit
   , getDeviceHandle
   , releaseDeviceHandle
   , openDeviceDir
   , listDevicesWithClass
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.Mount
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.KernelEvent
import ViperVM.System.Sys
import ViperVM.System.Event
import ViperVM.Utils.Flow

import System.FilePath

import Prelude hiding (init,tail)
import Control.Monad (void)
import Control.Concurrent.STM
import Data.Maybe (catMaybes)
import Data.ByteString (ByteString)

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)


data System = System
   { systemDevFS       :: Handle            -- ^ root of the tmpfs used to create device nodes
   , systemSysFS       :: Handle            -- ^ SysFS
   , systemProcFS      :: Handle            -- ^ procfs
   , systemNetlinkChan :: TChan KernelEvent -- ^ Netlink events
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

   -- create netlink reader
   netlink <- newKernelEventReader

   return (System devfd sysfd procfd netlink)

-- | Get a handle on a device
--
-- Linux doesn't provide an API to open a device directly from its major and
-- minor numbers. Instead we must create a special device file with mknod in
-- the VFS and open it. This is what this function does. Additionally, we
-- remove the file once it is opened.
getDeviceHandle :: System -> DeviceType -> Device -> Sys Handle
getDeviceHandle system typ dev = do

   let 
      devname = "./dummy"
      devfd   = systemDevFS system

   sysLogSequence "Open device" $ do
      sysCallAssert "Create device special file" $
         createDeviceFile devfd devname typ BitSet.empty dev
      fd  <- sysCallAssert "Open device special file" $
         sysOpenAt devfd devname (BitSet.fromList [HandleReadWrite,HandleNonBlocking]) BitSet.empty
      sysCallAssert "Remove device special file" $
         sysUnlinkAt devfd devname False
      return fd

-- | Release a device handle
releaseDeviceHandle :: Handle -> Sys ()
releaseDeviceHandle fd = do
   sysCallAssertQuiet "Close device" $ sysClose fd

-- | Find device path by number (major, minor)
openDeviceDir :: System -> DeviceType -> Device -> SysRet Handle
openDeviceDir system typ dev = sysOpenAt (systemDevFS system) path (BitSet.fromList [HandleDirectory]) BitSet.empty
   where
      path = "./dev/" ++ typ' ++ "/" ++ ids
      typ' = case typ of
         CharDevice  -> "char"
         BlockDevice -> "block"
      ids  = show (deviceMajor dev) ++ ":" ++ show (deviceMinor dev)


-- | List devices with the given class
--
-- TODO: support dynamic asynchronous device adding/removal
listDevicesWithClass :: System -> String -> Sys [(FilePath,Device)]
listDevicesWithClass system cls = do
   -- open class directory in SysFS
   let 
      clsdir = "class" </> cls

      -- parser for dev files
      -- content format is: MMM:mmm\n (where M is major and m is minor)
      parseDevFile :: Parsec ByteString Device
      parseDevFile = do
         major <- fromIntegral <$> decimal
         void (char ':')
         minor <- fromIntegral <$> decimal
         void eol
         return (Device major minor)

      -- read device major and minor in "dev" file
      readDevFile :: Handle -> Flow Sys '[Device,ErrorCode]
      readDevFile devfd = do
         -- 16 bytes should be enough
         sysCallWarn "Read dev file" (readByteString devfd 16)
         >.-.> \content -> case parseMaybe parseDevFile content of
            Nothing -> error "Invalid dev file format"
            Just x  -> x

      -- read device directory
      readDev :: Handle -> FilePath -> Flow Sys '[Maybe (FilePath,Device)]
      readDev fd dir =
         withOpenAt fd (dir </> "dev") BitSet.empty BitSet.empty readDevFile
            -- skip entries without "dev" file
            >.-.>  (Just . (clsdir </> dir,))
            >..~#> const (flowRet Nothing)

      -- read devices in a class
      readDevs :: Handle -> Flow Sys '[[(FilePath,Device)]]
      readDevs fd = do
         dirs <- sysCallAssert "List device directories" $ listDirectory fd
         let dirs'  = fmap entryName dirs
         flowTraverse (readDev fd) dirs' >.-.> catMaybes

   flowRes $ withOpenAt (systemSysFS system) clsdir BitSet.empty BitSet.empty readDevs
      -- in case of error, we don't return any dev
      >..~#> const (flowRet [])
