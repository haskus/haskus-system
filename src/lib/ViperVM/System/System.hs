-- | System
module ViperVM.System.System
   ( System(..)
   , defaultSystemInit
   , systemInit
   , openDevice
   , openDeviceDir
   , listDevicesWithClass
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.Mount
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.KernelEvent
import ViperVM.System.Devices

import System.FilePath

import Prelude hiding (init,tail)
import Control.Monad (void)
import Control.Concurrent.STM

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)


data System = System
   { systemDevFS       :: FileDescriptor     -- ^ root of the tmpfs used to create device nodes
   , systemSysFS       :: FileDescriptor     -- ^ SysFS
   , systemProcFS      :: FileDescriptor     -- ^ procfs
   , systemNetlinkChan :: TChan KernelEvent  -- ^ Netlink events
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
      r <- createDir path
      case r of
         Left EEXIST -> return (Right ())
         _           -> return r

   -- mount a tmpfs in root path
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount path

   -- mount sysfs
   sysCallAssert "Create sysfs directory" $ createDir sysfsPath
   sysCallAssert "Mount sysfs" $ mountSysFS sysMount sysfsPath
   sysfd <- sysCallAssert "Open sysfs directory" $ sysOpen sysfsPath [OpenReadOnly] BitSet.empty

   -- mount procfs
   sysCallAssert "Create procfs directory" $ createDir procfsPath
   sysCallAssert "Mount procfs" $ mountProcFS sysMount procfsPath
   procfd <- sysCallAssert "Open procfs directory" $ sysOpen procfsPath [OpenReadOnly] BitSet.empty

   -- create device directory
   sysCallAssert "Create device directory" $ createDir devicePath
   sysCallAssert "Mount tmpfs" $ mountTmpFS sysMount devicePath
   devfd <- sysCallAssert "Open device directory" $ sysOpen devicePath [OpenReadOnly] BitSet.empty

   -- create netlink reader
   netlink <- newKernelEventWaiterThread

   return (System devfd sysfd procfd netlink)

-- | Open a device
--
-- Linux doesn't provide an API to open a device directly from its major and
-- minor numbers. Instead we must create a special device file with mknod in
-- the VFS and open it. This is what this function does. Additionally, we
-- remove the file once it is opened.
openDevice :: System -> DeviceType -> Device -> Sys FileDescriptor
openDevice system typ dev = do

   let 
      devname = "./dummy"
      devfd   = systemDevFS system

   sysLogSequence "Open device" $ do
      sysCallAssert "Create device special file" $
         createDeviceFile devfd devname typ BitSet.empty dev
      fd  <- sysCallAssert "Open device special file" $
         sysOpenAt devfd devname [OpenReadWrite,OpenNonBlocking] BitSet.empty
      sysCallAssert "Remove device special file" $
         sysUnlinkAt devfd devname False
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


-- | List devices with the given class
--
-- TODO: support dynamic asynchronous device adding/removal
listDevicesWithClass :: System -> String -> (String -> Bool) -> Sys [(FilePath,Device)]
listDevicesWithClass system cls filtr = do
   -- open class directory in SysFS
   let 
      clsdir = "class" </> cls

      -- read device major and minor in "dev" file
      -- content format is: MMM:mmm\n (where M is major and m is minor)
      readDevFile devfd = do
         content <- sysCallAssert "Read dev file" $
                     readByteString devfd 16 -- 16 bytes should be enough
         let 
            parseDevFile = do
               major <- fromIntegral <$> decimal
               void (char ':')
               minor <- fromIntegral <$> decimal
               void eol
               return (Device major minor)
            dev = case parseMaybe parseDevFile content of
               Nothing -> error "Invalid dev file format"
               Just x  -> x
         return dev

      -- read device directory
      readDev fd dir = do
         dev <- withOpenAt fd (dir </> "dev") [OpenReadOnly] BitSet.empty readDevFile
         dev' <- sysCallAssert' ("Reading dev file: " ++ dir) dev
         return (clsdir </> dir, dev')

      -- read devices in a class
      readDevs :: FileDescriptor -> Sys [(FilePath,Device)]
      readDevs fd = do
         dirs <- sysCallAssert "List device directories" $ listDirectory fd
         let dirs'  = filter filtr (fmap entryName dirs)
         traverse (readDev fd) dirs'

   devs <- withOpenAt (systemSysFS system) clsdir [OpenReadOnly] BitSet.empty readDevs

   case devs of
      Left _  -> return []
      Right v -> return v
