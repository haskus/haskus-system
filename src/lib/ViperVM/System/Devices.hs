{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | Devices management
module ViperVM.System.Devices
   ( DeviceManager (..)
   , KernelObject (..)
   , initDeviceManager
   , getDeviceHandle
   , releaseDeviceHandle
   , openDeviceDir
   , listDevicesWithClass
   , listDeviceClasses
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Word
import ViperVM.Format.Text (Text, bufferDecodeUtf8)
import qualified ViperVM.Format.Text as Text
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.KernelEvent
import ViperVM.System.Sys
import ViperVM.System.Event
import ViperVM.System.ReadWrite
import ViperVM.System.Process
import ViperVM.Utils.Flow
import ViperVM.Utils.STM.TMap as TMap

import System.FilePath

import Prelude hiding (init,tail)
import Control.Monad (void,forever)
import Data.Foldable (forM_)
import Control.Concurrent.STM
import Data.Maybe (catMaybes)

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)

-- Note [SysFS]
-- ~~~~~~~~~~~~
--
-- Linux uses "sysfs" to export kernel objects, their attributes and their
-- relationships to user-space. The mapping is as follow:
--
--       |    Kernel     | User-space     |
--       |--------------------------------|
--       | Objects       | Directories    |
--       | Attributes    | Files          |
--       | Relationships | Symbolic links |
--
-- Initially attributes were ASCII files at most page-size large. Now there are
-- "binary attributes" (non-ASCII files) that can be larger than a page.
--
-- The SysFS tree is mutable: devices can be (un)plugged, renamed, etc.
--    * Object changes in SysFS are notified to userspace via Netlink's kernel events.
--    * Attribute changes are not. But we should be able to watch some of them with
--    inotify or epoll.
--
-- User-space can set some attributes by writing into the attribute files.
--
-- REFERENCES
--    * "The sysfs Filesystem", Patrick Mochel, 2005
--       https://www.kernel.org/pub/linux/kernel/people/mochel/doc/papers/ols-2005/mochel.pdf

-- Note [Kernel Object and Subsystems]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Kernel object (or kobject) is a kernel structure used as a top-class for
-- several kinds of objects. It provides/supports:
--    * reference counting
--    * an object name
--    * a hierarchy of kobject's
--       * via a "parent" field (pointer to another kobject)
--       * via "ksets" (subsystems)
--    * sysfs mapping and notifications
--
-- A subsystem (or a "kset") is basically a kobject which references a
-- linked-list of kobjects of the same type. Each kobject can only be in a
-- single subsystem (via its "kset" field).
--
--
-- Note [Devices]
-- ~~~~~~~~~~~~~~
--
-- According to Documation/sysfs-rules.txt in the kernel tree:
--    * there is a single tree containing all the devices: in /devices
--    * devices have the following properties:
--       * a devpath (e.g., /devices/pci0000:00/0000:00:1d.1/usb2/2-2/2-2:1.0)
--       used as a unique key to identify the device at this point in time
--       * a kernel name (basename of the devpath)
--       * a subsystem (optional): basename of the "subsystem" link
--       * a driver (optional): basename of the "driver" link
--       * attributes (files)
--
-- Devices are defined with a "struct device" (cf include/linux/device.h in the
-- kernel tree). 
--
-- Devices can be found by their subsystem: until it gets unified in a
-- /subsystem directory, we can find devices by subsystems by looking into
-- /class/SUB and /bus/SUB/devices.
--
-- "device" link shouldn't be used at all. The device hierarchy in /devices can
-- be used instead.
--
-- "subsystem" link shouldn't be used at all (except for getting the subsystem
-- name I guess).
--
-- We musn't assume a specific device hierarchy as it can change between kernel
-- versions.


-- | Kernel object
data KernelObject = KernelObject
   { kobjPath       :: TVar Text                 -- ^ Path in sysfs
   , kobjParent     :: TVar (Maybe KernelObject) -- ^ Parent object
   , kobjChildren   :: TMap Text KernelObject    -- ^ Children objects (subdirs in sysfs)
   }

-- | Device manager
data DeviceManager = DeviceManager
   { dmDevices :: KernelObject      -- ^ Device hierarchy
   , dmEvents  :: TChan KernelEvent -- ^ Netlink kobject events
   , dmSysFS   :: Handle            -- ^ Handle to sysfs
   , dmDevFS   :: Handle            -- ^ root of the tmpfs used to create device nodes
   , dmDevNum  :: TVar Word64       -- ^ counter used to create device node
   }

-- | Init a device manager
initDeviceManager :: Handle -> Handle -> Sys DeviceManager
initDeviceManager sysfs devfs = do
   
   -- open Netlink socket
   bch <- newKernelEventReader

   -- we block the event handler thread until we have totally read sysfs
   -- entries. Then we can proceed with events that have occured during this
   -- initialization phase.
   blocked <- sysIO (newTVarIO True)

   -- duplicate the channel
   ch <- sysIO $ atomically $ dupTChan bch

   sysFork $ do
      -- wait until we are unblocked
      sysIO $ atomically $ do
         b <- readTVar blocked
         if b then retry else return ()

      forever $ do
         -- read kernel event
         ev <- sysIO $ atomically (readTChan ch)

         -- update the device tree
         -- TODO

         -- trigger rules
         -- TODO
         return ()
         
   -- enumerate devices from sysfs
   let
      listDevs :: KernelObject -> Handle -> Sys ()
      listDevs parent root = do
         dirs <- flowRes $
                     sysIO (listDirectory root)
                        -- empty directoy list on error
                        >..-.> const []

         let
            flags = BitSet.fromList [ HandleDirectory
                                    , HandleNonBlocking
                                    ]

         -- recursively build the tree (depth-first traversal, parent nodes stay
         -- opened)
         forM_ dirs $ \entry -> do
            let 
               dir  = entryName entry
               dir' = Text.pack dir
            path <- do
               p' <- sysIO (readTVarIO (kobjPath parent))
               return (Text.concat [p',dir'])

            void $ withOpenAt root dir flags BitSet.empty $ \fd -> do
               ko <- KernelObject
                        <$> sysIO (newTVarIO path)
                        <*> sysIO (newTVarIO (Just parent))
                        <*> sysIO (atomically TMap.empty)

               -- insert into parent
               sysIO $ atomically $ TMap.insert dir' ko (kobjChildren parent)

               -- lookup children
               listDevs ko fd

               flowRet' ()

   rootko <- KernelObject
               <$> sysIO (newTVarIO (Text.pack "/devices"))
               <*> sysIO (newTVarIO Nothing)
               <*> sysIO (atomically TMap.empty)
   listDevs rootko sysfs

   -- unblock the event thread
   sysIO $ atomically $ writeTVar blocked False
   
   -- device node counter
   devNum <- sysIO (newTVarIO 0)

   return $ DeviceManager
      { dmDevices = rootko
      , dmEvents  = bch
      , dmSysFS   = sysfs
      , dmDevFS   = devfs
      , dmDevNum  = devNum
      }

-- | Get a handle on a device
--
-- Linux doesn't provide an API to open a device directly from its major and
-- minor numbers. Instead we must create a special device file with mknod in
-- the VFS and open it. This is what this function does. Additionally, we
-- remove the file once it is opened.
getDeviceHandle :: DeviceManager -> DeviceType -> Device -> Sys Handle
getDeviceHandle dm typ dev = do

   -- get a fresh device number
   num <- sysIO $ atomically $ do
            n <- readTVar (dmDevNum dm)
            writeTVar (dmDevNum dm) (n+1)
            return n

   let 
      devname = "./dummy" ++ show num
      devfd   = dmDevFS dm

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
openDeviceDir :: DeviceManager -> DeviceType -> Device -> SysRet Handle
openDeviceDir dm typ dev = sysOpenAt (dmDevFS dm) path (BitSet.fromList [HandleDirectory]) BitSet.empty
   where
      path = "./dev/" ++ typ' ++ "/" ++ ids
      typ' = case typ of
         CharDevice  -> "char"
         BlockDevice -> "block"
      ids  = show (deviceMajor dev) ++ ":" ++ show (deviceMinor dev)


-- | List devices classes
listDeviceClasses :: DeviceManager -> Flow Sys '[[String],ErrorCode]
listDeviceClasses dm = do
   withOpenAt (dmSysFS dm) "class" BitSet.empty BitSet.empty $ \fd -> do
      sysIO (listDirectory fd)
         >.-.> fmap entryName

-- | List devices with the given class
--
-- TODO: support dynamic asynchronous device adding/removal
listDevicesWithClass :: DeviceManager -> String -> Sys [(FilePath,Device)]
listDevicesWithClass dm cls = do
   -- open class directory in SysFS
   let 
      clsdir = "class" </> cls

      -- parser for dev files
      -- content format is: MMM:mmm\n (where M is major and m is minor)
      parseDevFile :: Parsec Text Device
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
         sysCallWarn "Read dev file" (handleReadBuffer devfd Nothing 16)
         >.-.> \content -> case parseMaybe parseDevFile (bufferDecodeUtf8 content) of
            Nothing -> error "Invalid dev file format"
            Just x  -> x

      -- read device directory
      readDev :: Handle -> FilePath -> SysV '[Maybe (FilePath,Device)]
      readDev fd dir =
         withOpenAt fd (dir </> "dev") BitSet.empty BitSet.empty readDevFile
            -- skip entries without "dev" file
            >.-.>  (Just . (clsdir </> dir,))
            >..~#> const (flowRet Nothing)

      -- read devices in a class
      readDevs :: Handle -> SysV '[[(FilePath,Device)]]
      readDevs fd = do
         dirs <- sysCallAssert "List device directories" $ listDirectory fd
         let dirs'  = fmap entryName dirs
         flowTraverse (readDev fd) dirs' >.-.> catMaybes

   flowRes $ withOpenAt (dmSysFS dm) clsdir BitSet.empty BitSet.empty readDevs
      -- in case of error, we don't return any dev
      >..~#> const (flowRet [])

