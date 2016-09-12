{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | Devices management
module ViperVM.System.Devices
   ( DeviceManager (..)
   , Dev (..)
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
import ViperVM.System.ReadWrite
import ViperVM.System.Process
import ViperVM.Utils.Flow
import qualified ViperVM.Utils.STM.TTree as TTree
import ViperVM.Utils.STM.TTree (TTree,TTreePath(..))
import ViperVM.Utils.Maybe

import System.FilePath
import System.Posix.Types (Fd(..))
import Control.Monad (void,forever,when)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Control.Concurrent
import Control.Concurrent.STM

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)

-- Note [sysfs]
-- ~~~~~~~~~~~~
--
-- Linux uses "sysfs" virtual file system to export kernel objects, their
-- attributes and their relationships to user-space. The mapping is as follow:
--
--       |    Kernel     | User-space     |
--       |--------------------------------|
--       | Objects       | Directories    |
--       | Attributes    | Files          |
--       | Relationships | Symbolic links |
--
-- Initially attributes were ASCII files at most one page-size large. Now there
-- are "binary attributes" (non-ASCII files) that can be larger than a page.
--
-- The sysfs tree is mutable: devices can be (un)plugged, renamed, etc.
--    * Object changes in sysfs are notified to userspace via Netlink's kernel events.
--    * Attribute changes are not. But we should be able to watch some of them with
--    inotify
--
-- User-space can set some attributes by writing into the attribute files.
--
-- sysfs documentation is very sparse and overly bad. I had to read sources
-- (udev, systemd's sd-device, linux), articles, kernel docs, MLs, etc. See here
-- for a tentative to document the whole thing by Rob Landley in 2007 and the
-- bad reactions from sysfs/udev devs:
--    "Documentation for sysfs, hotplug, and firmware loading." thread on LKML
--    http://lkml.iu.edu/hypermail/linux/kernel/0707.2/index.html#1085
--
-- Most of his critics are still valid:
--    * current documentation is bad (says what not to do, but not what to do)
--    * there is no unified /sys/subsystem directory
--    * we still have to check the subsystem to see if devices are block or char
--    * contradictions between anti-guidelines in sysfs-rules.txt and available
--    approaches
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
--
--
-- Kernel Object and Subsystems
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-- HotPlug and ColdPlug
-- ~~~~~~~~~~~~~~~~~~~~
--
-- HotPlug devices are signaled through a Netlink socket.
--
-- ColdPlug devices are already in the sysfs tree before we have a chance to
-- listen to the Netlink socket. We may:
--    1) write "add" in their "uevent" attribute to get them resent through the
--    Netlink socket with Add action (remove, change, move, etc. commands seem
--    to work too with the uevent attribute).
--    2) just parse their "uevent" attribute
--
--
-- SUMMARY
-- ~~~~~~~
--
-- The kernel wants to export a mutable tree to user-space:
--    * non-leaf nodes can be added, removed, moved (renamed)
--    * leaf nodes can be added, removed or have their value changed
--    * some leaf nodes can be written by user-space
--
-- sysfs offers a *non-atomic* interface on the current state of the tree because
-- of the nature of the VFS:
--    * nodes can be added/removed/moved between directory listing and actual
--    exploration of the listing
--    * an opened file may not be readable/writable anymore
--
-- netlink socket signals some of the changes:
--    * non-leaf node addition/removal/renaming
--    * generic "change" action for attributes
--
-- Specific attributes can be watched with inotify, especially if they don't
-- trigger "change" netlink notification when their value changes.
--
-- REFERENCES
--    * "The sysfs Filesystem", Patrick Mochel, 2005
--       https://www.kernel.org/pub/linux/kernel/people/mochel/doc/papers/ols-2005/mochel.pdf
--    * Documentation/sysfs-rules in the kernel tree (what not to do)
--    * lib/kobject.c in the kernel tree (e.g., function kobject_rename)
--

-- | Device
data Dev = Dev
   { deviceKernelName :: TVar Text -- ^ Device directory name in sysfs (i.e., kernel object name)
   }

-- | Device tree (key is deviceKernelName)
type DeviceTree = TTree (TVar Text) Dev

-- | Device manager
data DeviceManager = DeviceManager
   { dmEvents  :: TChan KernelEvent -- ^ Netlink kobject events
   , dmSysFS   :: Handle            -- ^ Handle to sysfs
   , dmDevFS   :: Handle            -- ^ root of the tmpfs used to create device nodes
   , dmDevNum  :: TVar Word64       -- ^ counter used to create device node
   , dmDevices :: DeviceTree        -- ^ Device hierarchy
   }

-- | Init a device manager
initDeviceManager :: Handle -> Handle -> Sys DeviceManager
initDeviceManager sysfs devfs = do
   
   -- open Netlink socket
   bch <- newKernelEventReader

   -- duplicate the kernel event channel: events accumulate until we launch the
   -- handling thread
   ch <- sysIO $ atomically $ dupTChan bch

   -- enumerate devices from sysfs
   let
      createDeviceTree :: Text -> STM DeviceTree
      createDeviceTree name = do
         d <- Dev <$> newTVar name
         TTree.singleton (deviceKernelName d) d

      flags = BitSet.fromList [ HandleDirectory
                              , HandleNonBlocking
                              , HandleDontFollowSymLinks
                              ]

      listDevs :: DeviceTree -> Handle -> Sys ()
      listDevs parent root = do
         dirs <- flowRes $
                     sysIO (listDirectory root)
                        -- empty directoy list on error
                        >..-.> const []

         -- recursively build the tree (depth-first traversal, parent nodes stay
         -- opened)
         forM_ dirs $ \entry -> do
            -- check that the entry is a directory
            when (entryType entry == TypeDirectory) $ do
               let 
                  dir  = entryName entry
                  dir' = Text.pack dir

               void $ withOpenAt root dir flags BitSet.empty $ \fd -> do
                  tree <- sysIO $ atomically $ do
                     -- create device node
                     t <- createDeviceTree dir'
                     -- insert into parent
                     parent `TTree.attachChild` t 
                     return t

                  -- lookup children
                  listDevs tree fd

                  flowRet' ()

   -- create root node
   root <- sysIO $ atomically $ createDeviceTree (Text.pack "devices")

   -- list devices in /devices
   void $ withOpenAt sysfs "devices" flags BitSet.empty $ \fd -> do
      listDevs root fd
      flowRet' ()

   -- device node counter
   devNum <- sysIO (newTVarIO 0)

   let dm = DeviceManager
               { dmDevices = root
               , dmEvents  = bch
               , dmSysFS   = sysfs
               , dmDevFS   = devfs
               , dmDevNum  = devNum
               }

   -- launch handling thread
   sysFork $ eventThread ch dm

   return dm

-- | Thread handling incoming kernel events
eventThread :: TChan KernelEvent -> DeviceManager -> Sys ()
eventThread ch dm = do
   forever $ do
      -- read kernel event
      ev <- sysIO $ atomically (readTChan ch)

      let makeTreePath s = fmap TTreePath
                           $ traverse (sysIO . newTVarIO)
                           $ tail  -- drop "" (blank before first "/")
                           $ Text.split (== '/') s

      devTreePath <- makeTreePath (kernelEventDevPath ev)

      -- update the device tree and trigger rules
      case kernelEventAction ev of
         ActionAdd        -> return () -- TODO
         ActionRemove     -> return () -- TODO
         ActionChange     -> return () -- TODO
         ActionOnline     -> return () -- TODO
         ActionOffline    -> return () -- TODO
         ActionOther _    -> return () -- TODO

         -- A device can be moved/renamed in the device tree (see kobject_rename
         -- in lib/kobject.c in the kernel sources)
         ActionMove       -> do
            -- get old device path
            let old = Map.lookup (Text.pack "DEVPATH_OLD") (kernelEventDetails ev)
            oldPath <- traverse makeTreePath old
                           `onNothingM` sysError "Cannot find DEVPATH_OLD entry for device move kernel event"

            err <- sysIO $ atomically $ do
               let 
                  TTreePath dtp = devTreePath
                  newParentPath = TTreePath (init dtp)
                  newName       = last dtp

               TTree.treeFollowPath (dmDevices dm) oldPath >>= \case
                  Nothing -> return (sysError ("Cannot find device to move: " ++ show old))
                  Just n  -> TTree.treeFollowPath (dmDevices dm) newParentPath >>= \case
                     Nothing -> return (sysError "Device moved to a on-existing parent device")
                     Just newParent -> do
                        -- move node in the tree
                        n `TTree.attachChild` newParent

                        -- rename node
                        readTVar newName >>= writeTVar (deviceKernelName (TTree.treeValue n))

                        -- signal move
                        -- TODO

                        -- return "no error"
                        return (return ())

            -- signal errors
            err


-- | Create a new thread reading kernel events and putting them in a TChan
newKernelEventReader :: Sys (TChan KernelEvent)
newKernelEventReader = do
   fd <- createKernelEventSocket
   ch <- sysIO newBroadcastTChanIO
   let
      Handle lowfd = fd
      rfd = Fd (fromIntegral lowfd)
      go  = sysIO $ forever $ do
               threadWaitRead rfd
               ev <- runSys $ receiveKernelEvent fd
               atomically $ writeTChan ch ev

   sysFork go
   return ch


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
      readDev fd dir = do
         -- read symlink pointing into /devices (we shall not directly use/return paths in /class)
         sysCallWarn "read link" (sysReadLinkAt fd dir)
            >.~#> (\path -> 
               -- the link is relative to the current dir (i.e., /class/CLASS)
               withOpenAt fd (path </> "dev") BitSet.empty BitSet.empty readDevFile
                  -- return an absolute path of the form "/devices/*"
                  >.-.>  (Just . (concat ("/" : drop 2 (splitPath path)),)))
            -- on error (e.g., if there is no "dev" file), skip the device directory
            >..~#> const (flowRet Nothing)

   -- open /class/CLASS directory
   flowRes $ withOpenAt (dmSysFS dm) clsdir BitSet.empty BitSet.empty
      (\clsHdl -> do
         -- list devices in a class
         dirs <- sysCallAssert "List devices in a class directory" $ listDirectory clsHdl
         let dirs'  = fmap entryName dirs
         flowTraverse (readDev clsHdl) dirs' >.-.> catMaybes
      )
      -- in case of error, we don't return any dev
      >..~#> const (flowRet [])

