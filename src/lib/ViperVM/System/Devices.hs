{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | Devices management
module ViperVM.System.Devices
   ( DeviceManager (..)
   , DeviceTree (..)
   , SubsystemIndex (..)
   , initDeviceManager
   , getDeviceHandle
   , releaseDeviceHandle
   , openDeviceDir
   , listDevicesWithClass
   , listDeviceClasses
   )
where

import Prelude hiding (lookup)

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
import ViperVM.Arch.Linux.FileSystem.SymLink
import ViperVM.Arch.Linux.KernelEvent
import ViperVM.System.Sys
import ViperVM.System.FileSystem
import ViperVM.System.Process
import ViperVM.Utils.Flow
import ViperVM.Utils.Maybe

import System.FilePath
import System.Posix.Types (Fd(..))
import Control.Monad (void,forever)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
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
-- According to Documentation/sysfs-rules.txt in the kernel tree:
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
-- If the subsystem is "block", device special files have to be of type "block",
-- otherwise they have to be of type "character".
--
-- "device" link shouldn't be used at all to find the parent device. The device
-- hierarchy in /devices can be used instead.
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
--    2) read their "uevent" attribute and fake an "Add" event
--    3) just parse their attributes if necessary
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

-- | Device tree
--
-- It is expected that the device tree will not change much after the
-- initialization phase (except when a device is (dis)connected, etc.), hence it
-- is an immutable data structure. It is much easier to perform tree traversal
-- with a single global lock thereafter.
data DeviceTree = DeviceNode
   { deviceNodeSubsystem     :: Maybe Text          -- ^ Subsystem
   , deviceNodeChildren      :: Map Text DeviceTree -- ^ Children devices
   , deviceNodeOnRemove      :: TChan KernelEvent   -- ^ On "remove" event
   , deviceNodeOnChange      :: TChan KernelEvent   -- ^ On "change" event
   , deviceNodeOnMove        :: TChan KernelEvent   -- ^ On "move" event
   , deviceNodeOnOnline      :: TChan KernelEvent   -- ^ On "online" event
   , deviceNodeOnOffline     :: TChan KernelEvent   -- ^ On "offline" event
   , deviceNodeOnOther       :: TChan KernelEvent   -- ^ On other events
   }

-- | Device manager
data DeviceManager = DeviceManager
   { dmEvents     :: TChan KernelEvent              -- ^ Netlink kobject events
   , dmSysFS      :: Handle                         -- ^ Handle to sysfs
   , dmDevFS      :: Handle                         -- ^ root of the tmpfs used to create device nodes
   , dmDevNum     :: TVar Word64                    -- ^ counter used to create device node
   , dmDevices    :: TVar DeviceTree                -- ^ Device hierarchy
   , dmSubsystems :: TVar (Map Text SubsystemIndex) -- ^ Per-subsystem index
   }

-- | Per-subsystem events
data SubsystemIndex = SubsystemIndex
   { subsystemDevices  :: Set Text   -- ^ Devices in the index
   , subsystemOnAdd    :: TChan Text -- ^ Signal device addition
   , subsystemOnRemove :: TChan Text -- ^ Signal device removal
   }

-- | Init a device manager
initDeviceManager :: Handle -> Handle -> Sys DeviceManager
initDeviceManager sysfs devfs = do
   
   -- open Netlink socket and then duplicate the kernel event channel so that
   -- events start accumulating until we launch the handling thread
   bch <- newKernelEventReader
   ch <- sysIO $ atomically $ dupTChan bch

   -- we enumerate devices from sysfs. Directory listing is non-atomic so
   -- directories may appear or be removed while we do the traversal. Hence we
   -- shouldn't fail on error, just skip the erroneous directories.
   --
   -- After the traversal, kernel events potentially received during the
   -- traversal are used to create/remove nodes. We have to be liberal in their
   -- interpretation: e.g., a remove event could be received for a directory we
   -- haven't be able to read, etc.
   let

      -- read a sysfs device directory and try to create a DeviceTree
      -- recursively from it. The current directory is already opened and the
      -- handle is passed (alongside the name and fullname).
      -- Return Nothing if it fails for any reason.
      readSysfsDir :: Text -> Text -> Handle -> SysV '[(Text,DeviceTree)]
      readSysfsDir name fullname hdl = do
         
         -- list directories (sub-devices) that are *not* symlinks
         dirs <- sysIO (listDirectory hdl)
                 -- filter to keep only directories (sysfs fills the type field)
                 >.-.> filter (\entry -> entryType entry == TypeDirectory)
                 -- only keep the directory name
                 >.-.> fmap entryName
                 -- return an empty directory list on error
                 >..-.> const []
                 -- extract the result
                 |> flowRes

         -- recursively try to create a tree for each sub-dir
         children <- flowRes <| 
                     catMaybes <.-.<
                     flowFor dirs $ \dir -> do
                        let
                           name'     = Text.pack dir
                           fullname' = Text.concat [fullname, Text.pack "/", name']

                        withOpenAt hdl dir flags BitSet.empty (readSysfsDir name' fullname')
                           -- wrap into Just on success
                           >.-.> Just
                           -- return Nothing on error
                           >..-.> const Nothing

         -- read the subsystem link
         readSymbolicLink (Just hdl) "subsystem"
            -- on success, only keep the basename as it is the subsystem name
            >.-.> (Just . Text.pack . takeBaseName)
            -- on error, use Nothing for the subsystem
            >..-.> const Nothing
            -- then create the node
            >.~.> (\subsystem -> do
                     node <- DeviceNode subsystem (Map.fromList children)
                              <$> sysIO newBroadcastTChanIO
                              <*> sysIO newBroadcastTChanIO
                              <*> sysIO newBroadcastTChanIO
                              <*> sysIO newBroadcastTChanIO
                              <*> sysIO newBroadcastTChanIO
                              <*> sysIO newBroadcastTChanIO
                     return (name,node))

      flags = BitSet.fromList [ HandleDirectory
                              , HandleNonBlocking
                              , HandleDontFollowSymLinks
                              ]

   -- list devices in /devices
   tree <- withOpenAt sysfs "devices" flags BitSet.empty 
                        (readSysfsDir (Text.pack "devices") (Text.pack "/devices"))
           >..%~!!> (\err -> sysError ("Cannot read /devices in sysfs: " ++ show (err :: ErrorCode)))
           >.-.> snd
           |> flowRes


   -- build subsystem index
   -- TODO
   let
      --makeIndex path node
      subIndex = undefined

   -- device node counter
   devNum <- sysIO (newTVarIO 0)

   -- create device manager
   subIndex' <- sysIO (newTVarIO subIndex)
   tree'     <- sysIO (newTVarIO tree)
   let dm = DeviceManager
               { dmDevices    = tree'
               , dmSubsystems = subIndex'
               , dmEvents     = bch
               , dmSysFS      = sysfs
               , dmDevFS      = devfs
               , dmDevNum     = devNum
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

      let 
         -- convert a path into a list of directory names
         -- (drop "", the blank before the first "/")
         makeTreePath s = Text.split (== '/') s |> tail
                            
         -- path into the tree for the event
         devTreePath = makeTreePath (kernelEventDevPath ev)

         -- move a node from (x:xs) to (y:ys) in the root tree
         move :: [Text] -> [Text] -> DeviceTree -> Maybe (DeviceTree, DeviceTree)
         move (x:xs) (y:ys) root
            -- we only modify the subtree concerned by the move
            | x == y    = do
               p      <- lookup [x] root
               (p',n) <- move xs ys p
               Just (insert [x] p' root, n)
         move xs ys root = lookup xs root >>= \n -> Just (insert ys n (remove xs root), n)

         -- lookup for a node
         lookup :: [Text] -> DeviceTree -> Maybe DeviceTree
         lookup path root = case path of
            []     -> error "lookup: empty path"
            [x]    -> Map.lookup x (deviceNodeChildren root)
            (x:xs) -> lookup xs =<< Map.lookup x (deviceNodeChildren root)

         -- remove a node in the tree
         remove path root = root { deviceNodeChildren = cs' }
            where
               cs = deviceNodeChildren root
               cs' = case path of
                        []     -> error "remove: empty path"
                        [x]    -> Map.delete x cs
                        (x:xs) -> Map.update (Just . remove xs) x cs

         -- insert a node in the tree
         insert path node root = root { deviceNodeChildren = cs' }
            where
               cs = deviceNodeChildren root
               cs' = case path of
                        []     -> error "insert: empty path"
                        [x]    -> Map.insert x node cs
                        (x:xs) -> Map.update (Just . insert xs node) x cs

         signalEvent f = do
            act <- sysIO $ atomically $ do
               tree <- readTVar (dmDevices dm)
               case lookup devTreePath tree of
                  Just node -> do
                     writeTChan (f node) ev
                     return (return ())
                  Nothing   -> do
                     let s = "Event received for non existing device: "
                             ++ show (kernelEventDevPath ev)
                     return (sysWarning s)
            act


      case Text.unpack (head devTreePath) of
         -- TODO: handle module ADD/REMOVE (/module/* path)
         "module" -> sysWarning "sysfs event in /module ignored"
         
         -- event in the device tree: update the device tree and trigger rules
         "devices" -> case kernelEventAction ev of

            ActionAdd        -> do
               -- read the subsystem link
               let subpath = Text.unpack (kernelEventDevPath ev) </> "subsystem"
               readSymbolicLink (Just (dmSysFS dm)) subpath
                  >.-.> Just . Text.pack . takeBaseName
                  >..-.> const Nothing
                  -- on success, add the node in the tree and signal it
                  >.~!> (\subsystem -> do
                              -- create the node (without any child, there will
                              -- be events for them too)
                              node <- DeviceNode subsystem Map.empty
                                       <$> sysIO newBroadcastTChanIO
                                       <*> sysIO newBroadcastTChanIO
                                       <*> sysIO newBroadcastTChanIO
                                       <*> sysIO newBroadcastTChanIO
                                       <*> sysIO newBroadcastTChanIO
                                       <*> sysIO newBroadcastTChanIO
                              sysIO $ atomically $ do
                                 modifyTVar (dmDevices dm) (insert devTreePath node)
                                 -- Add device into subsystem index
                                 -- TODO
                                 -- Signal the addition
                                 -- TODO
                        )


            ActionRemove     -> do
               act <- sysIO $ atomically $ do
                  tree <- readTVar (dmDevices dm)
                  case lookup devTreePath tree of
                     Just node  -> do
                        writeTVar (dmDevices dm) (remove devTreePath tree)
                        writeTChan (deviceNodeOnRemove node) ev
                        -- Remove from index
                        -- TODO
                        return (return ())
                     Nothing -> do
                        let s = "Remove event received for non existing device: "
                                 ++ show (kernelEventDevPath ev)
                        return (sysWarning s)
               act
               
            ActionChange     -> signalEvent deviceNodeOnChange
            ActionOnline     -> signalEvent deviceNodeOnOnline
            ActionOffline    -> signalEvent deviceNodeOnOffline
            ActionOther _    -> signalEvent deviceNodeOnOther

            -- A device can be moved/renamed in the device tree (see kobject_rename
            -- in lib/kobject.c in the kernel sources)
            ActionMove       -> do
               -- get old device path
               let oldPath' = Map.lookup (Text.pack "DEVPATH_OLD") (kernelEventDetails ev)
               oldPath <- case oldPath' of
                  Nothing -> sysError "Cannot find DEVPATH_OLD entry for device move kernel event"
                  Just x  -> return x
               let
                  oldTreePath = makeTreePath oldPath
                  newTreePath = devTreePath

               act <- sysIO $ atomically $ do
                  -- move the device in the tree
                  tree <- readTVar (dmDevices dm)
                  case move oldTreePath newTreePath tree of
                     Just (tree',node) -> do
                        writeTVar (dmDevices dm) tree'
                        -- signal the event
                        writeTChan (deviceNodeOnMove node) ev
                        return (return ())
                     -- FIXME: we should ensure that the target device has been
                     -- read from sysfs
                     Nothing           -> do
                        let s = "Move event received for non existing device: "
                                ++ show (kernelEventDevPath ev)
                        return (sysWarning s)
               act

         -- warn on unrecognized event
         str -> sysWarning ("sysfs event in /" ++ str ++ " ignored")


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
      logS    = "Opening "
                ++ case typ of
                     CharDevice  -> "character"
                     BlockDevice -> "block"
                ++ " device "
                ++ show dev
                ++ " into "
                ++ devname

   sysLogSequence logS $ do
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
listDevicesWithClass dm cls =
      -- try to open /class/CLASS directory
      readDevs ("class" </> cls)
      -- in case of error, try to open /bus/CLASS/devices
      >..~#> const (readDevs ("bus" </> cls </> "devices"))
      -- in case of error, we don't return any dev
      >..~#> const (flowRet [])
      |> flowRes
   where 

      readDevs path = withOpenAt (dmSysFS dm) path BitSet.empty BitSet.empty $ \clsHdl -> do
         -- list devices in a class
         dirs <- sysCallAssert ("List devices in "++ path ++" directory") $ listDirectory clsHdl
         let dirs'  = fmap entryName dirs
         flowTraverse (readDev clsHdl) dirs' >.-.> catMaybes


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
      readDev hdl dir = do
         -- read symlink pointing into /devices (we shall not directly use/return paths in /class)
         readSymbolicLink (Just hdl) dir
            >.~&> (\path -> 
               -- the link is relative to the current dir (i.e., /class/CLASS or
               -- /bus/CLASS/devices)
               withOpenAt hdl (path </> "dev") BitSet.empty BitSet.empty readDevFile
                  -- return an absolute path of the form "/devices/*"
                  >.-.>  (Just . (concat ("/" : dropWhile (/= "devices/") (splitPath path)),)))
            -- on error (e.g., if there is no "dev" file), skip the device directory
            >..~#> const (flowRet Nothing)


