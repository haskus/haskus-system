{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Devices management
--
-- This module allows the creation of a 'DeviceManager' which:
--
--    * maintains an up-to-date tree of connected devices
--    * maintains device index by subsystem type
--    * signals when tree changes through STM channels
--    * allows query of devices
--    * allows device property querying/setting
--
-- Internally, it relies on Linux's sysfs and on a socket to receive netlink
-- kernel object events.
--
module Haskus.System.Devices
   ( Device (..)
   -- * Device manager
   , DeviceManager (..)
   , initDeviceManager
   , deviceAdd
   , deviceMove
   , deviceRemove
   , deviceLookup
   -- * Rules
   , Rule
   , ruleDesc
   , rulePriority
   , RuleMatch
   , RuleAction
   , RuleActionResult (..)
   , addRule
   , removeRule
   -- * Device tree
   , DeviceTree (..)
   , DevicePath
   , SubsystemIndex (..)
   , deviceTreeCreate
   , deviceTreeInsert
   , deviceTreeRemove
   , deviceTreeLookup
   , deviceTreeMove
   -- * Various
   , getDeviceHandle
   , getDeviceHandleByName
   , releaseDeviceHandle
   , openDeviceDir
   , listDevicesWithClass
   , listDeviceClasses
   , listDevices
   )
where

import Prelude hiding (lookup)

import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Haskus.Utils.Text (Text)
import qualified Haskus.Utils.Text as Text
import Haskus.System.Linux.Error
import Haskus.System.Linux.Devices
import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem
import Haskus.System.Linux.FileSystem.Directory
import Haskus.System.Linux.KernelEvent
import Haskus.System.Sys
import Haskus.System.FileSystem
import Haskus.System.Process
import Haskus.System.Network
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
import Haskus.Utils.STM
import Haskus.Utils.List
import Haskus.Utils.Text (textFormat,string,shown,(%))

import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

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

-------------------------------------------------------------------------------
-- Device manager
-------------------------------------------------------------------------------

-- | Device manager
data DeviceManager = DeviceManager
   { dmEvents         :: TChan KernelEvent              -- ^ Netlink kobject events
   , dmSysFS          :: Handle                         -- ^ Handle to sysfs
   , dmDevFS          :: Handle                         -- ^ root of the tmpfs used to create device nodes
   , dmDevNum         :: TVar Word64                    -- ^ counter used to create device node
   , dmDevices        :: TVar DeviceTree                -- ^ Device hierarchy
   , dmSubsystems     :: TVar (Map Text SubsystemIndex) -- ^ Per-subsystem index
   , dmOnSubsystemAdd :: TChan Text                     -- ^ When a new subsystem appears
   , dmRules          :: TVar [Rule]                    -- ^ Rules triggered when an event occurs
   , dmRuleIndex      :: TVar Integer                   -- ^ Rule id generator
   }

-- | Init a device manager
initDeviceManager :: Handle -> Handle -> Sys DeviceManager
initDeviceManager sysfs devfs = do
   
   -- open Netlink socket and then duplicate the kernel event channel so that
   -- events start accumulating until we launch the handling thread
   bch <- newKernelEventReader
   ch <- atomically $ dupTChan bch

   -- create empty device manager
   root      <- deviceTreeCreate Nothing Nothing Map.empty
   devNum    <- newTVarIO 0          -- device node counter
   subIndex' <- newTVarIO Map.empty
   tree'     <- newTVarIO root
   sadd      <- newBroadcastTChanIO
   rules     <- newTVarIO []
   ruleIndex <- newTVarIO 0
   let dm = DeviceManager
               { dmDevices        = tree'
               , dmSubsystems     = subIndex'
               , dmEvents         = bch
               , dmSysFS          = sysfs
               , dmDevFS          = devfs
               , dmDevNum         = devNum
               , dmOnSubsystemAdd = sadd
               , dmRules          = rules
               , dmRuleIndex      = ruleIndex
               }

   -- we enumerate devices from sysfs. Directory listing is non-atomic so
   -- directories may appear or be removed while we do the traversal. Hence we
   -- shouldn't fail on error, just skip the erroneous directories.
   --
   -- After the traversal, kernel events potentially received during the
   -- traversal are used to create/remove nodes. We have to be liberal in their
   -- interpretation: e.g., a remove event could be received for a directory we
   -- haven't been able to read, etc.
   let

      withDevDir hdl path f = withOpenAt hdl path flags BitSet.empty f
      flags :: HandleFlags
      flags = BitSet.fromList [ HandleDirectory
                              , HandleNonBlocking
                              , HandleDontFollowSymLinks
                              ]

      -- read a sysfs device directory and try to create a DeviceTree
      -- recursively from it. The current directory is already opened and the
      -- handle is passed (alongside the name and fullname).
      -- Return Nothing if it fails for any reason.
      readSysfsDir :: Text -> Handle -> Excepts '[] Sys ()
      readSysfsDir path hdl = do
         
         unless (Text.null path) $
            lift (deviceAdd dm path Nothing)

         -- list directories (sub-devices) that are *not* symlinks
         dirs <- listDirectory hdl
                    -- filter to keep only directories (sysfs fills the type field)
                    ||> filter (\entry -> entryType entry == TypeDirectory)
                    -- only keep the directory name
                    ||> fmap entryName
                    -- return an empty directory list on error
                    |> catchAllE (\_ -> successE [])

         -- recursively try to create a tree for each sub-dir
         forM_ dirs $ \dir -> do
            let path' = Text.concat [path, Text.pack "/", Text.pack dir]
            withDevDir hdl dir (readSysfsDir path')
               |> catchAllE (\_ -> successE ())

         return ()


   -- list devices in /devices
   withDevDir sysfs "devices" (readSysfsDir Text.empty)
      |> catchEvalE \err -> sysError (textFormat ("Cannot read /devices in sysfs: " % shown) err)
      |> void

   -- launch handling thread
   sysFork "Kernel sysfs event handler" $ eventThread ch dm

   return dm

-- | Thread handling incoming kernel events
eventThread :: TChan KernelEvent -> DeviceManager -> Sys ()
eventThread ch dm = do
   forever $ do
      -- read kernel event
      ev <- atomically (readTChan ch)

      case Text.unpack (fst (bkPath (kernelEventDevPath ev))) of
         -- TODO: handle module ADD/REMOVE (/module/* path)
         "module" -> sysWarningShow "sysfs event in /module ignored"
                        (kernelEventDevPath ev)
         
         -- event in the device tree: update the device tree and trigger rules
         "devices" -> do
            let 
               -- remove "/devices" from the path
               path = Text.drop 8 (kernelEventDevPath ev)

               signalEvent f = do
                  notFound <- atomically $ do
                     tree <- readTVar (dmDevices dm)
                     case deviceTreeLookup path tree of
                        Just node -> do
                           writeTChan (f node) ev
                           return False
                        Nothing   -> return True
                  when notFound $
                     sysWarning (textFormat ("Event received for non existing device: " % shown) path)

            case kernelEventAction ev of
               ActionAdd     -> do
                  sysLogInfoShow "Added device" path
                  deviceAdd dm path (Just ev)
               ActionRemove  -> do
                  sysLogInfoShow "Removed device" path
                  deviceRemove dm path ev
               ActionMove    -> do
                  sysLogInfoShow "Moved device" path
                  deviceMove dm path ev
               ActionChange  -> do
                  sysLogInfoShow "Changed device" path
                  signalEvent deviceNodeOnChange
               ActionOnline  -> do
                  sysLogInfoShow "Device goes online" path
                  signalEvent deviceNodeOnOnline
               ActionOffline -> do
                  sysLogInfoShow "Device goes offline" path
                  signalEvent deviceNodeOnOffline
               ActionBind -> do
                  sysLogInfoShow "Bound device" path
                  signalEvent deviceNodeOnBind
               ActionUnBind -> do
                  sysLogInfoShow "Unbound device" path
                  signalEvent deviceNodeOnUnBind
               ActionOther x -> do
                  let info = mconcat
                        [ "Unknown device action ("
                        , Text.pack (show x)
                        , ")"
                        ]
                  sysLogInfoShow info path
                  signalEvent deviceNodeOnOther

            -- trigger rules
            let execRules []     = pure ()
                execRules (r:rs) = do
                  doesMatch <- ruleMatch r ev
                  case doesMatch of
                     False -> execRules rs
                     True -> do
                        ret <- ruleAction r ev
                        case ret of
                           Nothing       -> execRules rs
                           Just LastRule -> pure () -- stop rule matching now

            execRules =<< readTVarIO (dmRules dm)

         -- warn on unrecognized event
         str -> sysWarningShow (textFormat ("sysfs event in /" % string % " ignored") str) (kernelEventDevPath ev)


-- | Lookup a device by name
deviceLookup :: DeviceManager -> DevicePath -> Sys (Maybe DeviceTree)
deviceLookup dm path = deviceTreeLookup path <$> readTVarIO (dmDevices dm)

-- | Add a device
deviceAdd :: DeviceManager -> DevicePath -> Maybe KernelEvent -> Sys ()
deviceAdd dm path mev = do

   let rpath = "devices" ++ Text.unpack path  -- relative path in sysfs

   (msubsystem,mdev) <- case mev of
      Nothing -> sysfsReadDev (dmSysFS dm) rpath
      -- device id may be read from the event properties
      Just ev -> do
         let 
            detail k    = Map.lookup (Text.pack k) (kernelEventDetails ev)
            detailNum k = fmap (read . Text.unpack) (detail k)
         case (detailNum "MAJOR", detailNum "MINOR") of
            (Just ma, Just mi) -> do
               sub <- sysfsReadSubsystem (dmSysFS dm) rpath
               return (sub, (`sysfsMakeDev` DeviceID ma mi) <$> sub)
            _                  -> sysfsReadDev (dmSysFS dm) rpath

   node <- deviceTreeCreate msubsystem mdev Map.empty

   atomically $ do
      -- update the tree
      tree  <- readTVar (dmDevices dm)
      tree' <- deviceTreeInsert path node tree
      writeTVar (dmDevices dm) tree'

      case msubsystem of
         Nothing        -> return ()
         Just subsystem -> do
            -- Add device into subsystem index
            subs <- readTVar (dmSubsystems dm)
            subs' <- case Map.lookup subsystem subs of

               Nothing    -> do
                  -- create new index
                  index <- SubsystemIndex (Set.singleton path)
                              <$> newBroadcastTChan
                              <*> newBroadcastTChan
                  -- signal the new subsystem
                  writeTChan (dmOnSubsystemAdd dm) subsystem
                  -- return the new index
                  return (Map.insert subsystem index subs)

               Just index -> do
                  let
                     devs   = subsystemDevicePaths index
                     devs'  = Set.insert path devs
                     index' = index { subsystemDevicePaths = devs' }
                  -- signal the addition
                  writeTChan (subsystemOnAdd index) path
                  -- return the new index
                  return (Map.insert subsystem index' subs)

            writeTVar (dmSubsystems dm) subs'

-- | Remove a device
deviceRemove :: DeviceManager -> DevicePath -> KernelEvent -> Sys ()
deviceRemove dm path ev = do
   notFound <- atomically $ do
      tree <- readTVar (dmDevices dm)
      case deviceTreeLookup path tree of
         Just node  -> do
            -- remove from tree and signal
            writeTVar (dmDevices dm) (deviceTreeRemove path tree)
            writeTChan (deviceNodeOnRemove node) ev

            case deviceNodeSubsystem node of
               Nothing -> return ()
               Just s  -> do
                  -- Remove from index
                  subs <- readTVar (dmSubsystems dm)
                  let
                     index  = subs Map.! s
                     devs   = subsystemDevicePaths index
                     index' = index { subsystemDevicePaths = Set.delete path devs}
                  writeTVar (dmSubsystems dm) (Map.insert s index' subs)
                  -- signal for index
                  writeTChan (subsystemOnRemove index) path
            return False
         Nothing -> return True
   
   when notFound $ do
      sysWarning $ textFormat ("Remove event received for non existing device: " % shown) path

-- | Move a device
--
-- A device can be moved/renamed in the device tree (see kobject_rename
-- in lib/kobject.c in the kernel sources)
deviceMove :: DeviceManager -> DevicePath -> KernelEvent -> Sys ()
deviceMove dm path ev = do
   -- get old device path
   let oldPath' = Map.lookup (Text.pack "DEVPATH_OLD") (kernelEventDetails ev)
   oldPath <- case oldPath' of
      Nothing -> sysError "Cannot find DEVPATH_OLD entry for device move kernel event"
      Just x  -> return (Text.drop 8 x) -- remove "/devices"

   notFound <- atomically $ do
      -- move the device in the tree
      tree <- readTVar (dmDevices dm)
      case deviceTreeLookup oldPath tree of
         Just node -> do
            -- move the node in the tree
            tree' <- deviceTreeMove oldPath path tree
            writeTVar (dmDevices dm) tree'
            -- signal the event
            writeTChan (deviceNodeOnMove node) ev
            return False

         Nothing -> return True

   when notFound $ do
      sysWarning $ textFormat ("Move event received for non existing device: " % shown % ". We try to add it") path
      deviceAdd dm path (Just ev)

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

-- | A rule triggered when a device event occurs
--
-- For now `ruleMatch` and `ruleAction` are totally opaque. In the future we may
-- want to make them more declarative to improve matching and to avoid arbitrary
-- side-effects
data Rule = Rule
   { ruleId       :: Integer    -- ^ Rule unique Id (so that it can be easily removed, sorted, etc.)
   , ruleDesc     :: Text       -- ^ Rule description
   , rulePriority :: Float      -- ^ Rule priority
   , ruleMatch    :: RuleMatch  -- ^ Indicate if a rule matches
   , ruleAction   :: RuleAction -- ^ Rule action.
   }

-- | Rule action result
data RuleActionResult
   = LastRule -- ^ Ensure that this is the last rule triggered for a given event

type RuleMatch  = KernelEvent -> Sys Bool
type RuleAction = KernelEvent -> Sys (Maybe RuleActionResult)

-- | Add a rule
addRule :: MonadIO m => DeviceManager -> Text -> Float -> RuleMatch -> RuleAction -> m Rule
addRule dm desc prio match act = atomically do

   i <- readTVar (dmRuleIndex dm)
   modifyTVar' (dmRuleIndex dm) (+1)

   let
      rule = Rule
         { ruleId       = i
         , ruleDesc     = desc
         , rulePriority = prio
         , ruleMatch    = match
         , ruleAction   = act
         }

      -- insert sorted on priority
      insertSorted []     = [rule]
      insertSorted (r:rs)
         | rulePriority r <= rulePriority rule = r : insertSorted rs
         | otherwise                           = rule:r:rs

   modifyTVar (dmRules dm) insertSorted
   return rule

-- | Remove a rule
removeRule :: MonadIO m => DeviceManager -> Rule -> m ()
removeRule dm rule = atomically do
   -- modify strictly to avoid maintaining some objects alive because of a rule
   modifyTVar' (dmRules dm) <|
      -- we only delete the first matching because there can only be one rule
      -- with this Id
      deleteBy (\x y -> ruleId x /= ruleId y) rule

-------------------------------------------------------------------------------
-- Device tree & subsystem index
-------------------------------------------------------------------------------

-- | Device tree
--
-- It is expected that the device tree will not change much after the
-- initialization phase (except when a device is (dis)connected, etc.), hence it
-- is an immutable data structure. It is much easier to perform tree traversal
-- with a single global lock thereafter.
data DeviceTree = DeviceTree
   { deviceNodeSubsystem :: Maybe Text          -- ^ Subsystem
   , deviceDevice        :: Maybe Device        -- ^ Device identifier
   , deviceNodeChildren  :: Map Text DeviceTree -- ^ Children devices
   , deviceNodeOnRemove  :: TChan KernelEvent   -- ^ On "remove" event
   , deviceNodeOnChange  :: TChan KernelEvent   -- ^ On "change" event
   , deviceNodeOnMove    :: TChan KernelEvent   -- ^ On "move" event
   , deviceNodeOnOnline  :: TChan KernelEvent   -- ^ On "online" event
   , deviceNodeOnOffline :: TChan KernelEvent   -- ^ On "offline" event
   , deviceNodeOnBind    :: TChan KernelEvent   -- ^ On "bind" event
   , deviceNodeOnUnBind  :: TChan KernelEvent   -- ^ On "unbind" event
   , deviceNodeOnOther   :: TChan KernelEvent   -- ^ On other events
   }

-- | Per-subsystem events
data SubsystemIndex = SubsystemIndex
   { subsystemDevicePaths :: Set Text   -- ^ Devices in the index
   , subsystemOnAdd       :: TChan Text -- ^ Signal device addition
   , subsystemOnRemove    :: TChan Text -- ^ Signal device removal
   }

type DevicePath = Text

-- | Break a device tree path into (first component, remaining)
bkPath :: DevicePath -> (Text,Text)
bkPath p = second f (Text.breakOn (Text.pack "/") p')
   where
      -- handle paths starting with "/"
      p' = if not (Text.null p) && Text.head p == '/'
               then Text.tail p
               else p
      f xs 
         | Text.null xs = xs
         | otherwise    = Text.tail xs

-- | Create a device tree
deviceTreeCreate :: MonadIO m => Maybe Text -> Maybe Device -> Map Text DeviceTree -> m DeviceTree
deviceTreeCreate subsystem dev children = atomically (deviceTreeCreate' subsystem dev children)

-- | Create a device tree
deviceTreeCreate' :: Maybe Text -> Maybe Device -> Map Text DeviceTree -> STM DeviceTree
deviceTreeCreate' subsystem dev children = DeviceTree subsystem dev children
   <$> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan
   <*> newBroadcastTChan

-- move a node in the tree
deviceTreeMove :: Text -> Text -> DeviceTree -> STM DeviceTree
deviceTreeMove src tgt root = case (bkPath src, bkPath tgt) of
   ((x,xs),(y,ys))
      -- we only modify the subtree concerned by the move
      | x == y    -> do
         case Map.lookup x (deviceNodeChildren root) of
            Nothing -> error "deviceTreeLookup: source node doesn't exists"
            Just p  -> deviceTreeMove xs ys p
      | otherwise -> do
         case deviceTreeLookup src root of
            Nothing -> error "deviceTreeLookup: source node doesn't exists"
            Just n  -> deviceTreeInsert tgt n (deviceTreeRemove src root)

-- lookup for a node
deviceTreeLookup :: Text -> DeviceTree -> Maybe DeviceTree
deviceTreeLookup path root = case bkPath path of
   (x,xs)
      | Text.null x 
        && Text.null xs -> error "deviceTreeLookup': empty path"
      | otherwise       -> do
         n <- Map.lookup x (deviceNodeChildren root)
         if Text.null xs
            then Just n
            else deviceTreeLookup xs n

-- remove a node in the tree
deviceTreeRemove :: Text -> DeviceTree -> DeviceTree
deviceTreeRemove path root = root { deviceNodeChildren = cs' }
   where
      cs = deviceNodeChildren root
      cs' = case bkPath path of
               (x,xs)
                  | Text.null x 
                    && Text.null xs -> error "deviceTreeRemove: empty path"
                  | Text.null xs    -> Map.delete x cs
                  | otherwise       -> Map.update (Just . deviceTreeRemove xs) x cs

-- insert a node in the tree
deviceTreeInsert :: Text -> DeviceTree -> DeviceTree -> STM DeviceTree
deviceTreeInsert path node root = do
   let cs = deviceNodeChildren root

   cs' <- case bkPath path of
      (x,xs)
         | Text.null x && Text.null xs -> error "deviceTreeInsert: empty path"
         | Text.null xs                -> return (Map.insert x node cs)
         | otherwise                   -> 
            case Map.lookup x cs of
               Just p  -> do
                  node' <- deviceTreeInsert xs node p
                  return (Map.insert x node' cs)
               -- the parent doesn't exist yet. Add it. As it should not be a
               -- real device, we don't look for subsystem and device id (i.e.,
               -- we don't call deviceAdd)
               Nothing -> do
                  p <- deviceTreeCreate' Nothing Nothing Map.empty
                  node' <- deviceTreeInsert xs node p
                  return (Map.insert x node' cs)

   return (root { deviceNodeChildren = cs' })

-------------------------------------------------------------------------------
-- Various
-------------------------------------------------------------------------------


-- | Create a new thread reading kernel events and putting them in a TChan
newKernelEventReader :: Sys (TChan KernelEvent)
newKernelEventReader = do
   h  <- createKernelEventSocket
   ch <- newBroadcastTChanIO
   let
      go = forever $ do
               threadWaitRead h
               ev <- receiveKernelEvent h
               atomically $ writeTChan ch ev

   sysFork "Kernel sysfs event reader" go
   return ch


-- | Get device handle by name (i.e., sysfs path)
getDeviceHandleByName :: DeviceManager -> String -> Excepts (ErrorCode ': OpenErrors) Sys Handle
getDeviceHandleByName dm path = do
   dev <- lift <| deviceLookup dm (Text.pack path)
   case dev >>= deviceDevice of
      Just d  -> getDeviceHandle dm d
      Nothing -> throwE DeviceNotFound

-- | Get a handle on a device
--
-- Linux doesn't provide an API to open a device directly from its major and
-- minor numbers. Instead we must create a special device file with mknod in
-- the VFS and open it. This is what this function does. Additionally, we
-- remove the file once it is opened.
getDeviceHandle :: DeviceManager -> Device -> Excepts (ErrorCode ': OpenErrors) Sys Handle
getDeviceHandle dm dev = do

   -- get a fresh device number
   num <- atomically $ do
            n <- readTVar (dmDevNum dm)
            writeTVar (dmDevNum dm) (n+1)
            return n

   let 
      devname = "./dev" ++ show num
      devfd   = dmDevFS dm
      logS    = textFormat ("Opening device " % string % " into " % string)
                  (showDevice dev) devname

   sysLogSequenceL logS <| do
      -- create special file in device fs
      liftE <| createDeviceFile (Just devfd) devname dev BitSet.empty
      -- on success, try to open it
      let flgs = BitSet.fromList [HandleReadWrite,HandleNonBlocking]
      hdl <- liftE <| open (Just devfd) devname flgs BitSet.empty
      -- then remove it
      sysUnlinkAt devfd devname False
         |> warningShowE "Unlinking special device file"
         |> liftE
      return hdl

-- | Release a device handle
releaseDeviceHandle :: Handle -> Sys ()
releaseDeviceHandle fd =
   close fd
   |> warningShowE "close"
   |> runE_

-- | Find device path by number (major, minor)
openDeviceDir :: DeviceManager -> Device -> Excepts OpenErrors Sys Handle
openDeviceDir dm dev = open (Just (dmDevFS dm)) path (BitSet.fromList [HandleDirectory]) BitSet.empty
   where
      path = "./dev/" ++ typ' ++ "/" ++ ids
      typ' = case deviceType dev of
         CharDevice  -> "char"
         BlockDevice -> "block"
      ids  = show (deviceMajor (deviceID dev)) ++ ":" ++ show (deviceMinor (deviceID dev))

-- | List devices
listDevices :: DeviceManager -> Sys [Text]
listDevices dm = atomically (listDevices' dm)

-- | List devices
listDevices' :: DeviceManager -> STM [Text]
listDevices' dm = go Text.empty <$> readTVar (dmDevices dm)
   where
      go parent n = parent : (cs >>= f)
         where
            cs       = Map.assocs (deviceNodeChildren n)
            f (p,n') = go (Text.concat [parent, Text.pack "/", p]) n'


-- | List devices classes
listDeviceClasses :: DeviceManager -> Sys [Text]
listDeviceClasses dm = atomically (Map.keys <$> readTVar (dmSubsystems dm))

-- | List devices with the given class
--
-- TODO: support dynamic asynchronous device adding/removal
listDevicesWithClass :: DeviceManager -> String -> Sys [(DevicePath,DeviceTree)]
listDevicesWithClass dm cls = atomically $ do
   subs <- readTVar (dmSubsystems dm)
   devs <- readTVar (dmDevices dm)
   ds <- listDevices' dm

   let paths = Map.lookup (Text.pack cls) subs
               ||> Set.elems . subsystemDevicePaths
               |> fromMaybe []
       getNode x = case deviceTreeLookup x devs of
         Just n  -> n
         Nothing -> error <| mconcat
            [ "Mismatch between device tree and device subsystem index! Report this as a haskus-system bug. ("
            , show x
            , ") "
            , show ds
            ]
       nodes = fmap getNode paths
   return (paths `zip` nodes)
