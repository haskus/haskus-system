{-# LANGUAGE OverloadedStrings #-}

-- | Kernel events are sent by the kernel to indicate that something
-- changed in the device tree (e.g. device (un)plugged, moved, etc.)
module ViperVM.Arch.Linux.KernelEvent
   ( KernelEvent(..)
   , KernelEventAction(..)
   , createKernelEventSocket
   , receiveKernelEvent
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.Network
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Network.SendReceive
import ViperVM.Arch.Linux.Error
import ViperVM.System.Sys

import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | A kernel event
data KernelEvent = KernelEvent
   { kernelEventAction    :: KernelEventAction     -- ^ What happened
   , kernelEventDevPath   :: Text                  -- ^ Concerned device
   , kernelEventSubSystem :: Text                  -- ^ Device subsystem
   , kernelEventDetails   :: Map Text Text         -- ^ Event details
   } deriving Show

-- | Kernel event type of action
data KernelEventAction
   = ActionAdd             -- ^ A device has been added
   | ActionRemove          -- ^ A device has been removed
   | ActionChange          -- ^ A device state has been modified
   | ActionOnline          -- ^ A device is now on-line
   | ActionOffline         -- ^ A device is now off-line
   | ActionMove            -- ^ A device has been moved
   | ActionOther Text      -- ^ Other action
   deriving (Show)

-- | Create a socket for kernel events
createKernelEventSocket :: Sys Handle
createKernelEventSocket = sysLogSequence "Create kernel event socket" $ do
   -- internally the socket is a Netlink socket dedicated to kernel events
   fd <- sysCallAssert "Create NetLink socket" $ sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []
   -- bind the socket to any port (i.e. port 0), listen to all multicast groups
   sysCallAssert "Bind the socket" $ sysBindNetlink fd 0 0xFFFFFFFF
   return fd


-- | Block until a kernel event is received
receiveKernelEvent :: Handle -> Sys KernelEvent
receiveKernelEvent fd = go
   where
      go = do
         msg <- sysCallAssertQuiet "Receive kernel event" $ receiveByteString fd 2048 BitSet.empty
         case parseKernelEvent msg of
            Just m  -> return m
            Nothing -> sysError "Kernel event is not parsable"


-- | Parse a kernel event
--
-- Kernel events are received as several zero-terminal strings. The first line
-- isn't very useful because it is redundant with the content of the following
-- lines. The following lines have the "key=value" format.
--
-- Note: when kernel event sockets are used with a classic Linux distribution
-- using udev, libudev injects its own events with their own syntax. Hence we
-- discard them (they all begin with "libudev" characters).
parseKernelEvent :: BS.ByteString -> Maybe KernelEvent
parseKernelEvent bs = r
   where
      bss = fmap Text.decodeUtf8 (BS.split 0 bs)
      r = case bss of
            -- filter out injected libudev events
            ("libudev":_) -> Nothing
            _             -> Just (KernelEvent action devpath subsys details)

      -- parse fields
      fields = Map.fromList                      -- create Map from (key,value) tuples
             . fmap (toTuple . Text.splitOn "=") -- split "key=value"
             . filter (not . Text.null)          -- drop empty lines
             $ tail bss                          -- drop the first line (it contains redundant info)

      action = case fields Map.! "ACTION" of
         "add"    -> ActionAdd
         "remove" -> ActionRemove
         "change" -> ActionChange
         "online" -> ActionOnline
         "offline"-> ActionOffline
         "move"   -> ActionMove
         x        -> ActionOther x

      devpath = fields Map.! "DEVPATH"

      subsys  = fields Map.! "SUBSYSTEM"

      -- remove mandatory fields from the "details" field
      details = Map.delete "SUBSYSTEM" 
              . Map.delete "ACTION" 
              $ Map.delete "DEVPATH" fields

      toTuple (x:y:_) = (x,y)
      toTuple x = error $ "Invalid tuple: " ++ show x
