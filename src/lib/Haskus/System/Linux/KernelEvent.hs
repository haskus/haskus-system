{-# LANGUAGE OverloadedStrings #-}

-- | Kernel events are sent by the kernel to indicate that something
-- changed in the device tree (e.g. device (un)plugged, moved, etc.)
module Haskus.System.Linux.KernelEvent
   ( KernelEvent(..)
   , KernelEventAction(..)
   , parseKernelEvent
   )
where

import Haskus.Format.Binary.Buffer
import Haskus.Format.Text (Text)
import qualified Haskus.Format.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

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

-- | Parse a kernel event
--
-- Kernel events are received as several zero-terminal strings. The first line
-- isn't very useful because it is redundant with the content of the following
-- lines. The following lines have the "key=value" format.
--
-- Note: when kernel event sockets are used with a classic Linux distribution
-- using udev, libudev injects its own events with their own syntax to perform
-- netlink communication between processes (expected to be replaced with kdbus
-- at some point). Hence we discard these events (they all begin with "libudev"
-- characters) and return Nothing.
parseKernelEvent :: Buffer -> Maybe KernelEvent
parseKernelEvent bs = r
   where
      bss = fmap Text.bufferDecodeUtf8 (bufferSplitOn 0 bs)
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
