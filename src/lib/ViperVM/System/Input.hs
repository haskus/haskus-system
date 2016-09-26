{-# LANGUAGE RecordWildCards #-}

-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , InputEvent (..)
   , InputEventType (..)
   , EventType (..)
   , loadInputDevices
   , InputEventBundle (..)
   , newInputEventHandler
   -- re-export
   , SyncEventType (..)
   , KeyEventType (..)
   , Key  (..)
   , RelativeAxe (..)
   , AbsoluteAxe (..)
   , MiscEventType (..)
   , SwitchEventType (..)
   , LED (..)
   , Sound (..)
   )
where

import ViperVM.System.Sys
import ViperVM.System.Event
import ViperVM.System.Devices
import ViperVM.Utils.Flow
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Input as Input
import ViperVM.Arch.Linux.Time (TimeVal)
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import qualified ViperVM.Format.Text as Text

import Control.Concurrent.STM
import Prelude hiding (init,tail)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import System.FilePath (takeBaseName)

-- | Input device
data InputDevice = InputDevice
   { inputDevicePath    :: DevicePath             -- ^ Device path
   , inputDeviceDev     :: Device                 -- ^ Device ID
   , inputDeviceHandle  :: Handle                 -- ^ Descriptor
   , inputDeviceName    :: String                 -- ^ Device Name
   , inputDeviceInfo    :: DeviceInfo             -- ^ Device info
   , inputDeviceEvents  :: TChan Input.Event      -- ^ Event stream
   , inputDeviceBundles :: TChan InputEventBundle -- ^ Event bundle stream
   }

-- | Input event
data InputEvent = InputEvent
   { inputEventTime :: !TimeVal        -- ^ Event date
   , inputEventType :: !InputEventType -- ^ Event type
   } deriving (Show,Eq)

-- | Input event details
data InputEventType
   = InputSyncEvent !SyncEventType !Int32         -- ^ Synchronization event
   | InputKeyEvent !KeyEventType !Word16          -- ^ Key event
   | InputRelativeEvent !RelativeAxe !Int32       -- ^ Relative event
   | InputAbsoluteEvent !AbsoluteAxe !Int32       -- ^ Absolute event
   | InputMiscEvent !MiscEventType !Int32         -- ^ Misc event
   | InputSwitchEvent !SwitchEventType !Int32     -- ^ Switch event
   | InputLEDEvent !LED !Int32                    -- ^ LED event
   | InputSoundEvent !Sound !Int32                -- ^ Sound event
   | InputReplayEvent !Word16 !Int32              -- ^ Replay event
   | InputForceFeedbackEvent !Word16 !Int32       -- ^ Force feedback event
   | InputPowerEvent !Word16 !Int32               -- ^ Power event
   | InputForceFeedbackStatusEvent !Word16 !Int32 -- ^ Force feedback statusevent
   deriving (Show,Eq)

-- | Bundle of events
--
-- Evdev sends a series of input events to describe a single "action" (e.g., if
-- a mouse is moved diagonaly, there will be one event for each axis), then it
-- sends a synchronization event. We bundle these events into a single
-- InputEventBundle.
newtype InputEventBundle = InputEventBundle [InputEvent] deriving (Show,Eq)

-- | Convert a raw input event into an InputEvent
makeInputEvent :: Input.Event -> InputEvent
makeInputEvent (Input.Event {..}) = InputEvent eventTime t
   where
      c = eventCode
      v = eventValue
      t = case fromEnumField eventType of
            EventTypeSync                -> InputSyncEvent (toCEnum c) v
            EventTypeKey                 -> InputKeyEvent (toCEnum v) c
            EventTypeRelative            -> InputRelativeEvent (toCEnum c) v
            EventTypeAbsolute            -> InputAbsoluteEvent (toCEnum c) v
            EventTypeMisc                -> InputMiscEvent (toCEnum c) v
            EventTypeSwitch              -> InputSwitchEvent (toCEnum c) v
            EventTypeLED                 -> InputLEDEvent (toCEnum c) v
            EventTypeSound               -> InputSoundEvent (toCEnum c) v
            EventTypeReplay              -> InputReplayEvent c v
            EventTypeForceFeedback       -> InputForceFeedbackEvent c v
            EventTypePower               -> InputPowerEvent c v
            EventTypeForceFeedbackStatus -> InputForceFeedbackStatusEvent c v

-- | List and load devices with the "input" class
loadInputDevices :: DeviceManager -> Sys [InputDevice]
loadInputDevices dm = sysLogSequence "Load input devices" $ do
   devs <- listDevicesWithClass dm "input"
   let
      isEvent (p,_) = "event" `isPrefixOf` takeBaseName (Text.unpack p)
      hasDevice (p,d) = case deviceDevice d of
         Nothing -> Nothing
         Just x  -> Just (p,x)
      devs' = filter isEvent (mapMaybe hasDevice devs)
   flowForFilter devs' $ \(devpath,dev) -> do
      getDeviceHandle dm dev
         -- try to grab device
         >.~=> (\hdl -> sysIO (grabDevice hdl)
                           >..~!> sysWarningShow "Cannot grab device")
         -- try to read infos and return InputDevice
         >.~-> (\hdl -> do
                  eventChannel  <- newEventReader hdl
                  bundleChannel <- newInputEventHandler eventChannel
                  InputDevice devpath dev hdl
                     <$< sysIO (Input.getDeviceName hdl)
                     <*< sysIO (Input.getDeviceInfo hdl)
                     <|< flowRet0' eventChannel
                     <|< flowRet0' bundleChannel
               )



-- | Convert a stream a input events into a stream of input event bundles
--
-- Note: maybe we could in the future totally get rid of the channel of events
-- to only keep the channel of event bundles. It would avoid going through an
-- intermediate channel (current implementation, that could be improved too).
-- For now, we keep the event channel, mostly for debugging purpose.
-- We would also need to check that it doesn't increase the number of dropped
-- synchronization events.
--
-- TODO: handle SyncDropped (reader not fast enough to read kernel generated
-- events, leading the kernel to drop events)
newInputEventHandler :: TChan Input.Event -> Sys (TChan InputEventBundle)
newInputEventHandler eventChannel = do
   bundleChannel <- sysIO newBroadcastTChanIO
   onEventWithData [] eventChannel $ \xs ev' -> do
      let ev = makeInputEvent ev'
      case inputEventType ev of
         -- Ignore kernel generated key-repeat events
         -- TODO: disable them in the kernel instead
         InputKeyEvent KeyRepeat _ -> return xs
         -- On synchronization, commit the bundle (without the sync event)
         InputSyncEvent SyncReport 0 -> do
            let bundle = InputEventBundle (reverse xs)
            sysIO $ atomically $ writeTChan bundleChannel bundle
            return []
         -- otherwise append the event
         _                     -> return (ev:xs)
   return bundleChannel
