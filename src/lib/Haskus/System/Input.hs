{-# OPTIONS_GHC -freduction-depth=0 #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Manage input devices
module Haskus.System.Input
   ( InputDevice(..)
   , InputEvent (..)
   , InputEventType (..)
   , EventType (..)
   , loadInputDevices
   , InputEventBundle (..)
   , newInputEventHandler
   , makeInputEvent
   , inputSetAutoRepeatDelay
   , inputSetAutoRepeatPeriod
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

import Haskus.System.Sys
import Haskus.System.Event
import Haskus.System.Devices
import Haskus.Utils.Flow
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.Handle
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Internals.Input as Input
import Haskus.System.Linux.Time (TimeVal(..))
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word
import qualified Haskus.Format.Text as Text
import Haskus.Utils.List (isPrefixOf)
import Haskus.Utils.Maybe (mapMaybe)
import Haskus.Utils.STM

import Prelude hiding (init,tail)
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
   | InputKeyEvent !KeyEventType !Key             -- ^ Key event
   | InputRelativeEvent !RelativeAxe !Int32       -- ^ Relative event
   | InputAbsoluteEvent !AbsoluteAxe !Int32       -- ^ Absolute event
   | InputMiscEvent !MiscEventType !Int32         -- ^ Misc event
   | InputSwitchEvent !SwitchEventType !Int32     -- ^ Switch event
   | InputLEDEvent !LED !Int32                    -- ^ LED event
   | InputSoundEvent !Sound !Int32                -- ^ Sound event
   | InputRepeatEvent !Word16 !Int32              -- ^ Repeated event
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
            EventTypeKey                 -> InputKeyEvent (toCEnum v) (toCEnum c)
            EventTypeRelative            -> InputRelativeEvent (toCEnum c) v
            EventTypeAbsolute            -> InputAbsoluteEvent (toCEnum c) v
            EventTypeMisc                -> InputMiscEvent (toCEnum c) v
            EventTypeSwitch              -> InputSwitchEvent (toCEnum c) v
            EventTypeLED                 -> InputLEDEvent (toCEnum c) v
            EventTypeSound               -> InputSoundEvent (toCEnum c) v
            EventTypeRepeat              -> InputRepeatEvent c v
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
         -- try to read infos and return InputDevice
         >.~|> (\hdl -> do
                  eventChannel  <- newEventReader hdl
                  bundleChannel <- newInputEventHandler eventChannel
                  InputDevice devpath dev hdl
                     <$< liftIO (Input.getDeviceName hdl)
                     <*< liftIO (Input.getDeviceInfo hdl)
                     <|< flowSingle eventChannel
                     <|< flowSingle bundleChannel
               )

-- | Configure auto-repeat delay
inputSetAutoRepeatDelay :: MonadInIO m => Handle -> Word32 -> Flow m '[Word64,ErrorCode]
inputSetAutoRepeatDelay hdl delay = do
   let
      tv = TimeVal 0 0
      ev = Input.Event tv (toEnumField EventTypeRepeat) (fromCEnum RepeatDelay) (fromIntegral delay)
   with ev $ \pev ->
      sysWrite hdl pev (sizeOfT' @Input.Event)

-- | Configure auto-repeat period
inputSetAutoRepeatPeriod :: MonadInIO m => Handle -> Word32 -> Flow m '[Word64,ErrorCode]
inputSetAutoRepeatPeriod hdl period = do
   let
      tv = TimeVal 0 0
      ev = Input.Event tv (toEnumField EventTypeRepeat) (fromCEnum RepeatPeriod) (fromIntegral period)
   with ev $ \pev ->
      sysWrite hdl pev (sizeOfT' @Input.Event)
   


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
   bundleChannel <- newBroadcastTChanIO
   onEventWithData [] eventChannel $ \xs ev' -> do
      let ev = makeInputEvent ev'
      case inputEventType ev of
         -- Ignore kernel generated key-repeat events
         -- TODO: disable them in the kernel instead
         InputKeyEvent KeyRepeat _ -> return xs
         -- On synchronization, commit the bundle (without the sync event)
         InputSyncEvent SyncReport 0 -> do
            let bundle = InputEventBundle (reverse xs)
            atomically $ writeTChan bundleChannel bundle
            return []
         -- otherwise append the event
         _                     -> return (ev:xs)
   return bundleChannel
