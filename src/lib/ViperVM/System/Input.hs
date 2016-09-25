-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , loadInputDevices
   , InputEventBundle (..)
   , newInputEventHandler
   )
where

import ViperVM.System.Sys
import ViperVM.System.Event
import ViperVM.System.Devices
import ViperVM.Utils.Flow
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Input as Input
import ViperVM.Format.Binary.Enum
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
         >.~?> (\hdl -> do
                  eventChannel  <- newEventReader hdl
                  bundleChannel <- newInputEventHandler eventChannel
                  InputDevice devpath dev hdl
                     <$< sysIO (Input.getDeviceName hdl)
                     <*< sysIO (Input.getDeviceInfo hdl)
                     <&< flowRet' eventChannel
                     <&< flowRet' bundleChannel
               )

-- | Bundle of events
--
-- Evdev sends a series of input events to describe a single "action" (e.g., if
-- a mouse is moved diagonaly, there will be one event for each axis), then it
-- sends a synchronization event. We bundle these events into a single
-- InputEventBundle.
newtype InputEventBundle = InputEventBundle [Event]


-- | Convert a stream a input events into a stream of input event bundles
--
-- Note: maybe we could in the future totally get rid of the channel of events
-- to only keep the channel of event bundles. It would avoid going through an
-- intermediate channel (current implementation, that could be improved too).
-- For now, we keep the event channel, mostly for debugging purpose.
newInputEventHandler :: TChan Input.Event -> Sys (TChan InputEventBundle)
newInputEventHandler eventChannel = do
   bundleChannel <- sysIO newBroadcastTChanIO
   onEventWithData [] eventChannel $ \xs ev -> do
      case (fromEnumField (eventType ev), eventCode ev, eventValue ev) of
         -- Ignore kernel generated key-repeat events
         -- TODO: disable them in the kernel instead
         (EventTypeKey, _, 2)  -> return xs
         -- On synchronization, commit the bundle (without the sync event)
         (EventTypeSync, 0, 0) -> do
            let bundle = InputEventBundle (reverse xs)
            sysIO $ atomically $ writeTChan bundleChannel bundle
            return []
         -- otherwise append the event
         _                     -> return (ev:xs)
   return bundleChannel
