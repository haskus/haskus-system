-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , loadInputDevices
   )
where

import ViperVM.System.Sys
import ViperVM.System.Event
import ViperVM.System.Devices
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Internals.Input as Input
import qualified ViperVM.Format.Text as Text

import Control.Concurrent.STM
import Data.Traversable (forM)
import Prelude hiding (init,tail)
import Control.Monad (void)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import System.FilePath (takeBaseName)

-- | Input device
data InputDevice = InputDevice
   { inputDevicePath             :: DevicePath        -- ^ Device path
   , inputDeviceDev              :: Device            -- ^ Device ID
   , inputDeviceHandle           :: Handle            -- ^ Descriptor
   , inputDeviceName             :: String            -- ^ Device Name
   , inputDeviceInfo             :: DeviceInfo        -- ^ Device info
   , inputDeviceChan             :: TChan Input.Event -- ^ Event stream
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
   forM devs' $ \(devpath,dev) -> do
      fd   <- getDeviceHandle dm dev
      void $ sysCallWarn "Grab device" $ grabDevice fd
      InputDevice devpath dev fd
         <$> sysCallAssert "Get device name"
                  (Input.getDeviceName fd)
         <*> sysCallAssert "Get device info"
                  (Input.getDeviceInfo fd)
         <*> newEventReader fd
