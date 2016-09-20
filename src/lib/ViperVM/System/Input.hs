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
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Internals.Input as Input

import Control.Concurrent.STM
import Data.Traversable (forM)
import Prelude hiding (init,tail)
import Control.Monad (void)
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

-- | Input device
data InputDevice = InputDevice
   { inputDevicePath             :: FilePath          -- ^ SysFS path
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
      isEvent (p,_) = "event" `isPrefixOf` takeBaseName p
      devs' = filter isEvent devs
   forM devs' $ \(devpath,dev) -> do
      fd   <- getDeviceHandle dm (Device CharDevice dev)
      void $ sysCallWarn "Grab device" $ grabDevice fd
      InputDevice devpath (Device CharDevice dev) fd
         <$> sysCallAssert "Get device name"
                  (Input.getDeviceName fd)
         <*> sysCallAssert "Get device info"
                  (Input.getDeviceInfo fd)
         <*> newEventReader fd
