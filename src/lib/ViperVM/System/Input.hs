-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , loadInputDevices
   )
where

import ViperVM.System.System
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Input.Device as Input

import Data.Traversable (forM)

data InputDevice = InputDevice
   { inputDevicePath             :: FilePath          -- ^ SysFS path
   , inputDeviceDev              :: Device            -- ^ Device ID
   , inputDeviceHandle           :: FileDescriptor    -- ^ Descriptor
   , inputDeviceName             :: String            -- ^ Device Name
   } deriving (Show)


loadInputDevices :: System -> Sys [InputDevice]
loadInputDevices system = sysLogSequence "Load input devices" $ do
   devs <- listDevicesWithClass system "input" (const True)
   forM devs $ \(devpath,dev) -> do
      fd   <- openDevice system CharDevice dev
      name <- sysCallAssert "Get device name" $
                  Input.getDeviceName sysIoctl fd
      return (InputDevice devpath dev fd name)
