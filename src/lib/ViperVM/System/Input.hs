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

import Prelude hiding (init,tail)
import Control.Monad (void)
import Data.Maybe (isJust)

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)


data InputDevice = InputDevice
   { inputDevicePath             :: FilePath          -- ^ SysFS path
   , inputDeviceDev              :: Device            -- ^ Device ID
   , inputDeviceHandle           :: FileDescriptor    -- ^ Descriptor
   , inputDeviceName             :: String            -- ^ Device Name
   } deriving (Show)


loadInputDevices :: System -> Sys [InputDevice]
loadInputDevices system = sysLogSequence "Load input devices" $ do
   let
      parseInput = void (string "event" >> decimal)
      isInput    = isJust . parseMaybe parseInput
   devs <- listDevicesWithClass system "input" isInput
   forM devs $ \(devpath,dev) -> do
      fd   <- openDevice system CharDevice dev
      name <- sysCallAssert "Get device name" $
                  Input.getDeviceName sysIoctl fd
      return (InputDevice devpath dev fd name)
