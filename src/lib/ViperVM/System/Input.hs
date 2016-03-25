-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , loadInputDevices
   )
where

import ViperVM.System.Sys
import ViperVM.System.System
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Internals.Input as Input

import Control.Concurrent.STM
import Control.Concurrent
import Data.Traversable (forM)
import Data.Foldable (traverse_)
import Prelude hiding (init,tail)
import Control.Monad (void,forever)
import Control.Monad.Trans.Class (lift)
import Foreign.Storable
import Foreign.Marshal (allocaArray, peekArray)
import System.Posix.Types (Fd(..))
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

-- | Input device
data InputDevice = InputDevice
   { inputDevicePath             :: FilePath          -- ^ SysFS path
   , inputDeviceDev              :: Device            -- ^ Device ID
   , inputDeviceHandle           :: FileDescriptor    -- ^ Descriptor
   , inputDeviceName             :: String            -- ^ Device Name
   , inputDeviceInfo             :: DeviceInfo        -- ^ Device info
   , inputDeviceChan             :: TChan Input.Event -- ^ Event stream
   }


-- | List and load devices with the "input" class
loadInputDevices :: System -> Sys [InputDevice]
loadInputDevices system = sysLogSequence "Load input devices" $ do
   devs <- listDevicesWithClass system "input"
   let
      isEvent (p,_) = "event" `isPrefixOf` takeBaseName p
      devs' = filter isEvent devs
   forM devs' $ \(devpath,dev) -> do
      fd   <- getDeviceHandle system CharDevice dev
      void $ sysCallWarn "Grab device" $ grabDevice fd
      InputDevice devpath dev fd
         <$> sysCallAssert "Get device name"
                  (Input.getDeviceName fd)
         <*> sysCallAssert "Get device info"
                  (Input.getDeviceInfo fd)
         <*> newEventWaiterThread fd

-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: FileDescriptor -> Sys (TChan Input.Event)
newEventWaiterThread fd@(FileDescriptor lowfd) = do
   let
      sz  = sizeOf (undefined :: Input.Event)
      rfd = Fd (fromIntegral lowfd)
      nb  = 50 -- number of events read at once

   ch <- lift newBroadcastTChanIO
   void $ lift $ forkIO $ allocaArray nb $ \ptr -> forever $ do
      threadWaitRead rfd
      r <- sysRead fd ptr (fromIntegral sz * fromIntegral nb)
      case r of
         -- FIXME: we should somehow signal that an error occured and
         -- that we won't report future events (if any)
         Left _  -> return ()
         Right sz2 -> do
            evs <- peekArray (fromIntegral sz2 `div` sz) ptr
            atomically $ traverse_ (writeTChan ch) evs
   return ch
