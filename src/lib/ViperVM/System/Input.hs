-- | Manage input devices
module ViperVM.System.Input
   ( InputDevice(..)
   , loadInputDevices
   , makeSimpleInputListener
   )
where

import ViperVM.System.System
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Input.Device as Input
import ViperVM.Arch.Linux.Input.Event as Input

import Control.Concurrent.STM
import Control.Concurrent
import Data.Traversable (forM)
import Prelude hiding (init,tail)
import Control.Monad (void, forever)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust)
import Foreign.Storable
import Foreign.Marshal (allocaBytes)
import Foreign.Ptr
import System.Posix.Types (Fd(..))

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)


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
   let
      parseInput = void (string "event" >> decimal)
      isInput    = isJust . parseMaybe parseInput
   devs <- listDevicesWithClass system "input" isInput
   forM devs $ \(devpath,dev) -> do
      fd   <- openDevice system CharDevice dev
      InputDevice devpath dev fd
         <$> (sysCallAssert "Get device name" $
                  Input.getDeviceName sysIoctl fd)
         <*> (sysCallAssert "Get device info" $
                  Input.getDeviceInfo sysIoctl fd)
         <*> newEventWaiterThread fd

-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: FileDescriptor -> Sys (TChan Input.Event)
newEventWaiterThread fd@(FileDescriptor lowfd) = do
   let
      sz  = sizeOf (undefined :: Input.Event)
      rfd = Fd (fromIntegral lowfd)

   ch <- lift $ newBroadcastTChanIO
   void $ lift $ forkIO $ allocaBytes sz $ \ptr -> do
      let go = do
            threadWaitRead rfd
            r <- sysRead fd ptr (fromIntegral sz)
            case r of
               -- FIXME: we should somehow signal that an error occured and
               -- that we won't report future events (if any)
               Left _  -> return ()
               -- FIXME: we should keep the read values for the next iteration
               Right sz2 
                  | sz2 /= fromIntegral sz -> error "We read an incomplete event"
               Right _ -> do
                  ev <- peek (castPtr ptr :: Ptr Input.Event)
                  atomically $ writeTChan ch ev
                  go
      go
   return ch


-- | Execute the given callback on each input event
makeSimpleInputListener :: InputDevice -> (Input.Event -> IO ()) -> Sys ()
makeSimpleInputListener dev f = do
   sysLog LogInfo ("Creating simple listener on device " ++ inputDeviceName dev)
   lift $ do
      ch <- atomically $ dupTChan (inputDeviceChan dev)
      void $ forkIO $ forever (atomically (readTChan ch) >>= f)
