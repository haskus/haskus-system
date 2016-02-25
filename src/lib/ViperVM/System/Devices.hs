-- | Manage devices
module ViperVM.System.Devices
   ( newKernelEventWaiterThread
   , makeSimpleDeviceEventListener
   )
where

import ViperVM.Arch.Linux.KernelEvent
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Error

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Trans.Class (lift)
import System.Posix.Types (Fd(..))


-- | Create a new thread reading kernel events and putting them in a TChan
newKernelEventWaiterThread :: Sys (TChan KernelEvent)
newKernelEventWaiterThread = do
   fd <- createKernelEventSocket
   ch <- lift $ newBroadcastTChanIO
   let
      FileDescriptor lowfd = fd
      rfd = Fd (fromIntegral lowfd)
      go  = forever $ do
               threadWaitRead rfd
               ev <- runSys $ receiveKernelEvent fd
               atomically $ writeTChan ch ev

   void $ lift $ forkIO go
   return ch

makeSimpleDeviceEventListener :: TChan KernelEvent -> (KernelEvent -> IO ()) -> Sys ()
makeSimpleDeviceEventListener bch f = do
   sysLog LogInfo "Creating simple kernel event listener"

   lift $ do
      ch <- atomically $ dupTChan bch
      void $ forkIO $ forever (atomically (readTChan ch) >>= f)
