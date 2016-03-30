-- | Manage devices
module ViperVM.System.Devices
   ( makeKernelEventChannel
   , onEvent
   )
where

import ViperVM.Arch.Linux.KernelEvent
import ViperVM.Arch.Linux.Handle
import ViperVM.System.Sys

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Trans.Class (lift)
import System.Posix.Types (Fd(..))


-- | Create a new thread reading kernel events and putting them in a TChan
makeKernelEventChannel :: Sys (TChan KernelEvent)
makeKernelEventChannel = do
   fd <- createKernelEventSocket
   ch <- lift newBroadcastTChanIO
   let
      Handle lowfd = fd
      rfd = Fd (fromIntegral lowfd)
      go  = forever $ do
               threadWaitRead rfd
               ev <- runSys $ receiveKernelEvent fd
               atomically $ writeTChan ch ev

   void $ lift $ forkIO go
   return ch

-- | Read events in the given channel forever
onEvent :: TChan e -> (e -> Sys ()) -> Sys ()
onEvent bch f = do
   sysLog LogInfo "Creating event listener"

   lift $ do
      ch <- atomically $ dupTChan bch
      void $ forkIO $ forever (atomically (readTChan ch) >>= runSys' . f)
