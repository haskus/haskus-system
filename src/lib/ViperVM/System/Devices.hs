-- | Manage devices
module ViperVM.System.Devices
   ( makeKernelEventChannel
   , onEvent
   )
where

import ViperVM.Arch.Linux.KernelEvent
import ViperVM.Arch.Linux.Handle
import ViperVM.System.Sys
import ViperVM.System.Process

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever)
import System.Posix.Types (Fd(..))


-- | Create a new thread reading kernel events and putting them in a TChan
makeKernelEventChannel :: Sys (TChan KernelEvent)
makeKernelEventChannel = do
   fd <- createKernelEventSocket
   ch <- sysIO newBroadcastTChanIO
   let
      Handle lowfd = fd
      rfd = Fd (fromIntegral lowfd)
      go  = sysIO $ forever $ do
               threadWaitRead rfd
               ev <- runSys $ receiveKernelEvent fd
               atomically $ writeTChan ch ev

   sysFork go
   return ch

-- | Read events in the given channel forever
onEvent :: TChan e -> (e -> Sys ()) -> Sys ()
onEvent bch f = do
   sysLog LogInfo "Creating event listener"

   ch <- sysIO $ atomically $ dupTChan bch
   sysFork $ sysIO $ forever (atomically (readTChan ch) >>= runSys' . f)
