module ViperVM.System.Terminal
   ( waitForKey
   )
where

import ViperVM.Arch.Linux.Terminal
import ViperVM.Arch.Linux.Error
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.Types (Fd(..))

waitForKey :: Sys ()
waitForKey = do
   quitKey <- lift $ atomically $ newTVar False

   lift $ void $ forkIO $ do
      threadWaitRead (Fd 0)
      _ <- runSys' $ sysCallAssertQuiet "Reading char" $ readChar stdin
      atomically $ writeTVar quitKey True

   lift $ atomically $ do
      q <- readTVar quitKey
      case q of
         True  -> return ()
         False -> retry
