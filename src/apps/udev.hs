import ViperVM.Arch.Linux.KernelEvent
import ViperVM.System.Sys

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)

main :: IO ()
main = runSys' $ do
   fd <- createKernelEventSocket
   forever $ do
      msg <- receiveKernelEvent fd
      liftIO $ putStrLn (show msg)
