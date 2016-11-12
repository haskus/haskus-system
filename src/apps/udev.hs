import ViperVM.System.Sys
import ViperVM.System.Network
import ViperVM.Utils.Flow

main :: IO ()
main = runSys' $ do
   fd <- createKernelEventSocket
   forever $ do
      msg <- receiveKernelEvent fd
      liftIO $ putStrLn (show msg)
