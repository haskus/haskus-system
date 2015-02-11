import ViperVM.Arch.X86_64.Linux.Power
import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.Linux.ErrorCode

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   putStrLn "Press a key to continue"
   waitKey

   putStrLn "And now, shutting down"
   check $ sysSync
   check $ sysPower PowerOff


waitKey :: IO ()
waitKey = getChar >> return ()

check :: SysRet a -> IO a
check f = do
   r <- f
   case r of
      Left err -> do
         error (show err)
      Right v -> return v
