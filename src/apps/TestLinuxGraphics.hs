import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.X86_64.Linux.FileSystem

import Control.Monad.Trans.Either
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
   let ioctl = drmIoctl sysIoctl

   ret <- runEitherT $ do
      fd <- EitherT $ sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] []
      _ <- (/= 0) <$> (EitherT $ getCapability ioctl fd CapDumbBuffer)
      liftIO $ putStrLn "The card has DumbBuffer capability :)"

   case ret of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> return ()
