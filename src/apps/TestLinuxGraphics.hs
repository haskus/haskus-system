import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.X86_64.Linux.FileSystem

import Control.Monad.Trans.Either
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)

main :: IO ()
main = do
   let ioctl = drmIoctl sysIoctl

   ret <- runEitherT $ do
      -- Open device
      fd <- EitherT $ sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] []

      -- Test for DumbBuffer capability
      _ <- (/= 0) <$> (EitherT $ getCapability ioctl fd CapDumbBuffer)
      liftIO $ putStrLn "The card has DumbBuffer capability :)"

      -- Get resources
      liftIO $ putStrLn "==================\n= RESOURCES\n=================="
      res <- EitherT $ getModeResources ioctl fd
      liftIO $ putStrLn $ show res

      liftIO $ putStrLn "==================\n= CONNECTORS\n=================="

      forM_ (connectors res) $ \connId -> do
         conn <- EitherT $ getConnector ioctl fd connId

         liftIO $ putStrLn $ show conn

      liftIO $ putStrLn "==================\n= ENCODERS \n=================="
      forM_ (encoders res) $ \encId -> do
         enc <- EitherT $ getEncoder ioctl fd encId
         liftIO $ do
            putStrLn $ show enc
            putStrLn $ "Valid CRTCs: " ++ (show $ getEncoderCRTCs res enc)


   case ret of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> return ()