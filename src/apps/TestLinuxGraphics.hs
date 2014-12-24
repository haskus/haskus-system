import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.X86_64.Linux.FileSystem

import Control.Monad.Trans.Either
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Data.Traversable (forM)

main :: IO ()
main = do
   let ioctl = drmIoctl sysIoctl

   ret <- runEitherT $ do
      -- Open device
      fd <- EitherT $ sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] []

      -- Test for DumbBuffer capability
      _ <- (/= 0) <$> (EitherT $ getCapability ioctl fd CapDumbBuffer)
      liftIO $ putStrLn "The card has DumbBuffer capability :)"

      liftIO $ putStrLn "==================\n= RESOURCES\n=================="

      res <- EitherT $ getModeResources ioctl fd
      liftIO $ putStrLn $ show res


      liftIO $ putStrLn "==================\n= CONNECTORS\n=================="

      conns <- forM (connectors res) $ \connId -> do
         conn <- EitherT $ getConnector ioctl fd connId
         liftIO $ putStrLn $ show conn
         return conn


      liftIO $ putStrLn "==================\n= ENCODERS \n=================="

      forM_ (encoders res) $ \encId -> do
         enc <- EitherT $ getEncoder ioctl fd encId
         liftIO $ do
            putStrLn $ show enc
            putStrLn $ "Valid CRTCs: " ++ (show $ getEncoderCRTCs res enc)

      liftIO $ putStrLn "==================\n= Test \n=================="
      
      -- Find connected connectors
      let
         isValid x  = connConnection x == Connected
                      && not (null $ connModes x)
         validConns = filter isValid conns

         -- select first connector
         conn = head validConns

         -- select highest mode
         mode = head (connModes conn)

      -- check if the connector already has an associated encoder+crtc to avoid modesetting
      (curCrtc,curEnc) <- EitherT $ getConnectorCRTC ioctl fd conn

      liftIO $ putStrLn $ "Current CRTC and encoder: " ++ show (curCrtc,curEnc)

         -- find a crtc
         -- TODO

         -- create a framebuffer
         -- TODO


   return ()
