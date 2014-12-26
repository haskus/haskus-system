import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.Linux.Graphics.DumbBuffer
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Encoder

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Memory

import Control.Monad.Trans.Either
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Data.Traversable (forM)

main :: IO ()
main = do
   let 
      ioctl = drmIoctl sysIoctl
      mmap  = sysMemMap

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
            putStrLn $ "Valid Controllers: " ++ (show $ getEncoderControllers res enc)

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
      (curCrtc,curEnc) <- EitherT $ getConnectorController ioctl fd conn

      liftIO $ putStrLn $ "Current Controller and encoder: " ++ show (curCrtc,curEnc)

      -- find a crtc, attach encoder and set mode
      -- TODO
      
      let
         width  = fromIntegral $ modeHorizontalDisplay mode
         height = fromIntegral $ modeVerticalDisplay mode
         bpp    = 32
         dbFlags = 0

      -- create a dumb buffer
      db <- EitherT $ createDumbBuffer ioctl fd width height bpp dbFlags

      -- create a framebuffer for the dumb buffer
      let 
         plane = Plane (dumbBufferHandle db) (dumbBufferPitch db) 0
         fmt   = PixelFormat RGBX8888 LittleEndian
         fbFlgs = 0
      fb <- EitherT $ addFrameBuffer ioctl fd width height fmt fbFlgs [plane]

      -- prepare buffer for memory mapping
      dbmap <- EitherT $ mapDumbBuffer ioctl fd db

	   -- perform actual memory mapping
      let size = dumbBufferSize db
      mem <- EitherT $ mmap Nothing size [ProtRead,ProtWrite] [MapShared] (Just (fd, dumbMapOffset dbmap))

      return ()

   return ()
