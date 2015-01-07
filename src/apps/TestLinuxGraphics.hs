{-# LANGUAGE RecordWildCards #-}
import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.Linux.Graphics.DumbBuffer
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.IDs

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Memory

import Control.Monad.Trans.Either
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Data.Traversable (forM)

import Text.Printf

-- Print connector info
printConnector :: Connector -> String
printConnector (Connector {..}) = 
   printf "%d %s %s %umm x %umm"
      connId
      (show connConnectorType)
      (show connConnection)
      connWidth connHeight
   where
      ConnectorID connId = connConnectorID

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

      liftIO $ putStrLn "==================\n= cardOURCES\n=================="

      card <- EitherT $ getCard ioctl fd
      liftIO $ putStrLn $ show card


      liftIO $ putStrLn "==================\n= CONNECTORS\n=================="

      conns <- forM (cardConnectors card) $ \connId -> do
         conn <- EitherT $ getConnector ioctl fd connId
         liftIO $ putStrLn $ show conn
         liftIO $ putStrLn (printConnector conn)
         return conn


      liftIO $ putStrLn "==================\n= ENCODERS \n=================="

      forM_ (cardEncoders card) $ \encId -> do
         enc <- EitherT $ getEncoder ioctl fd encId
         liftIO $ do
            putStrLn $ show enc
            putStrLn $ "  * Valid controllers: " ++ (show $ getEncoderControllers card enc)
            putStrLn $ "  * Valid connectors: " ++ (show $ getEncoderConnectors card enc)

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
