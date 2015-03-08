{-# LANGUAGE RecordWildCards #-}
import ViperVM.Arch.Linux.Graphics.Graphics
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Memory
import ViperVM.Arch.Linux.ErrorCode

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)

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

      card <- EitherT $ getCard ioctl fd

      -- Test for GenericBuffer capability
      cap <- EitherT $ cardHasSupportFor ioctl card CapGenericBuffer
      hoistEither $ if cap
         then Left ENOENT 
         else Right ()

      liftIO $ putStrLn "The card has GenericBuffer capability :)"

      liftIO $ putStrLn "==================\n= CARD\n=================="

      liftIO $ putStrLn $ show card


      liftIO $ putStrLn "==================\n= CONNECTORS\n=================="

      conns <- liftIO $ cardConnectors ioctl card
      forM_ conns $ \conn -> do
         liftIO $ putStrLn $ show conn
         liftIO $ putStrLn (printConnector conn)


      liftIO $ putStrLn "==================\n= ENCODERS \n=================="

      forM_ (cardEncoderIDs card) $ \encId -> do
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

      -- create a generic buffer
      db <- EitherT $ createGenericBuffer ioctl fd width height bpp dbFlags

      -- create a framebuffer for the generic buffer
      let 
         plane = Plane (genericBufferHandle db) (genericBufferPitch db) 0
         fmt   = PixelFormat RGBX8888 LittleEndian
         fbFlgs = 0
      fb <- EitherT $ addFrameBuffer ioctl fd width height fmt fbFlgs [plane]

      -- prepare buffer for memory mapping
      dbmap <- EitherT $ mapGenericBuffer ioctl fd db

	   -- perform actual memory mapping
      let size = genericBufferSize db
      mem <- EitherT $ mmap Nothing size [ProtRead,ProtWrite] [MapShared] (Just (fd, genericMapOffset dbmap))

      return ()

   return ()
