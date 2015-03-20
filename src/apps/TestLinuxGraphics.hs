{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Capability

import ViperVM.Arch.Linux.System.Graphics
import ViperVM.Arch.Linux.System.SysFS

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Memory
import ViperVM.Arch.Linux.ErrorCode

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Control.Applicative ((<$>))

import Text.Printf

-- Print connector info
printConnector :: Connector -> String
printConnector (Connector {..}) = 
   printf "%d %s %s %umm x %umm"
      connId
      (show connectorType)
      (show connectorState)
      connectorWidth connectorHeight
   where
      ConnectorID connId = connectorID

main :: IO ()
main = do
   let 
      ioctl = drmIoctl sysIoctl
      mmap  = sysMemMap
      try str a = EitherT (a >>= \case
         Left err -> return (Left (str,err))
         Right v  -> return (Right v))

   ret <- runEitherT $ do
      sysfs <- try "Load SysFS" $ fmap SysFS <$> sysOpen "/sys" [OpenReadOnly] []

      cards <- try "Load graphic cards" $ loadGraphicCards sysfs

      liftIO $ do
         putStrLn "Graphic cards:"
         print cards

      -- Open device
      fd <- try "Open card" $ sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] []

      card <- try "Read card info" $ getCard ioctl fd

      -- Test for GenericBuffer capability
      cap <- try "Check support for generic buffers" $ cardHasSupportFor card CapGenericBuffer
      if cap
         then left ("No support for generic buffers", ENOENT)
         else right ()

      liftIO $ putStrLn "The card has GenericBuffer capability :)"

      liftIO $ putStrLn "==================\n= CARD\n=================="

      liftIO $ putStrLn $ show card


      liftIO $ putStrLn "==================\n= CONNECTORS\n=================="

      conns <- liftIO $ cardConnectors card
      forM_ conns $ \conn -> do
         liftIO $ putStrLn $ show conn
         liftIO $ putStrLn (printConnector conn)


      liftIO $ putStrLn "==================\n= ENCODERS \n=================="

      forM_ (cardEncoderIDs card) $ \encId -> do
         enc <- try "Get encoder" $ cardEncoderFromID card encId
         liftIO $ do
            putStrLn $ show enc
            putStrLn $ "  * Valid controllers: " ++ (show $ encoderPossibleControllers enc)
            putStrLn $ "  * Valid connectors: " ++ (show $ encoderPossibleConnectors enc)

      liftIO $ putStrLn "==================\n= Test \n=================="
      
      -- Find connected connectors
      let
         isValid x  = connectorState x == Connected
                      && not (null $ connectorModes x)
         validConns = filter isValid conns

         -- select first connector
         conn = head validConns

         -- select highest mode
         mode = head (connectorModes conn)

      -- check if the connector already has an associated encoder+crtc to avoid modesetting
      (curCrtc,curEnc) <- try "Get current state" (connectorController conn)

      liftIO $ putStrLn $ "Current Controller and encoder: " ++ show (curCrtc,curEnc)

      -- find a crtc, attach encoder and set mode
      -- TODO
      
      let
         width  = fromIntegral $ modeHorizontalDisplay mode
         height = fromIntegral $ modeVerticalDisplay mode
         bpp    = 32
         dbFlags = 0

      -- create a generic buffer
      db <- try "Create a generic buffer" $ cardCreateGenericBuffer card width height bpp dbFlags

      -- create a framebuffer for the generic buffer
      let 
         plane = Plane (genericBufferHandle db) (genericBufferPitch db) 0
         fmt   = PixelFormat RGBX8888 LittleEndian
         fbFlgs = 0
      fb <- try "Add a frame buffer" $ cardAddFrameBuffer card width height fmt fbFlgs [plane]

      -- prepare buffer for memory mapping
      dbmap <- try "Prepare for mapping" $ cardMapGenericBuffer card db

	   -- perform actual memory mapping
      let size = genericBufferSize db
      mem <- try "Map generic buffer" $ mmap Nothing size [ProtRead,ProtWrite] [MapShared] (Just (fd, genericMapOffset dbmap))

      return ()

   case ret of
      Left (str,err) -> error $ "Error: " ++ str ++ " (" ++ show err ++ ")"
      Right _ -> putStrLn "Done"
