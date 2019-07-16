{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Graphics.Config
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      forEachConnectedDisplay card \conn display -> do
         let
            mode        = head (displayModes display)
            pixelFormat = makePixelFormat XRGB8888 LittleEndian

         frame <- createGenericFullScreenFrame card mode pixelFormat 0
          -- fill the frame
         forEachGenericFramePixel frame 0 \x y ptr -> do
            let col = 0x310000 + (x .&. 0xFF) `shiftL` 8 + y .&. 0xFF
            poke ptr (col :: Word32)

         entities <- getEntities card
                        |> assertE "Can't get entities"

         -- get a primary plane
         let planes = entitiesPlanes entities
         let plane = head planes

         -- select a controller
         let ctrls = planePossibleControllers plane
         let ctrlID  = head ctrls

         -- build the configuration
         assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlobID ->
            configureGraphics card Commit Synchronous AllowFullModeset do
               setPlaneTarget plane ctrlID
               setPlaneSize plane (frameWidth frame) (frameHeight frame)
               setPlaneSource plane frame
               setPlaneSourceSize plane (fromIntegral (frameWidth frame)) (fromIntegral (frameHeight frame))
               setConnectorSource conn ctrlID
               setMode ctrlID modeBlobID
               enableController ctrlID True

   waitForKey term
   powerOff
