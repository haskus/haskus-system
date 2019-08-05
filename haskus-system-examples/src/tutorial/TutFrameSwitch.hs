{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Graphics.Config
import Haskus.Binary.Storable
import Haskus.Number.Word

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      onEvent (graphicCardChan card) \ev -> do
         writeStrLn term (show ev)

      forEachConnectedDisplay card \conn display -> do
         -- get a primary plane
         plane <- getEntities card
                  |> assertE "Can't get entities"
                  ||> entitiesPlanes
                  ||> filter (\p -> planeType p == Primary)
                  ||> head

         -- test that the plane supports the pixel format we want to use
         let pixelFormat = makePixelFormat XRGB8888 LittleEndian
         unless (pixelFormat `elem` planeFormats plane) do
            writeStrLn term "Pixel format not supported!"

         -- create and fill a fullscreen frame for the preferred mode (first in
         -- the list)
         let mode = head (displayModes display)
         frame <- createGenericFullScreenFrame card mode pixelFormat 0
         let render col = do
               forEachGenericFramePixel frame 0 \_x _y ptr -> do
                  poke ptr (col :: Word32)
               render (col + 0x10)
         sysFork "Test" (render 0)
            

         -- select a controller
         let ctrlID = planePossibleControllers plane
                      |> head

         -- build the pipeline
         assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlobID ->
            configureGraphics card Commit EnableVSync EnableFullModeset do
               setConnectorSource conn ctrlID -- connector  <-> controller
               setPlaneTarget plane ctrlID    -- controller <-> plane
               setPlaneSource plane frame     -- plane      <-> frame
               -- sizes and modes
               setPlaneSize plane (frameWidth frame) (frameHeight frame)
               setPlaneSourceSize plane (frameWidth frame) (frameHeight frame)
               setMode ctrlID modeBlobID
               -- enable the controller
               enableController ctrlID True

   waitForKey term
   powerOff
