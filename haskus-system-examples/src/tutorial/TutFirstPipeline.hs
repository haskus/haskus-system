{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Graphics.Config

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

         entities <- getEntities card
                        |> assertE "Can't get entities"

         -- get a primary plane
         let planes = entitiesPlanes entities
         let plane = head planes
         let plnID = planeID plane
         let frmID = frameID frame

         -- select a controller
         let ctrls = planePossibleControllers plane
         let ctrlID  = head ctrls

         -- build the configuration
         assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlobID ->
            configureGraphics card Commit Synchronous AllowFullModeset
               [ CmdPlaneTarget         plnID ctrlID 0 0 (frameWidth frame) (frameHeight frame)
               , CmdPlaneSource         plnID frmID 0 0 (fromIntegral (frameWidth frame)) (fromIntegral (frameHeight frame))
               , CmdConnectorController (connectorID conn) ctrlID
               , CmdControllerMode      ctrlID modeBlobID
               , CmdControllerActive    ctrlID True
               ]

   waitForKey term
   powerOff
