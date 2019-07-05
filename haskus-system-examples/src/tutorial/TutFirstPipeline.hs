{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Graphics.Config

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   -- enable DRM debug
   writeSysTextAttribute sys "module/drm/parameters/debug" "0xf"
   writeProcTextAttribute sys "sys/kernel/printk" "7"
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      forEachConnectedDisplay card \conn display -> do
         let
            mode        = head (displayModes display)
            pixelFormat = makePixelFormat XRGB8888 LittleEndian

         dumpAllEntities card
         -- create a frame
         frame <- createGenericFullScreenFrame card mode pixelFormat 0

         entities <- getEntities card
                        |> assertE "Can't get entities"

         -- get a primary plane
         let planes = entitiesPlanes entities
         let plane = head planes
         let plnID = planeID plane

         -- select a controller
         let ctrls = planePossibleControllers plane
         let ctrlID  = head ctrls

         writeStrLn term <| mconcat
            [ "trying to use:"
            , "\n - ", showObjectQualifiedID plnID
            , "\n - ", showObjectQualifiedID ctrlID
            , "\n - ", showObjectQualifiedID (connectorID conn)
            , "\n - ", showObjectQualifiedID (frameID frame)
            ]

         dumpAllEntities card

         -- build the configuration
         assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlob -> do
            let cmds =
                  [ CmdConnectorController (connectorID conn) ctrlID
                  , CmdControllerMode      ctrlID modeBlob
                  , CmdControllerActive    ctrlID True
                  , CmdPlaneSource         plnID (frameID frame) 0 0 (fromIntegral (frameWidth frame)) (fromIntegral (frameHeight frame))
                  , CmdPlaneTarget         plnID ctrlID 0 0 (frameWidth frame) (frameHeight frame)
                  ]
            configureGraphics card Commit Synchronous AllowFullModeset cmds
         dumpAllEntities card


   waitForKey term
   powerOff
