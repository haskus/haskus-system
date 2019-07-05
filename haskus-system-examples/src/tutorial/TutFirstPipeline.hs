{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.State
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
      entities <- getEntities card
                     |> assertE "Can't get entities"

      forEachConnectedDisplay card \conn display -> do
         let
            mode        = head (displayModes display)
            pixelFormat = makePixelFormat XRGB8888 LittleEndian
            showProps o = do
               -- get properties of object o
               mprops <- runE (getObjectProperties card o)
               -- show them
               forM_ mprops \props -> do
                  writeStrLn term ("* " ++ showObjectQualifiedID o)
                  forM_ props \prop ->
                     writeStrLn term ("    " ++ showProperty prop)

            showAllProps = do
               entities' <- getEntities card
                              |> assertLogShowErrorE "Get entities"

               mapM_ showProps (entitiesConnectors entities')
               mapM_ showProps (entitiesControllers entities')
               mapM_ showProps (entitiesPlanes entities')
               mapM_ showProps (entitiesFrames entities')

         showAllProps
         -- create a frame
         frame <- createGenericFullScreenFrame card mode pixelFormat 0

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

         showAllProps

         -- build the configuration
         assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlob -> do
            let cmds =
                  [ CmdConnectorController (connectorID conn) ctrlID
                  , CmdControllerMode      ctrlID modeBlob
                  , CmdControllerActive    ctrlID True
                  , CmdPlaneSource         plnID (frameID frame) 0 0 (frameWidth frame) (frameHeight frame)
                  , CmdPlaneTarget         plnID ctrlID 0 0 (frameWidth frame) (frameHeight frame)
                  ]
            configureGraphics card Commit Synchronous AllowFullModeset cmds
         showAllProps


   waitForKey term
   powerOff
