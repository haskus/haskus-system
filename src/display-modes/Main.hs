{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Helper
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.PixelFormat
import qualified Haskus.Utils.Map as Map

main :: IO ()
main = runSys' <| do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- flowAssertQuiet "Read graphics state"
                  <| readGraphicsState (graphicCardHandle card)

      forM_ (graphicsConnectors state) <| \conn -> do
         
         let ctrl = do
               encId  <- connectorEncoderID conn
               enc    <- Map.lookup encId (graphicsEncoders state)
               ctrlId <- encoderControllerID enc
               Map.lookup ctrlId (graphicsControllers state)

         case (ctrl, connectorState conn) of
            (Just c, Connected dev) -> do
               let
                  modes  = connectedDeviceModes dev 
                  fmt    = makePixelFormat XRGB8888 LittleEndian

               forM_ modes <| \mode -> do
                  threadDelaySec 4
                  writeStrLn term (modeName mode)

                  gfb <- initGenericFrameBuffer card mode fmt

                  setController c (SetSource (genericFrameBuffer gfb)) [conn] (Just mode)
                     |> flowAssert "Set controller"

                  freeGenericFrameBuffer card gfb
            _ -> return ()


   powerOff
