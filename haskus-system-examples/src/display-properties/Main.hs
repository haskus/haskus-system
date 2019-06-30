{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.AtomicConfig


main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal

   -- get graphic card devices
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- getHandleEntities (graphicCardHandle card)
                  |> assertLogShowErrorE "Get entities"

      let
         showProps o = do
            mprops <- graphicsConfig (graphicCardHandle card) do
                        getPropertyM o
                           |> runE
            forM_ mprops \props -> do
               writeStrLn term ("* " ++ showObjectQualifiedID o)
               forM_ props \prop ->
                  writeStrLn term ("    " ++ showProperty prop)

      mapM_ showProps (entitiesConnectors state)
      mapM_ showProps (entitiesControllers state)
      mapM_ showProps (entitiesPlanes state)
      mapM_ showProps (entitiesFrames state)

   powerOff
