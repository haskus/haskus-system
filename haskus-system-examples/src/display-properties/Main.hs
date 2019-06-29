{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.AtomicConfig
import qualified Haskus.Utils.Map as Map


main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal

   -- get graphic card devices
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
                  |> assertLogShowErrorE "Read graphics state"

      let
         showProps o = do
            mprops <- graphicsConfig (graphicCardHandle card) do
                        getPropertyM o
                           |> runE
            forM_ mprops \props -> do
               writeStrLn term ("* " ++ showObjectQualifiedID o)
               forM_ props \prop ->
                  writeStrLn term ("    " ++ showProperty prop)

      mapM_ showProps (Map.elems (graphicsConnectors state))
      mapM_ showProps (Map.elems (graphicsControllers state))
      mapM_ showProps (Map.elems (graphicsPlanes state))

   powerOff
