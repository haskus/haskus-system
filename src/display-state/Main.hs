{-# LANGUAGE OverloadedStrings #-}

import Haskus.System
import Haskus.System.Linux.Graphics.State

main :: IO ()
main = runSys <| do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- flowAssertQuiet "Read graphics state"
                  <| readGraphicsState (graphicCardHandle card)

      writeStrLn term (show state)

   void powerOff
