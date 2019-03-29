{-# LANGUAGE OverloadedStrings #-}

import Haskus.System
import Haskus.System.Linux.Graphics.State

main :: IO ()
main = runSys <| do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
                  |> assertE "Read graphics state"

      writeStrLn term (show state)

   void powerOff
