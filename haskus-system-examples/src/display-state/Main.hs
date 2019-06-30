{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Haskus.System.Linux.Graphics.State

main :: IO ()
main = runSys do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      state <- getHandleEntities (graphicCardHandle card)
                  |> assertE "Get entities"

      writeStrLn term (show state)

   void powerOff
