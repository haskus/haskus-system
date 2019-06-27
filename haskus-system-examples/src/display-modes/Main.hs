{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Object

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      state <- assertE "Read graphics state"
                  <| readGraphicsState (graphicCardHandle card)

      let conns = graphicsConnectors state
      when (null conns) do
         writeStrLn term "No connector found"

      forM_ conns \conn -> do
         writeStrLn term ("Probing " ++ getObjectQualifiedID conn)

         case connectorState conn of
            Disconnected      -> writeStrLn term " -> disconnected"
            ConnectionUnknown -> writeStrLn term " -> unknown connection"
            Connected dev -> do
               forM_ (connectedDeviceModes dev) \mode ->
                  writeStrLn term (show mode)

   powerOff
