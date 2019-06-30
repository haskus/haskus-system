{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.Mode

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do

      state <- getHandleEntities (graphicCardHandle card)
                  |> assertLogShowErrorE "Get entities"

      let conns = entitiesConnectors state

      when (null conns) do
         writeStrLn term "No connector found"

      forM_ conns \conn -> do
         writeStrLn term ("Probing " ++ showObjectQualifiedID conn)

         case connectorState conn of
            Disconnected      -> writeStrLn term " -> disconnected"
            ConnectionUnknown -> writeStrLn term " -> unknown connection"
            Connected dev -> do
               writeStrLn term "Modes"
               forM_ (videoModes dev) \mode ->
                  writeStrLn term (showMode mode)

               writeStrLn term "Properties"
               forM_ (videoProperties dev) \prop ->
                  writeStrLn term ("    " ++ showProperty prop)

   powerOff
