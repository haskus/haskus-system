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

      entities <- getEntities card
                  |> assertLogShowErrorE "Get entities"

      let conns = entitiesConnectors entities

      when (null conns) do
         writeStrLn term "No connector found"

      forM_ conns \conn -> do
         writeStrLn term <| mconcat
            [ "Probing "
            , showObjectQualifiedID conn
            , ": "
            , show (connectorType conn)
            , "-"
            , show (connectorByTypeIndex conn)
            ]

         case connectorState conn of
            Disconnected      -> writeStrLn term " -> disconnected"
            ConnectionUnknown -> writeStrLn term " -> unknown connection"
            Connected videoDisplay -> do
               writeStrLn term "Modes"
               forM_ (videoModes videoDisplay) \mode ->
                  writeStrLn term (showMode mode)

               writeStrLn term "Properties"
               forM_ (videoProperties videoDisplay) \prop ->
                  writeStrLn term ("    " ++ showProperty prop)

   powerOff
