{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

import Haskus.System

import Haskus.System.Linux.Graphics.State
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
            Disconnected          -> writeStrLn term " -> disconnected"
            ConnectionUnknown     -> writeStrLn term " -> unknown connection"
            Connected Display{..} -> do
               writeStrLn term <| mconcat
                  [ "Physical size: "
                  , show displayPhysicalWidth
                  , "mm X "
                  , show displayPhysicalHeight
                  , " mm"
                  ]
               writeStrLn term ("Sub-pixel layout: " <> show displaySubPixel)
               writeStrLn term "Modes"
               forM_ displayModes \mode ->
                  writeStrLn term (showMode mode)

               writeStrLn term "Properties"
               forM_ displayProperties \prop ->
                  writeStrLn term ("    " ++ showRawProperty card prop)

   powerOff
