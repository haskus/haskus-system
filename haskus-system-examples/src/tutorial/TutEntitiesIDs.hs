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
      writeStrLn term ("Card " <> show (graphicCardID card))
      mids <- runE (getEntitiesIDs card)
      case mids of
         VLeft  es -> writeStrLn term (" * Error while getting entities ID: " <> show es)
         VRight r  -> do
            let
               showObj :: Object a => a -> Sys ()
               showObj = writeStrLn term . (" - " <>) . showObjectQualifiedID
            forM_ (entitiesConnectorsIDs   r) showObj
            forM_ (entitiesControllersIDs  r) showObj
            forM_ (entitiesPlanesIDs       r) showObj
            forM_ (entitiesFramesIDs       r) showObj

   void powerOff
