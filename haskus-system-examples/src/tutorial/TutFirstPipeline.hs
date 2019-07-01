{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   _term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      forEachConnectedDisplay card \_connector display -> do
         let
            mode        = head (displayModes display)
            pixelFormat = makePixelFormat XRGB8888 LittleEndian
         _frame <- createGenericFullScreenFrame card mode pixelFormat 0
         return ()

   powerOff
