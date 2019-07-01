{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      forEachConnectedDisplay card \_conn display -> do
         let mode        = head (displayModes display)
             pixelFormat = makePixelFormat XRGB8888 LittleEndian


         writeStrLn term "Selected mode:"
         writeStrLn term (showMode mode)

         writeStrLn term ("Selected format: " ++ show pixelFormat)

         frame <- createGenericFrame card mode pixelFormat
         writeStrLn term (showFrame frame)


   powerOff
