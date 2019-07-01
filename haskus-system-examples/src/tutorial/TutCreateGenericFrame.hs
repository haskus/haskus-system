{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      let pixelFormat = makePixelFormat XRGB8888 LittleEndian
      frame <- createGenericFrame card 1024 768 pixelFormat 0
      writeStrLn term (showFrame frame)
      freeGenericFrame frame

   powerOff
