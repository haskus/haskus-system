{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      let pixelFormat = makePixelFormat XRGB8888 LittleEndian
      frame <- createGenericFrame card 1024 768 pixelFormat 0
      writeStrLn term (showFrame frame)

      -- fill the generic frame with a color
      -- (0 is the FrameBuffer index)
      forEachGenericFramePixel frame 0 \_x _y ptr ->
         poke ptr (0x316594 :: Word32) -- write a XRGB8888 color

      freeGenericFrame frame

   powerOff
