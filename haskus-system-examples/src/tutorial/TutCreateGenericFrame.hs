{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word
import Foreign.Ptr

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      let pixelFormat = makePixelFormat XRGB8888 LittleEndian
      frame <- createGenericFrame card 1024 768 pixelFormat 0
      writeStrLn term (showFrame frame)

      let
         -- we know that XRGB8888 pixel format uses a single (frame)buffers
         fb    = head (frameBuffers frame)
         color = 0x316594

      -- fill the frame with a color
      withGenericFrameBufferPtr fb \addr ->
         forEachFramePixel frame \x y -> do
            let off = frameBufferPixelOffset fb 4 x y -- 4 is pixel component size in bytes
            pokeByteOff (castPtr addr) (fromIntegral off) (color :: Word32)

      freeGenericFrame frame

   powerOff
