{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | Drawing methods for frame buffers
-- 
-- Use and re-export parts of JuicyPixels and Rasterific.
module Haskus.System.Graphics.Drawing
   ( BlendOp (..)
   , blendImage
   , loadPng
   , fillFrame
   -- re-export
   , module Graphics.Rasterific
   , module Graphics.Rasterific.Texture
   , PixelRGBA8 (..)
   )
where

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture.Png
import Codec.Picture.Types

import Control.Loop (forLoop)

import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Foreign.Ptr
import Haskus.Format.Binary.Buffer as B
import Haskus.System.Graphics
import Haskus.System.Linux.Graphics.Frame
import Haskus.System.Linux.Graphics.PixelFormat

-- | Blanding method
data BlendOp
   = BlendCopy
   | BlendAlpha
   deriving (Show,Eq)

translate :: (Int,Int,Int,Int) -> (Int,Int) -> (Int,Int,Int,Int)
translate (x,y,w,h) (px,py) = (x+px,y+py,w,h)

intersect :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
intersect (x1,y1,w1,h1) (x2,y2,w2,h2)
   | x1 >= x2+w2 = (0,0,0,0)
   | x2 >= x1+w1 = (0,0,0,0)
   | y1 >= y2+h2 = (0,0,0,0)
   | y2 >= y1+h1 = (0,0,0,0)
   | otherwise   = (x', y', min (x1+w1) (x2+w2) - x', min (y1+h1) (y2+h2) - y')
         where
            x' = max x1 x2
            y' = max y1 y2

-- | Load a PNG
loadPng :: B.Buffer -> Image PixelRGBA8
loadPng bs = img
   where
      Right (ImageRGBA8 img) = decodePng (bufferUnpackByteString bs)


-- | check framebuffer pixel format
checkPixelFormat :: Frame b -> IO (FrameBuffer b)
checkPixelFormat frame = do
   case formatFormat (framePixelFormat frame) of
      ARGB8888 -> return (head (frameBuffers frame))
      XRGB8888 -> return (head (frameBuffers frame))
      _        -> error "Unsupported pixel format"


-- | Fill with a color
fillFrame :: Frame GenericBuffer -> Word32 -> IO ()
fillFrame frame color = do
   _fb <- checkPixelFormat frame
      
   forEachGenericFramePixel frame 0 \_x _y ptr ->
      poke ptr color


-- | Display an image
blendImage :: Frame GenericBuffer -> Image PixelRGBA8 -> BlendOp -> (Int,Int) -> (Int,Int,Int,Int) -> IO ()
blendImage frame img op pos clp = do
   fb <- checkPixelFormat frame

   -- compute drawing rect
   let
      (w,h)         = (frameWidth frame, frameHeight frame)
      pitch'        = fromIntegral (fbPitch fb)
      (cx,cy,cw,ch) = clp
      clip'         = (cx,cy, min (imageWidth img - cx) cw, min (imageHeight img - cy) ch)
      (px,py)       = pos
      frameRect     = (0,0,fromIntegral w, fromIntegral h)
      dstRect       = translate clip' pos `intersect` frameRect
      srcRect       = translate dstRect (-1 * px, -1 * py)

      (sx,sy,sw,sh) = srcRect
      (dx,dy,_,_)   = dstRect

      -- Convert betweeen RGBA8 and XRGB8 (endianness in DRM is misleading)
      {-# INLINE myPackPixel #-}
      myPackPixel (PixelRGBA8 !r !g !b !a) =
          (fi r `uncheckedShiftL` (2 * bitCount)) .|.
          (fi g `uncheckedShiftL` (1 * bitCount)) .|.
          (fi b `uncheckedShiftL` (0 * bitCount)) .|.
          (fi a `uncheckedShiftL` (3 * bitCount))
        where fi = fromIntegral
              bitCount = 8

      {-# INLINE myUnpackPixel #-}
      myUnpackPixel !v = PixelRGBA8
          (low $ (v :: Word32) `uncheckedShiftR` (2 * bitCount))
          (low $ v `uncheckedShiftR` bitCount)
          (low v)
          (low $ v `uncheckedShiftR` (3 * bitCount))
         where
           low = fromIntegral . (.&. 0xFF)
           bitCount = 8

   withGenericFrameBufferPtr fb \addr ->
      case op of
         BlendAlpha -> 
            forLoop 0 (< sh) (+1) $! \y ->
               forLoop 0 (< sw) (+1) $! \x -> do
                  -- dest offset
                  let !doff = (dx+x)*4 + (dy+y)*pitch'
                  -- old value
                  !old <- myUnpackPixel <$> peekByteOff (castPtr addr) doff
                  let
                     !new  = pixelAt img (sx+x) (y+sy)
                     !opa  = fromIntegral $ pixelOpacity new
                     -- clip to 255
                     bl _ !s !d = if (z `uncheckedShiftR` 8) /= 0
                           then 255
                           else fromIntegral (z .&. 0xff)
                        where
                           !z = ((fromIntegral s :: Word32) * opa + (fromIntegral d :: Word32) * (255-opa)) `uncheckedShiftR` 8
                     !v = myPackPixel (mixWith bl new old)
                  pokeByteOff (castPtr addr) doff (v :: Word32)

         BlendCopy ->
            forLoop 0 (< sh) (+1) $ \y ->
               forLoop 0 (< sw) (+1) $ \x -> do
                  let
                     !doff = (dx+x)*4 + (dy+y)*pitch'
                     !new  = pixelAt img (sx+x) (y+sy)
                     !v = myPackPixel new
                  pokeByteOff (castPtr addr) doff (v :: Word32)
