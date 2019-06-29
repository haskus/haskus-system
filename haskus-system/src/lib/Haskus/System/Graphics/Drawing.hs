{-# LANGUAGE BangPatterns #-}

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
checkPixelFormat :: Frame -> IO ()
checkPixelFormat fs = do
   let pixFmt = framePixelFormat fs

   case formatFormat pixFmt of
      ARGB8888 -> return ()
      XRGB8888 -> return ()
      _        -> error "Unsupported pixel format"


-- | Fill with a color
fillFrame :: GenericFrame -> Word32 -> IO ()
fillFrame gfb color = do
   let
      fb     = genericFrameBuffer gfb

   checkPixelFormat fb
      
   let
      [buf] = genericFrameBuffers gfb
      addr  = mappedSurfacePointer buf
      pitch = surfacePitch (mappedSurfaceInfo buf)

   forLoop 0 (< fromIntegral (frameHeight fb)) (+1) $ \y ->
      forLoop 0 (< fromIntegral (frameWidth fb)) (+1) $ \x -> do
         let !off = x*4 + y*fromIntegral pitch
         pokeByteOff (castPtr addr) off (color :: Word32)


-- | Display an image
blendImage :: GenericFrame -> Image PixelRGBA8 -> BlendOp -> (Int,Int) -> (Int,Int,Int,Int) -> IO ()
blendImage gfb img op pos clp = do

   let
      fb     = genericFrameBuffer gfb

   checkPixelFormat fb

   let
      [buf] = genericFrameBuffers gfb
      addr  = mappedSurfacePointer buf


   -- compute drawing rect
   let
      (w,h)         = (frameWidth fb, frameHeight fb)
      pitch         = surfacePitch (mappedSurfaceInfo buf)
      (cx,cy,cw,ch) = clp
      clip'         = (cx,cy, min (imageWidth img - cx) cw, min (imageHeight img - cy) ch)
      (px,py)       = pos
      frame         = (0,0,fromIntegral w, fromIntegral h)
      dstRect       = translate clip' pos `intersect` frame
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

      pitch' = fromIntegral pitch

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
