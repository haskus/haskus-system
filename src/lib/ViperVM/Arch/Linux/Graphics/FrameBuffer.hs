{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.Linux.Graphics.FrameBuffer
   ( Plane(..)
   , FrameBuffer(..)
   )
where

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Word
import Data.List (transpose)
import Control.Applicative ((<$>), (<*>))

import ViperVM.Arch.Linux.Graphics.PixelFormat

data Plane = Plane
   { planeHandle :: Word32
   , planePitch  :: Word32
   , planeOffset :: Word32
   }

data FrameBuffer = FrameBuffer
   { fbID :: Word32
   , fbWidth :: Word32
   , fbHeight :: Word32
   , fbPixelFormat :: PixelFormat
   , fbFlags :: Word32
   , fbPlanes :: (Plane,Plane,Plane,Plane)
   }

instance Storable FrameBuffer where
   sizeOf _    = 5*4 + 8
   alignment _ = 8
   peek ptr    = do
      let 
         p = castPtr ptr :: Ptr Word8
         phandles = castPtr (p `plusPtr` 20) :: Ptr Word32
         ppitches = castPtr (p `plusPtr` 36) :: Ptr Word32
         poffsets = castPtr (p `plusPtr` 52) :: Ptr Word32
      handles <- peekArray 4 phandles
      pitches <- peekArray 4 ppitches
      offsets <- peekArray 4 poffsets

      let 
         ps = transpose [handles,pitches,offsets]
         f [h,pp,o]  = Plane h pp o
         f _         = undefined
         g [a,b,c,d] = (a,b,c,d)
         g _         = undefined
         planes = g (map f ps)
      
      FrameBuffer
         <$> peekByteOff ptr 0
         <*> peekByteOff ptr 4
         <*> peekByteOff ptr 8
         <*> peekByteOff ptr 12
         <*> peekByteOff ptr 16
         <*> return planes

   poke ptr (FrameBuffer {..}) = do
      pokeByteOff ptr 0  fbID
      pokeByteOff ptr 4  fbWidth
      pokeByteOff ptr 8  fbHeight
      pokeByteOff ptr 12 fbPixelFormat
      pokeByteOff ptr 16 fbFlags
      let 
         p = castPtr ptr :: Ptr Word8
         phandles = castPtr (p `plusPtr` 20) :: Ptr Word32
         ppitches = castPtr (p `plusPtr` 36) :: Ptr Word32
         poffsets = castPtr (p `plusPtr` 52) :: Ptr Word32
         g (a,b,c,d) = [a,b,c,d]
         planes = g fbPlanes 
      
      pokeArray phandles (map planeHandle planes)
      pokeArray ppitches (map planePitch  planes)
      pokeArray poffsets (map planeOffset planes)
