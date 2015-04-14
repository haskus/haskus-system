{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Frame buffer management
module ViperVM.Arch.Linux.Graphics.FrameBuffer
   ( Plane(..)
   , FrameBuffer(..)
   , cardAddFrameBuffer
   , cardRemoveFrameBuffer
   )
where

import Data.List (transpose)
import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)
import Data.Word
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Utils.EnumSet

type Vec4 = Vec (S (S (S (S Z))))

-- | Plane (used for pixel formats that use several sources)
data Plane = Plane
   { planeHandle :: Word32       -- ^ Handle of the plane
   , planePitch  :: Word32       -- ^ Pitch of the plane
   , planeOffset :: Word32       -- ^ Offset of the plane
   }

-- | Frame buffer
data FrameBuffer = FrameBuffer
   { fbID            :: Word32                      -- ^ Frame buffer identifier
   , fbWidth         :: Word32                      -- ^ Frame buffer width
   , fbHeight        :: Word32                      -- ^ Frame buffer height
   , fbPixelFormat   :: PixelFormat                 -- ^ Pixel format
   , fbFlags         :: Word32                      -- ^ Flags
   , fbPlanes        :: (Plane,Plane,Plane,Plane)   -- ^ Data sources (up to four planes)
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
         toPlane [h,pp,o]   = Plane h pp o
         toPlane _          = undefined
         takeFour [a,b,c,d] = (a,b,c,d)
         takeFour _         = undefined
         planes = takeFour (map toPlane ps)
      
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

-- | Create a framebuffer
--
-- We use DRM_IOCTL_MODE_ADDFB2 as it provides more control on pixel format
addFrameBuffer :: IOCTL -> FileDescriptor -> Word32 -> Word32 -> PixelFormat -> Word32 -> [Plane] -> SysRet FrameBuffer
addFrameBuffer ioctl fd width height fmt flags planes = do
   
   let
      emptyPlane = Plane 0 0 0
      takeFour (a:b:c:d:_) = (a,b,c,d)
      takeFour _           = undefined
      planes' = takeFour (planes ++ repeat emptyPlane)
      fb = FrameBuffer 0 width height fmt flags planes'

   ioctlReadWrite ioctl 0x64 0xB8 defaultCheck fd fb

-- | Release a frame buffer
removeFrameBuffer :: IOCTL -> FileDescriptor -> FrameBuffer -> SysRet ()
removeFrameBuffer ioctl fd fb = do
   with (fbID fb) $ \bufPtr ->
      fmap (const ()) <$> ioctlReadWrite ioctl 0x64 0xAF defaultCheck fd bufPtr

data FrameBufferMode
   = FrameBufferInterlaced
   deriving (Show,Eq,Enum)

instance EnumBitSet FrameBufferMode


-- | Data matching the C structure drm_mode_fb_cmd2
data FbCmd2Struct = FbCmd2Struct
   { fc2FbId          :: Word32
   , fc2Width         :: Word32
   , fc2Height        :: Word32
   , fc2PixelFormat   :: Word32
   , fc2Flags         :: Word32
   , fc2Handles       :: StorableWrap (Vec4 Word32)
   , fc2Pitches       :: StorableWrap (Vec4 Word32)
   , fc2Offsets       :: StorableWrap (Vec4 Word32)
   } deriving Generic

instance CStorable FbCmd2Struct
instance Storable FbCmd2Struct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


data FrameBufferDirty
   = FrameBufferDirtyNone
   | FrameBufferDirtyAnnotateCopy
   | FrameBufferDirtyAnnotateFill
   | FrameBufferDirtyFlags
   deriving (Show,Eq,Enum)

-- | Data matching the C structure drm_mode_fb_dirty_cmd
data FbDirtyStruct = FbDirtyStruct
   { fdFbId          :: Word32
   , fdFlags         :: Word32
   , fdColor         :: Word32
   , fdNumClips      :: Word32
   , fdClipsPtr      :: Word64
   } deriving Generic

instance CStorable FbDirtyStruct
instance Storable FbDirtyStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Create a framebuffer
cardAddFrameBuffer :: Card -> Word32 -> Word32 -> PixelFormat -> Word32 -> [Plane] -> SysRet FrameBuffer
cardAddFrameBuffer card width height fmt flags planes =
   withCard card addFrameBuffer width height fmt flags planes

-- | Release a frame buffer
cardRemoveFrameBuffer :: Card -> FrameBuffer -> SysRet ()
cardRemoveFrameBuffer card fb = withCard card removeFrameBuffer fb
