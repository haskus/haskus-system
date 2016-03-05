{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | Frame buffer management
module ViperVM.Arch.Linux.Graphics.FrameBuffer
   ( Plane(..)
   , FrameBuffer(..)
   , FrameBufferFlag (..)
   , FrameBufferFlags
   , addFrameBuffer
   , removeFrameBuffer
   , DirtyMode (..)
   , Clip (..)
   , dirtyFrameBuffer
   )
where

import Data.List (zip4)
import Data.Word
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Control.Monad (void)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Format.Binary.BitSet
import ViperVM.Format.Binary.Vector (Vector)
import ViperVM.Utils.Tuples (uncurry4,take4)

-- | Plane (used for pixel formats that use several sources)
data Plane = Plane
   { planeHandle    :: Word32 -- ^ Handle of the plane
   , planePitch     :: Word32 -- ^ Pitch of the plane
   , planeOffset    :: Word32 -- ^ Offset of the plane
   , planeModifiers :: Word64 -- ^ Modifiers for the plane
   }

-- | Frame buffer
data FrameBuffer = FrameBuffer
   { fbID          :: FrameBufferID             -- ^ Frame buffer identifier
   , fbWidth       :: Word32                    -- ^ Frame buffer width
   , fbHeight      :: Word32                    -- ^ Frame buffer height
   , fbPixelFormat :: PixelFormat               -- ^ Pixel format
   , fbFlags       :: FrameBufferFlags          -- ^ Flags
   , fbPlanes      :: (Plane,Plane,Plane,Plane) -- ^ Data sources (up to four planes)
   }

-- | Frame buffer flags
data FrameBufferFlag
   = FrameBufferInterlaced    -- ^ Interlaced frame buffer
   | FrameBufferUseModifiers  -- ^ Enable modifiers
   deriving (Show,Eq,Enum,EnumBitSet)

type FrameBufferFlags = BitSet Word32 FrameBufferFlag

instance Storable FrameBuffer where
   sizeOf _    = 4 * 17
   alignment _ = 4
   peek ptr    = do
      let 
         p          = castPtr ptr :: Ptr Word8
         phandles   = castPtr (p `plusPtr` 20) :: Ptr Word32
         ppitches   = castPtr (p `plusPtr` 36) :: Ptr Word32
         poffsets   = castPtr (p `plusPtr` 52) :: Ptr Word32
         pmodifiers = castPtr (p `plusPtr` 68) :: Ptr Word64
      handles   <- peekArray 4 phandles
      pitches   <- peekArray 4 ppitches
      offsets   <- peekArray 4 poffsets
      modifiers <- peekArray 4 pmodifiers

      let 
         planes = uncurry4 Plane <$> zip4 handles pitches offsets modifiers
      
      FrameBuffer
         <$> peekByteOff ptr 0
         <*> peekByteOff ptr 4
         <*> peekByteOff ptr 8
         <*> peekByteOff ptr 12
         <*> peekByteOff ptr 16
         <*> return (take4 planes)

   poke ptr FrameBuffer{..} = do
      pokeByteOff ptr 0  fbID
      pokeByteOff ptr 4  fbWidth
      pokeByteOff ptr 8  fbHeight
      pokeByteOff ptr 12 fbPixelFormat
      pokeByteOff ptr 16 fbFlags
      let 
         p           = castPtr ptr :: Ptr Word8
         phandles    = castPtr (p `plusPtr` 20) :: Ptr Word32
         ppitches    = castPtr (p `plusPtr` 36) :: Ptr Word32
         poffsets    = castPtr (p `plusPtr` 52) :: Ptr Word32
         pmodifiers  = castPtr (p `plusPtr` 68) :: Ptr Word64
         g (a,b,c,d) = [a,b,c,d]
         planes      = g fbPlanes
      
      pokeArray phandles   (map planeHandle    planes)
      pokeArray ppitches   (map planePitch     planes)
      pokeArray poffsets   (map planeOffset    planes)
      pokeArray pmodifiers (map planeModifiers planes)

-- | Create a framebuffer
addFrameBuffer :: Card -> Word32 -> Word32 -> PixelFormat -> FrameBufferFlags -> [Plane] -> SysRet FrameBuffer
addFrameBuffer card width height fmt flags planes = do
   
   let
      emptyPlane = Plane 0 0 0 0
      takeFour (a:b:c:d:_) = (a,b,c,d)
      takeFour _           = undefined
      planes' = takeFour (planes ++ repeat emptyPlane)
      fb = FrameBuffer (FrameBufferID 0) width height fmt flags planes'

   ioctlModeAddFrameBuffer (cardHandle card) fb

-- | Release a frame buffer
removeFrameBuffer :: Card -> FrameBuffer -> SysRet ()
removeFrameBuffer card fb =
   with (fbID fb) $ \bufPtr ->
      void <$> ioctlModeRemoveFrameBuffer (cardHandle card) bufPtr


-- | Data matching the C structure drm_mode_fb_cmd2
data FbCmd2Struct = FbCmd2Struct
   { fc2FbId          :: Word32
   , fc2Width         :: Word32
   , fc2Height        :: Word32
   , fc2PixelFormat   :: Word32
   , fc2Flags         :: Word32
   , fc2Handles       :: Vector 4 Word32
   , fc2Pitches       :: Vector 4 Word32
   , fc2Offsets       :: Vector 4 Word32
   , fc2Modifiers     :: Vector 4 Word64
   } deriving (Generic,CStorable)

instance Storable FbCmd2Struct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke




-- | Mark a region of a framebuffer as dirty.
-- 
-- Some hardware does not automatically update display contents as a hardware or
-- software draw to a framebuffer. This ioctl allows userspace to tell the
-- kernel and the hardware what regions of the framebuffer have changed.
-- 
-- The kernel or hardware is free to update more then just the region specified
-- by the clip rects. The kernel or hardware may also delay and/or coalesce
-- several calls to dirty into a single update.
-- 
-- Userspace may annotate the updates, the annotates are a promise made by the
-- caller that the change is either a copy of pixels or a fill of a single color
-- in the region specified.
-- 
-- If the DirtyCopy mode is used then the clip rects are paired as (src,dst).
-- The width and height of each one of the pairs must match.
-- 
-- If the DirtyFill mode is used the caller promises that the region specified
-- of the clip rects is filled completely with a single color as given in the
-- color argument.
data DirtyMode
   = Dirty     [Clip]
   | DirtyCopy [(Clip,Clip)]
   | DirtyFill Word32 [Clip]
   deriving (Show,Eq)

-- | Data matching the C structure drm_mode_fb_dirty_cmd
data FbDirtyStruct = FbDirtyStruct
   { fdFbId          :: Word32
   , fdFlags         :: Word32
   , fdColor         :: Word32
   , fdNumClips      :: Word32
   , fdClipsPtr      :: Word64
   } deriving (Generic,CStorable)

instance Storable FbDirtyStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

data Clip = Clip
   { clipX1 :: Word16
   , clipY1 :: Word16
   , clipX2 :: Word16
   , clipY2 :: Word16
   } deriving (Show,Eq,Generic,CStorable)

instance Storable  Clip where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

dirtyFrameBuffer :: Card -> FrameBuffer -> DirtyMode -> SysRet ()
dirtyFrameBuffer card fb mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)
      FrameBufferID fbid = fbID fb

   withArray clips $ \clipPtr -> do
      let s = FbDirtyStruct
               { fdFbId     = fbid
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      void <$> ioctlModeDirtyFrameBuffer (cardHandle card) s


