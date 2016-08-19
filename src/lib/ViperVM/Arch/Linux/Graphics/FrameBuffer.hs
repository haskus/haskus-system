{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | Frame buffer management
module ViperVM.Arch.Linux.Graphics.FrameBuffer
   ( Buffer(..)
   , FrameBuffer(..)
   , addFrameBuffer
   , removeFrameBuffer
   , dirtyFrameBuffer
   , PageFlipFlag (..)
   , PageFlipFlags
   , DirtyAnnotation (..)
   , Clip (..)
   )
where

import Data.List (zip4)
import Foreign.Marshal.Array

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Format.Binary.Vector as Vector
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Tuples
import ViperVM.Utils.Flow

-- | Buffer
data Buffer = Buffer
   { bufferHandle    :: Word32 -- ^ Handle of the buffer
   , bufferPitch     :: Word32 -- ^ Pitch of the buffer
   , bufferOffset    :: Word32 -- ^ Offset of the buffer
   , bufferModifiers :: Word64 -- ^ Modifiers for the buffer
   } deriving (Show)

-- | Frame buffer
data FrameBuffer = FrameBuffer
   { fbID          :: FrameBufferID    -- ^ Frame buffer identifier
   , fbWidth       :: Word32           -- ^ Frame buffer width
   , fbHeight      :: Word32           -- ^ Frame buffer height
   , fbPixelFormat :: PixelFormat      -- ^ Pixel format
   , fbFlags       :: FrameBufferFlags -- ^ Flags
   , fbBuffers     :: [Buffer]         -- ^ Data sources (up to four)
   } deriving (Show)

fromFrameBuffer :: FrameBuffer -> StructFrameBufferCommand
fromFrameBuffer FrameBuffer{..} = s
   where
      FrameBufferID fbid = fbID
      g f  = Vector.fromFilledList 0 (fmap f fbBuffers)
      s = StructFrameBufferCommand fbid
            fbWidth fbHeight fbPixelFormat fbFlags
            (g bufferHandle) (g bufferPitch)
            (g bufferOffset) (g bufferModifiers)

toFrameBuffer :: StructFrameBufferCommand -> FrameBuffer
toFrameBuffer StructFrameBufferCommand{..} = s
   where
      bufs = uncurry4 Buffer <$> zip4
               (Vector.toList fc2Handles)
               (Vector.toList fc2Pitches)
               (Vector.toList fc2Offsets)
               (Vector.toList fc2Modifiers)
      s = FrameBuffer (FrameBufferID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs


-- | Create a framebuffer
addFrameBuffer :: Handle -> Word32 -> Word32 -> PixelFormat -> FrameBufferFlags -> [Buffer] -> SysRet FrameBuffer
addFrameBuffer hdl width height fmt flags buffers = do
   
   let s = FrameBuffer (FrameBufferID 0) width height
               fmt flags buffers

   ioctlAddFrameBuffer (fromFrameBuffer s) hdl >.-.> toFrameBuffer

-- | Release a frame buffer
removeFrameBuffer :: Handle -> FrameBuffer -> SysRet ()
removeFrameBuffer hdl fb = do
   let FrameBufferID fbid = fbID fb
   ioctlRemoveFrameBuffer fbid hdl >.-.> const ()


-- | Indicate dirty parts of a framebuffer
dirtyFrameBuffer :: Handle -> FrameBuffer -> DirtyAnnotation -> SysRet ()
dirtyFrameBuffer hdl fb mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)
      FrameBufferID fbid = fbID fb

   withArray clips $ \clipPtr -> do
      let s = StructFrameBufferDirty
               { fdFbId     = fbid
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      ioctlDirtyFrameBuffer s hdl >.-.> const ()


