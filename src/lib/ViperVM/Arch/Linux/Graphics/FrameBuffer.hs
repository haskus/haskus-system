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
   )
where

import Data.List (zip4)
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Monad (void)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Format.Binary.Vector as Vector
import ViperVM.Utils.Tuples

-- | Buffer
data Buffer = Buffer
   { bufferHandle    :: Word32 -- ^ Handle of the buffer
   , bufferPitch     :: Word32 -- ^ Pitch of the buffer
   , bufferOffset    :: Word32 -- ^ Offset of the buffer
   , bufferModifiers :: Word64 -- ^ Modifiers for the buffer
   }

-- | Frame buffer
data FrameBuffer = FrameBuffer
   { fbID          :: FrameBufferID                 -- ^ Frame buffer identifier
   , fbWidth       :: Word32                        -- ^ Frame buffer width
   , fbHeight      :: Word32                        -- ^ Frame buffer height
   , fbPixelFormat :: PixelFormat                   -- ^ Pixel format
   , fbFlags       :: FrameBufferFlags              -- ^ Flags
   , fbBuffers     :: (Buffer,Buffer,Buffer,Buffer) -- ^ Data sources (up to four planes)
   }

fromFrameBuffer :: FrameBuffer -> StructFrameBufferCommand
fromFrameBuffer FrameBuffer{..} = s
   where
      FrameBufferID fbid = fbID
      bufs = fromTuple4 fbBuffers
      g f  = Vector.fromFilledList 0 (fmap f bufs)
      s = StructFrameBufferCommand fbid
            fbWidth fbHeight fbPixelFormat fbFlags
            (g bufferHandle) (g bufferPitch)
            (g bufferOffset) (g bufferModifiers)

toFrameBuffer :: StructFrameBufferCommand -> FrameBuffer
toFrameBuffer StructFrameBufferCommand{..} = s
   where
      bufs = take4 $ uncurry4 Buffer <$> zip4
               (Vector.toList fc2Handles)
               (Vector.toList fc2Pitches)
               (Vector.toList fc2Offsets)
               (Vector.toList fc2Modifiers)
      s = FrameBuffer (FrameBufferID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs


-- | Create a framebuffer
addFrameBuffer :: Card -> Word32 -> Word32 -> PixelFormat -> FrameBufferFlags -> [Buffer] -> SysRet FrameBuffer
addFrameBuffer card width height fmt flags buffers = do
   
   let s = FrameBuffer (FrameBufferID 0) width height
               fmt flags (take4 buffers)

   fmap toFrameBuffer <$> ioctlAddFrameBuffer (cardHandle card)
                           (fromFrameBuffer s)

-- | Release a frame buffer
removeFrameBuffer :: Card -> FrameBuffer -> SysRet ()
removeFrameBuffer card fb = do
   let FrameBufferID fbid = fbID fb
   void <$> ioctlRemoveFrameBuffer (cardHandle card) fbid


dirtyFrameBuffer :: Card -> FrameBuffer -> DirtyAnnotation -> SysRet ()
dirtyFrameBuffer card fb mode = do
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
      void <$> ioctlDirtyFrameBuffer (cardHandle card) s


