{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Frame source
module Haskus.System.Linux.Graphics.FrameSource
   ( -- * Frame source
     FrameSource(..)
   , addFrameSource
   , removeFrameSource
   , dirtyFrameSource
   -- * Pixel source
   , PixelSource(..)
   -- * Flip, Clip, Dirty
   , PageFlipFlag (..)
   , PageFlipFlags
   , DirtyAnnotation (..)
   , Clip (..)
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Internals.Graphics
import Haskus.Format.Binary.Vector as Vector
import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Utils.Tuple
import Haskus.Utils.Flow
import Haskus.Utils.List (zip4)


fromFrameSource :: FrameSource -> StructFrameBufferCommand
fromFrameSource FrameSource{..} = s
   where
      g :: (Num a,Storable a) => (PixelSource -> a) -> Vector 4 a
      g f = Vector.fromFilledList 0 (fmap f frameSources)
      s   = StructFrameBufferCommand (unEntityID frameID)
               frameWidth frameHeight framePixelFormat frameFlags
               (g surfaceHandle) (g surfacePitch)
               (g surfaceOffset) (g surfaceModifiers)

toFrameSource :: StructFrameBufferCommand -> FrameSource
toFrameSource StructFrameBufferCommand{..} = s
   where
      bufs = uncurry4 PixelSource <$> zip4
               (Vector.toList fc2Handles)
               (Vector.toList fc2Pitches)
               (Vector.toList fc2Offsets)
               (Vector.toList fc2Modifiers)
      s = FrameSource (EntityID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs


-- | Create a framebuffer
addFrameSource :: MonadInIO m => Handle -> Word32 -> Word32 -> PixelFormat -> FrameBufferFlags -> [PixelSource] -> Excepts '[ErrorCode] m FrameSource
addFrameSource hdl width height fmt flags buffers = do
   
   let s = FrameSource (EntityID 0) width height
               fmt flags buffers

   ioctlAddFrameBuffer (fromFrameSource s) hdl
      ||> toFrameSource

-- | Release a frame buffer
removeFrameSource :: MonadInIO m => Handle -> FrameSource -> Excepts '[ErrorCode] m ()
removeFrameSource hdl fs = do
   void (ioctlRemoveFrameBuffer (unEntityID (frameID fs)) hdl)


-- | Indicate dirty parts of a frame source
dirtyFrameSource :: MonadInIO m => Handle -> FrameSource -> DirtyAnnotation -> Excepts '[ErrorCode] m ()
dirtyFrameSource hdl fs mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)

   void $ withArray clips $ \clipPtr -> do
      let s = StructFrameBufferDirty
               { fdFbId     = unEntityID (frameID fs)
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      ioctlDirtyFrameBuffer s hdl


