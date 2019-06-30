{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Frame
--
-- A frame is a picture in memory. Its pixel components may be scattered into
-- different frame buffers though.
module Haskus.System.Linux.Graphics.Frame
   ( Frame(..)
   , FrameBuffer (..)
   , createFrame
   , freeFrame
   , dirtyFrame
   , SwitchFrameFlag (..)
   , SwitchFrameFlags
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


fromFrame :: Frame -> StructFrameCommand
fromFrame Frame{..} = s
   where
      g :: (Num a,Storable a) => (FrameBuffer -> a) -> Vector 4 a
      g f = Vector.fromFilledList 0 (fmap f frameBuffers)
      s   = StructFrameCommand (unEntityID frameID)
               frameWidth frameHeight framePixelFormat frameFlags
               (g fbBuffer) (g fbPitch)
               (g fbOffset) (g fbModifiers)

toFrame :: StructFrameCommand -> Frame
toFrame StructFrameCommand{..} = s
   where
      bufs = uncurry4 FrameBuffer <$> zip4
               (Vector.toList fc2Handles)
               (Vector.toList fc2Pitches)
               (Vector.toList fc2Offsets)
               (Vector.toList fc2Modifiers)
      s = Frame (EntityID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs


-- | Create a frame
createFrame :: MonadInIO m => Handle -> Word32 -> Word32 -> PixelFormat -> FrameFlags -> [FrameBuffer] -> Excepts '[ErrorCode] m Frame
createFrame hdl width height fmt flags buffers = do
   
   let s = Frame (EntityID 0) width height
               fmt flags buffers

   ioctlAddFrame (fromFrame s) hdl
      ||> toFrame

-- | Release a frame
freeFrame :: MonadInIO m => Handle -> Frame -> Excepts '[ErrorCode] m ()
freeFrame hdl fs = do
   void (ioctlRemoveFrame (unEntityID (frameID fs)) hdl)


-- | Indicate dirty parts of a frame source
dirtyFrame :: MonadInIO m => Handle -> Frame -> DirtyAnnotation -> Excepts '[ErrorCode] m ()
dirtyFrame hdl fs mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)

   void $ withArray clips $ \clipPtr -> do
      let s = StructFrameDirty
               { fdFbId     = unEntityID (frameID fs)
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      ioctlDirtyFrame s hdl


