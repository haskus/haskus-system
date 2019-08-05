{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

-- | Frame
--
-- A frame is a picture in memory. Its pixel components may be scattered into
-- different frame buffers though.
module Haskus.System.Linux.Graphics.Frame
   ( Frame(..)
   , FrameBuffer (..)
   , handleCreateFrame
   , freeFrame
   , dirtyFrame
   , SwitchFrameFlag (..)
   , SwitchFrameFlags
   , DirtyAnnotation (..)
   , Clip (..)
   , forEachFrameLine
   , forEachFrameColumn
   , forEachFramePixel
   , frameBufferPixelOffset
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.KIO
import Haskus.Binary.Vector as Vector
import Haskus.Number.Word
import Foreign.Ptr
import Haskus.Binary.Storable
import Haskus.Utils.Tuple
import Haskus.Utils.Flow
import Haskus.Utils.List (zip5)


fromFrame :: forall b. Frame b -> StructFrameCommand2
fromFrame Frame{..} =
   StructFrameCommand2 (unEntityID frameID)
      frameWidth frameHeight framePixelFormat frameFlags
      (g fbBufferHandle) (g fbPitch)
      (g fbOffset) (g fbModifiers)
   where
      g :: (Num a,Storable a) => (FrameBuffer b -> a) -> Vector 4 a
      g f = Vector.fromFilledList 0 (fmap f frameBuffers)


toFrame :: Handle -> [FrameBuffer b] -> StructFrameCommand2 -> Frame b
toFrame hdl fbs StructFrameCommand2{..} = s
   where
      bufs = uncurry5 FrameBuffer <$> zip5
               (fmap fbBuffer fbs)
               (Vector.toList fc2Handles)
               (Vector.toList fc2Pitches)
               (Vector.toList fc2Offsets)
               (Vector.toList fc2Modifiers)
      s = Frame (EntityID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs hdl


-- | Create a frame
handleCreateFrame :: MonadInIO m => Handle -> Word32 -> Word32 -> PixelFormat -> FrameFlags -> [FrameBuffer b] -> Excepts '[ErrorCode] m (Frame b)
handleCreateFrame hdl width height fmt flags frameBuffers = do
   
   let s = Frame (EntityID 0) width height
               fmt flags frameBuffers hdl

   ioctlAddFrame (fromFrame s) hdl
      ||> toFrame hdl frameBuffers

-- | Release a frame
freeFrame :: MonadInIO m => Frame b -> Excepts '[ErrorCode] m ()
freeFrame frame = do
   void <| ioctlRemoveFrame
               (unEntityID (frameID frame))
               (frameCardHandle frame)


-- | Indicate dirty parts of a frame source
dirtyFrame :: MonadInIO m => Frame b -> DirtyAnnotation -> Excepts '[ErrorCode] m ()
dirtyFrame frame mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)

   void $ withArray clips $ \clipPtr -> do
      let s = StructFrameDirty
               { fdFbId     = unEntityID (frameID frame)
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      ioctlDirtyFrame s (frameCardHandle frame)

-- | Do something for each line of the frame (top to bottom)
forEachFrameLine :: Monad m => Frame b -> (Word32 -> m ()) -> m ()
{-# INLINABLE forEachFrameLine #-}
forEachFrameLine frame action = forLoopM_ 0 (< frameHeight frame) (+1) action


-- | Do something for each column of the frame (left to right)
forEachFrameColumn :: Monad m => Frame b -> (Word32 -> m ()) -> m ()
{-# INLINABLE forEachFrameColumn #-}
forEachFrameColumn frame action = forLoopM_ 0 (< frameWidth frame) (+1) action

-- | Do something for each pixel (x,y) of the frame
forEachFramePixel :: Monad m => Frame b -> (Word32 -> Word32 -> m ()) -> m ()
{-# INLINABLE forEachFramePixel #-}
forEachFramePixel frame action =
   forEachFrameLine frame \y ->
      forEachFrameColumn frame \x ->
         action x y

-- | Compute an offset in a FrameBuffer from pixel component size in bytes and
-- pixel coordinates (x,y)
frameBufferPixelOffset :: FrameBuffer b -> Word32 -> Word32 -> Word32 -> Word32
{-# INLINABLE frameBufferPixelOffset #-}
frameBufferPixelOffset fb pixelSize x y =
   fbOffset fb + x*pixelSize + y*(fbPitch fb)
