{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pixel source
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
import Haskus.System.Linux.Graphics.IDs
import Haskus.System.Linux.Internals.Graphics
import Haskus.Format.Binary.Vector as Vector
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Utils.Tuple
import Haskus.Utils.Flow
import Haskus.Utils.List (zip4)

-- | Pixel source
data PixelSource = PixelSource
   { surfaceHandle    :: Word32 -- ^ Handle of the surface
   , surfacePitch     :: Word32 -- ^ Pitch of the surface
   , surfaceOffset    :: Word32 -- ^ Offset of the surface
   , surfaceModifiers :: Word64 -- ^ Modifiers for the surface
   } deriving (Show)

-- | Abstract frame source
data FrameSource = FrameSource
   { frameID          :: FrameSourceID    -- ^ Frame buffer identifier
   , frameWidth       :: Word32           -- ^ Frame buffer width
   , frameHeight      :: Word32           -- ^ Frame buffer height
   , framePixelFormat :: PixelFormat      -- ^ Pixel format
   , frameFlags       :: FrameBufferFlags -- ^ Flags
   , frameSources     :: [PixelSource]    -- ^ Data sources (up to four)
   } deriving (Show)

fromFrameSource :: FrameSource -> StructFrameBufferCommand
fromFrameSource FrameSource{..} = s
   where
      FrameSourceID fbid = frameID
      g :: (Num a,Storable a) => (PixelSource -> a) -> Vector 4 a
      g f = Vector.fromFilledList 0 (fmap f frameSources)
      s   = StructFrameBufferCommand fbid
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
      s = FrameSource (FrameSourceID fc2FbId)
            fc2Width fc2Height fc2PixelFormat fc2Flags bufs


-- | Create a framebuffer
addFrameSource :: MonadIO m => Handle -> Word32 -> Word32 -> PixelFormat -> FrameBufferFlags -> [PixelSource] -> Flow m '[FrameSource,ErrorCode]
addFrameSource hdl width height fmt flags buffers = do
   
   let s = FrameSource (FrameSourceID 0) width height
               fmt flags buffers

   liftIO (ioctlAddFrameBuffer (fromFrameSource s) hdl)
      >.-.> toFrameSource

-- | Release a frame buffer
removeFrameSource :: MonadIO m => Handle -> FrameSource -> Flow m '[(),ErrorCode]
removeFrameSource hdl fs = do
   let FrameSourceID fbid = frameID fs
   liftIO (ioctlRemoveFrameBuffer fbid hdl)
      >.-.> const ()


-- | Indicate dirty parts of a frame source
dirtyFrameSource :: MonadInIO m => Handle -> FrameSource -> DirtyAnnotation -> Flow m '[(),ErrorCode]
dirtyFrameSource hdl fs mode = do
   let
      (color,flags,clips) = case mode of
         Dirty     cs   -> (0,0,cs)
         DirtyCopy cs   -> (0,1, concatMap (\(a,b) -> [a,b]) cs)
         DirtyFill c cs -> (c,2,cs)
      FrameSourceID fbid = frameID fs

   withArray clips $ \clipPtr -> do
      let s = StructFrameBufferDirty
               { fdFbId     = fbid
               , fdFlags    = flags
               , fdColor    = color
               , fdNumClips = fromIntegral (length clips)
               , fdClipsPtr = fromIntegral (ptrToWordPtr clipPtr)
               }
      liftIO (ioctlDirtyFrameBuffer s hdl) >.-.> const ()


