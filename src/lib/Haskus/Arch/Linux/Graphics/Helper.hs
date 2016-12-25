{-# LANGUAGE DataKinds #-}

-- | Helpers for the graphics API
module Haskus.Arch.Linux.Graphics.Helper
   ( FBConfig (..)
   , setController
   , switchFrameBuffer
   )
where

import Haskus.Arch.Linux.Graphics.State
import Haskus.Arch.Linux.Graphics.Mode
import Haskus.Arch.Linux.Graphics.FrameBuffer
import Haskus.Arch.Linux.ErrorCode
import Haskus.Utils.Flow

data FBConfig
   = SetFB FrameBuffer
   | ReuseFB
   | ReleaseFB
   deriving (Show)

-- | Configure a controller
--
-- A connected framebuffer is required to set a mode: if ReuseFB is passed, the
-- connected one is used.
setController :: MonadInIO m => Controller -> FBConfig -> [Connector] -> Maybe Mode -> Flow m '[(),ErrorCode]
setController ctrl fbconf conns mode = do
   let 
      fbpos = case fbconf of
         SetFB fb  -> Just $ FrameBufferPos (fbID fb) 0 0
         ReuseFB   -> Just $ FrameBufferPos (FrameBufferID (-1)) 0 0
         ReleaseFB -> Nothing
      hdl  = controllerHandle ctrl
   setController' hdl (controllerID ctrl) fbpos (fmap connectorID conns) mode

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
switchFrameBuffer :: MonadIO m => Controller -> FrameBuffer -> PageFlipFlags -> Flow m '[(),ErrorCode]
switchFrameBuffer ctrl fb flags =
   switchFrameBuffer' (controllerHandle ctrl) (controllerID ctrl) (fbID fb) flags
