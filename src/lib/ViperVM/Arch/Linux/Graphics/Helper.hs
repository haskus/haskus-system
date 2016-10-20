module ViperVM.Arch.Linux.Graphics.Helper
   ( FBConfig (..)
   , setController
   , switchFrameBuffer
   )
where

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.ErrorCode

data FBConfig
   = SetFB FrameBuffer
   | ReuseFB
   | ReleaseFB
   deriving (Show)

-- | Configure a controller
--
-- A connected framebuffer is required to set a mode: if ReuseFB is passed, the
-- connected one is used.
setController :: Controller -> FBConfig -> [Connector] -> Maybe Mode -> IOErr ()
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
switchFrameBuffer :: Controller -> FrameBuffer -> PageFlipFlags -> IOErr ()
switchFrameBuffer ctrl fb flags =
   switchFrameBuffer' (controllerHandle ctrl) (controllerID ctrl) (fbID fb) flags
