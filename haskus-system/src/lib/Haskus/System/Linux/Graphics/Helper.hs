{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Helpers for the graphics API
module Haskus.System.Linux.Graphics.Helper
   ( FrameAction (..)
   , setController
   , switchFrame
   )
where

import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Frame
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.ErrorCode
import Haskus.Format.Binary.Word
import Haskus.Utils.Flow

-- | How to configure frame source with setController
data FrameAction b
   = UseFrame (Frame b) -- ^ Use the given Frame
   | KeepCurrentFrame   -- ^ Use the current Frame (if any)
   | NoFrame            -- ^ Release the frame

-- | Configure a controller
--
-- A Frame is required to set a Mode. If KeepCurrentFrame is passed, the current
-- one is re-used (be sure it is large enough!)
setController :: MonadInIO m => Controller -> FrameAction b -> [Connector] -> Maybe Mode -> Excepts '[ErrorCode] m ()
setController ctrl frameSourceAction conns mode = do
   let 
      mframe = case frameSourceAction of
         UseFrame frame   -> Just $ FrameView (frameID frame) 0 0
         KeepCurrentFrame -> Just $ FrameView (EntityID maxBound) 0 0
         NoFrame          -> Nothing
      hdl  = controllerHandle ctrl
   setController' hdl (controllerID ctrl) mframe (fmap connectorID conns) mode

-- | Switch to another frame for the given controller without doing a full mode
-- change
switchFrame :: MonadInIO m => Controller -> Frame b -> SwitchFrameFlags -> Word64 -> Excepts '[ErrorCode] m ()
switchFrame ctrl fs flags udata =
   switchFrame' (controllerHandle ctrl) (controllerID ctrl) (frameID fs) flags udata
