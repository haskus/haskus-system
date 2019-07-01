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
   = SetSource (Frame b) -- ^ Use this given source
   | ReuseSource         -- ^ Use the already set one
   | ReleaseSource       -- ^ Release the set source

-- | Configure a controller
--
-- A connected frame source is required to set a mode: if ReuseSource is passed, the
-- connected one is used.
setController :: MonadInIO m => Controller -> FrameAction b -> [Connector] -> Maybe Mode -> Excepts '[ErrorCode] m ()
setController ctrl frameSourceAction conns mode = do
   let 
      mframe = case frameSourceAction of
         SetSource fs  -> Just $ FrameView (frameID fs) 0 0
         ReuseSource   -> Just $ FrameView (EntityID maxBound) 0 0
         ReleaseSource -> Nothing
      hdl  = controllerHandle ctrl
   setController' hdl (controllerID ctrl) mframe (fmap connectorID conns) mode

-- | Switch to another frame for the given controller without doing a full mode
-- change
switchFrame :: MonadInIO m => Controller -> Frame b -> SwitchFrameFlags -> Word64 -> Excepts '[ErrorCode] m ()
switchFrame ctrl fs flags udata =
   switchFrame' (controllerHandle ctrl) (controllerID ctrl) (frameID fs) flags udata
