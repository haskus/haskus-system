{-# LANGUAGE DataKinds #-}

-- | Helpers for the graphics API
module Haskus.System.Linux.Graphics.Helper
   ( FrameSourceAction (..)
   , setController
   , switchFrameSource
   )
where

import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.FrameSource
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.ErrorCode
import Haskus.Format.Binary.Word
import Haskus.Utils.Flow

-- | How to configure frame source with setController
data FrameSourceAction
   = SetSource FrameSource -- ^ Use this given source
   | ReuseSource           -- ^ Use the already set one
   | ReleaseSource         -- ^ Release the set source
   deriving (Show)

-- | Configure a controller
--
-- A connected frame source is required to set a mode: if ReuseSource is passed, the
-- connected one is used.
setController :: MonadInIO m => Controller -> FrameSourceAction -> [Connector] -> Maybe Mode -> Flow '[ErrorCode] m ()
setController ctrl frameSourceAction conns mode = do
   let 
      mframe = case frameSourceAction of
         SetSource fs  -> Just $ Frame (frameID fs) 0 0
         ReuseSource   -> Just $ Frame (EntityID maxBound) 0 0
         ReleaseSource -> Nothing
      hdl  = controllerHandle ctrl
   setController' hdl (controllerID ctrl) mframe (fmap connectorID conns) mode

-- | Switch to another frame source for the given controller without doing a
-- full mode change
switchFrameSource :: MonadInIO m => Controller -> FrameSource -> PageFlipFlags -> Word64 -> Flow '[ErrorCode] m ()
switchFrameSource ctrl fs flags udata =
   switchFrameBuffer' (controllerHandle ctrl) (controllerID ctrl) (frameID fs) flags udata
