module ViperVM.Arch.Linux.Graphics.Helper
   ( cardSetController
   , cardSwitchFrameBuffer
   )
where

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode

-- | Configure a controller
cardSetController :: Card -> Controller -> Maybe FrameBuffer -> [Connector] -> Maybe Mode -> SysRet ()
cardSetController card ctrl fb conns mode = do
   let fbpos = fmap (\z -> FrameBufferPos (fbID z) 0 0) fb
   setController' card (controllerID ctrl) fbpos (fmap connectorID conns) mode

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
cardSwitchFrameBuffer :: Card -> Controller -> FrameBuffer -> PageFlipFlags -> SysRet ()
cardSwitchFrameBuffer card ctrl fb flags =
   switchFrameBuffer' card (controllerID ctrl) (fbID fb) flags

