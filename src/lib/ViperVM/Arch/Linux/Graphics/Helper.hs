module ViperVM.Arch.Linux.Graphics.Helper
   ( cardSetController
   )
where

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

-- | Set Controller
setController :: IOCTL -> FileDescriptor -> Controller -> Maybe FrameBuffer -> [Connector] -> Maybe Mode -> SysRet ()
setController ioctl fd crtc fb conns mode =
   setController' ioctl fd (controllerID crtc) (fmap (FrameBufferID . fbID) fb) (fmap connectorID conns) mode

-- | Configure a controller
cardSetController :: Card -> Controller -> Maybe FrameBuffer -> [Connector] -> Maybe Mode -> SysRet ()
cardSetController card = withCard card setController 

